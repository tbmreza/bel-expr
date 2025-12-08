{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module BEL
  ( Env
  , partitions, Part(..)
  , eval, render
  -- For testing:
  , toExpr, Token(..), Expr(..), match, finalValue
  ) where

import Debug.Trace

import qualified BEL.BatteriesMain as BEL

import qualified Data.ByteString.Lazy as ByteString (toStrict)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Void (Void)
import qualified Data.Vector as Vec
import           Data.Aeson as Aeson (encode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.Aeson.JSONPath as Aeson (query)
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some
                                 , anySingle
                                 , many, manyTill
                                 , try, runParser
                                 , takeWhile1P
                                 , choice
                                 )

import Control.Applicative (empty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific (Scientific)

type Env = HM.HashMap String Aeson.Value

toExpr :: Env -> [Token] -> IO Expr
toExpr _env [TIdentifier thunk, TParenOpn, TParenCls] = do
    tdy <- BEL.ioToday
    yr <- BEL.ioYear
    dom <- BEL.ioDayOfMonth
    pure $ case thunk of
        "today" -> VString (Text.pack tdy)
        "year" -> VString (Text.pack yr)
        "dayOfMonth" -> VString (Text.pack dom)
        -- "loremIpsum 5" -> Data $ Aeson.String $ Text.pack $ BEL.loremChars 5
        _ -> VString ""

toExpr env els = 
    let (expr, _) = expression env 0 els
    in pure (match env expr)

-- Pratt Parser Implementation

bp :: Token -> Int
bp TEq = 5
bp TNeq = 5
bp TLte = 5
bp TGte = 5
bp TPlus = 10
bp TMinus = 10
bp TMult = 20
bp TDiv = 20
bp _ = 0



nud :: Env -> Token -> [Token] -> (Expr, [Token])

nud _ (TNum n) rest = (VNum n, rest)

nud _ (TBool b) rest = (VBool b, rest)

nud _ (TQuoted s) rest = (VString s, rest)

nud env (TIdentifier t) rest =
    case HM.lookup (Text.unpack t) env of
        Just (Aeson.Bool v) -> (VBool v, rest)
        Just (Aeson.String v) -> (VString v, rest)
        Just (Aeson.Number v) -> (VNum v, rest)
        Just av -> (trace (show av) (VString "else"), rest)
        Nothing -> (VString t, rest)

nud env TJsonpath (TQuoted t : rest) = (App (Fn "jsonpath") (VString t), rest)

nud env TParenOpn rest =
    let (e, rest') = expression env 0 rest
    in case rest' of
        (TParenCls:rest'') -> (e, rest'')
        _ -> (e, rest')

nud _ t _ = (VString (Text.pack $ show [t]), [])



led :: Env -> Token -> Expr -> [Token] -> (Expr, [Token])

led env TPlus left rest =
    let (right, rest') = expression env 10 rest
    in (Add left right, rest')

led env TMinus left rest =
    let (right, rest') = expression env 10 rest
    in (Sub left right, rest')

led env TMult left rest =
    let (right, rest') = expression env 20 rest
    in (Mul left right, rest')

led env TDiv left rest =
    let (right, rest') = expression env 20 rest
    in (Div left right, rest')

led env TEq left rest =
    let (right, rest') = expression env 5 rest
    in (Eq left right, rest')

led env TNeq left rest =
    let (right, rest') = expression env 5 rest
    in (Neq left right, rest')

led _ t left rest = (left, t:rest)

expression :: Env -> Int -> [Token] -> (Expr, [Token])
expression env rbp tokens =
    case tokens of
        [] -> (VString "", [])
        (t:rest) ->
            let (left, rest') = nud env t rest
            in loop env rbp left rest'

loop :: Env -> Int -> Expr -> [Token] -> (Expr, [Token])
loop env rbp left tokens =
    case tokens of
        [] -> (left, [])
        (t:rest) ->
            if bp t > rbp
            then
                let (newLeft, newRest) = led env t left rest
                in loop env rbp newLeft newRest
            else (left, tokens)


-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

number :: Parser Scientific
number = do
    L.signed sc L.scientific

type Parser = Parsec Void Text

-- `show (t :: Text)` does introduce double quote on both ends.
show' :: Text -> String
show' t = trimQuotes $ show t
    where
    trimQuotes :: String -> String
    trimQuotes s =
      case s of
        ('"':xs) -> case reverse xs of
                      ('"':ys) -> reverse ys
                      _        -> s
        _        -> s



eval :: Env -> Text -> IO Expr
eval env input =
    case runParser exprP "" input of
        Left _ -> pure $ VString input
        Right (tokens :: [Token]) -> do
            e <- toExpr env tokens
            pure (match env e)


finalValue :: Env -> Expr -> Aeson.Value

finalValue env (VString k) =
    case HM.lookup (Text.unpack k) env of
        Just v -> v
        Nothing -> Aeson.String k

finalValue _ (VBool s) = Aeson.Bool s
finalValue _ (VNum s) = Aeson.Number s
finalValue _ e = Aeson.String (Text.pack $ show e)

data Expr =
    VBool !Bool
  -- | VObj  -- https://hackage.haskell.org/package/aeson-2.2.3.0/docs/Data-Aeson.html#t:Value
  | VString !Text
  | VNum  !Scientific

  | Fn   String

  | Neg Expr
  | Eq  Expr Expr
  | Neq Expr Expr

  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

  | App Expr Expr
  deriving (Show, Eq)

-- headers: header, cookie
-- body: jsonpath, bytes

match :: Env -> Expr -> Expr
match env = go
    where
    go :: Expr -> Expr

    go final@(VNum _) = final

    go (Neg (VBool b)) = VBool $ not b
    go (Neg e) = go (Neg (go e))

    go (Eq v1 v2) = VBool (v1 == v2)
    go (Eq e1 e2) = go (Eq (go e1) (go e2))
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (App (Fn "jsonpath") (VString q)) =
        case HM.lookup "RESP_BODY" env of
            Nothing -> VString ""
            Just root -> case queryBody (Text.unpack q) root of
                Nothing -> VString ""
                Just (one :: Aeson.Value) -> case one of
                    Aeson.Bool v -> VBool v
                    Aeson.String v -> VString v
                    Aeson.Number v -> VNum v

    go (Add (VNum v1) (VNum v2)) = VNum (v1 + v2)
    go (Add e1 e2) =       go (Add (go e1) (go e2))

    go (Mul (VNum v1) (VNum v2)) = VNum (v1 * v2)
    go (Mul e1 e2) =       go (Mul (go e1) (go e2))

    go (Sub (VNum v1) (VNum v2)) = VNum (v1 - v2)
    go (Sub e1 e2) = go (Sub (go e1) (go e2))

    go (Div (VNum v1) (VNum v2)) = VNum (v1 / v2)
    go (Div e1 e2) = go (Div (go e1) (go e2))

    go e = e

jsonpathArg :: Parser [Token]
jsonpathArg = try $ do
    _ <- C.char '"'
    t <- takeWhile1P Nothing (/= '"')
    _ <- C.char '"'
    pure [TQuoted t]

invocJsonpath :: Parser [Token]
invocJsonpath = try $ do
    _ <- C.string "jsonpath"
    sc
    [quoted] <- jsonpathArg
    pure [TJsonpath, quoted]

-- Expect one matching Value.
queryBody :: String -> Aeson.Value -> Maybe Aeson.Value
queryBody q root =
    case Aeson.query q root of
        Left _ -> Nothing
        Right v -> case Vec.uncons v of
            Nothing -> Nothing
            Just (one, _) -> Just one

type NeedsEval = Text
data Part =
    L NeedsEval
  | R Text
  deriving (Show, Eq)

data Segment =
    Lit Text
  | Var Text
  | RawVar Text
  deriving (Show, Eq)

data Arg =
    Input Text
  | Literal Text
  deriving (Show, Eq)

data Token =
    TUnit
  | TBool Bool
  | TTrue | TFalse
  | TEq | TNeq | TLte | TGte
  | TJsonpath
  | TIdentifier Text
  | TQuoted Text
  | TParenOpn | TParenCls
  | TPlus | TMinus | TMult | TDiv
  | TNum Scientific
  deriving (Show, Eq)


boolP :: Parser Token
boolP = choice
  [ (TBool False) <$ C.string "false"
  , (TBool True)  <$ C.string "true"
  ]



propP :: Parser [Token]
propP =
    (:[]) <$> boolP
    <|> relExprP

relExprP :: Parser [Token]
relExprP = try $ do
    v1 <- valueP
    sc
    op <- relP
    sc
    v2 <- valueP
    pure [v1, op, v2]

relP :: Parser Token
relP = choice
  [ TNeq <$ C.string "!="
  , TEq  <$ C.string "=="
  , TLte <$ C.string "<="
  , TGte <$ C.string ">="
  ]

-- arith expressions simplify to a number.
arithP :: Parser [Token]
arithP = try $ do
    sc
    first <- valueP
    rest <- many $ do
        sc
        op <- operatorP
        sc
        val <- valueP
        pure [op, val]
    pure $ first : concat rest

operatorP :: Parser Token
operatorP = choice
  [ TPlus  <$ C.char '+'
  , TMinus <$ C.char '-'
  , TMult  <$ C.char '*'
  , TDiv   <$ C.char '/'
  ]

valueP :: Parser Token
valueP = choice
  [ TNum <$> try number
  , TIdentifier <$> identifier
  ]


-- identifier (many arg)
-- today()
-- size(identifier)
-- invocation expressions simplify to a value.
invocationP :: Parser [Token]
invocationP = try $ do
    fn :: Text <- identifier
    opn <- TParenOpn <$ C.char '('
    cls <- TParenCls <$ C.char ')'
    pure [TIdentifier fn, opn, cls]

exprP :: Parser [Token]
exprP = try $ do
    -- sc
    propP
    <|> arithP
    <|> invocationP
    -- <|> numEqNum
    <|> invocJsonpath 
    <|> jsonpathArg
    <|> word


partsP :: Parser [Part]
partsP = do
    many ((try needsEvalP) <|> literalP)

-- The R parser.
literalP :: Parser Part
literalP = try $ do
    t <- takeWhile1P Nothing (/= '{')  -- ??: test user escape open curly
    pure (R t)

-- The L parser.
needsEvalP :: Parser Part
needsEvalP = try $ do
    _ <- C.string "{{"
    w <- manyTill anySingle (C.string "}}")
    pure (L $ Text.pack w)


partitions :: Text -> [Part]
partitions input =
    case runParser partsP "" input of
        Left _ -> [R input]
        Right [] -> [R input]
        Right parts -> parts

-- Argument either needs evaluation (Left) or already just "Right".
render :: Env -> Aeson.Value -> [Part] -> IO Aeson.Value
render _env accStr [] =
    pure accStr

render env (Aeson.String acc) ((R t):rest) =
    let grow :: Text = Text.concat [acc, t] in
    render env (Aeson.String grow) rest

render env (Aeson.String acc) ((L t):rest) = do
    -- evaled :: Aeson.Value <- eval env t
    evale  :: Expr <- eval env t
    let evaled = finalValue env evale

    -- render necessitates for effective final values to be string.
    let str = case evaled of
            Aeson.String txt -> show' txt
            Aeson.Object obj -> show obj
            Aeson.Number n -> show n  -- ??: present point zero as (show n)[:-2]
            _ -> ("unhandled render L" :: String)

    let ss :: Aeson.Value = Aeson.String $ Text.concat [acc, Text.pack str]
    render env ss rest

-- render _ _ _ = pure $ Aeson.String ""


identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure [TIdentifier (Text.pack xs)]
