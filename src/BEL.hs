{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module BEL
  ( Env
  , partitions, Part(..)
  , eval, render
  -- For testing:
  , toExpr, Token(..), Expr(..), match, finalValue, queryEnvRespBody
  ) where

import Debug.Trace

import           Control.Applicative (empty)
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Vector as Vec
import qualified Data.Aeson as Aeson (Value(..))
import qualified Data.Aeson.JSONPath as Aeson (query)
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some
                                 , anySingle
                                 , many, manyTill
                                 , try, runParser
                                 , takeWhile1P
                                 , choice
                                 )

import qualified BEL.BatteriesMain as BEL
import           BEL.Pratt


-- ??: parametrized fn invocation "loremIpsum 5" -> ... BEL.loremChars 5
toExpr :: Env -> [Token] -> IO Expr
-- toExpr _env [TIdentifier thunk, TParenOpn, TParenCls] =
toExpr _env (TIdentifier thunk : TParenOpn : TParenCls : []) = do
    case thunk of
        "today" -> do
            tdy <- BEL.ioToday
            pure $ VString (Text.pack tdy)
        "year" -> do
            yr <- BEL.ioYear
            pure $ VString (Text.pack yr)
        "dayOfMonth" -> do
            dom <- BEL.ioDayOfMonth
            pure $ VString (Text.pack dom)
        _ -> pure $ VString ""

toExpr env toks = do
    let (expr, _rest) = expression 0 (trace ("toks:" ++ show toks) $ toks)  -- ??: effectfulExpr :: 0 toks -> IO ...
    -- let (expr, _rest) = expression 0 toks  -- ??: effectfulExpr :: 0 toks -> IO ...
        -- res :: Expr = match env expr
        res :: Expr = match env (trace ("pratted:" ++ show expr)$ expr)
    case (trace ("matched:" ++ show res) $ res) of

        EPrint e -> do
            print (finalValue env e)
            pure (VBool True)

        _ -> pure res


-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

number :: Parser Scientific
number = do
    L.signed sc L.scientific

type Parser = Parsec Void Text



eval :: Env -> Text -> IO Expr
eval env input =
    -- case runParser exprP "" input of
    case runParser exprP "" (trace ("input:" ++ show input)$ input) of
        Left _ -> pure $ VString input
        Right (tokens :: [Token]) -> do
            e <- toExpr env tokens
            pure (match env e)


-- ??: git diff  auto VIdent  with goal of removing Env here
finalValue :: Env -> Expr -> Aeson.Value

finalValue _ (VString k) = Aeson.String k
finalValue _ (VBool s) = Aeson.Bool s
finalValue _ (VNum s) = Aeson.Number s
finalValue _ (VObj s) = Aeson.Object s
finalValue _ (VArray s) = Aeson.Array s
finalValue _ VNull = Aeson.Null
finalValue _ e = Aeson.String (Text.pack $ show e)


aesonToExpr :: Aeson.Value -> Expr
aesonToExpr (Aeson.Bool v)   = VBool v
aesonToExpr (Aeson.String v) = VString v
aesonToExpr (Aeson.Number v) = VNum v
aesonToExpr (Aeson.Object v) = VObj v
aesonToExpr (Aeson.Array v)  = VArray v
aesonToExpr Aeson.Null       = VNull


queryEnvRespBody :: Env -> Text -> Expr
queryEnvRespBody env q =
    case HM.lookup "RESP_BODY" env of
        Nothing -> VString ""
        Just root -> case queryBody (Text.unpack q) root of
            Nothing -> VString ""
            Just one -> aesonToExpr one


match :: Env -> Expr -> Expr
match env = go
    where
    go :: Expr -> Expr

    go final@(VNum _) = final

    go (VIdent t) =
        case HM.lookup (Text.unpack t) env of
            Just v -> aesonToExpr v
            Nothing -> VString t

    go (Neg (VBool b)) = VBool $ not b
    go (Neg e) = go (Neg (go e))

    go (Eq e1 e2) = VBool (go e1 == go e2)
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (Lte e1 e2) =
        case (go e1, go e2) of
            (VNum n1, VNum n2) -> VBool (n1 <= n2)
            _ -> VBool False

    go (Gte e1 e2) =
        case (go e1, go e2) of
            (VNum n1, VNum n2) -> VBool (n1 >= n2)
            _ -> VBool False

    -- `debug` is a special assertion line that always evaluates to true, main
    -- functionality being its side effect of printing to stdout.

    -- ??: pattern match when queryBody returns none, then lookup, else literal
    -- debug $.method
    go (App (Fn "debug") (VString q)) =
        EPrint (queryEnvRespBody env q)


    go (App (Fn "jsonpath") (VString q)) =
        queryEnvRespBody env q

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

invocDebug :: Parser [Token]
invocDebug = try $ do
    fn <- C.string "debug"
    sc
    tokens <- exprP
    pure (TIdentifier fn : tokens)

invocJsonpath :: Parser [Token]
invocJsonpath = try $ do
    _ <- C.string "jsonpath"
    sc
    [quoted] <- jsonpathArg
    pure [TJsonpath, quoted]

-- Expect one matching Value.
queryBody :: String -> Aeson.Value -> Maybe Aeson.Value
queryBody q root = case Aeson.query q root of
    Left _ -> Nothing
    Right v -> case Vec.uncons v of
        Nothing ->       Nothing
        Just (one, _) -> Just one

-- (auto)
newtype CookieString = CookieString Text
  deriving (Eq, Show)

mkCookieString :: Text -> CookieString
mkCookieString = CookieString

parseCookies :: CookieString -> [Text]
parseCookies (CookieString t) = Text.split (== ';') t


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
    propP
    <|> invocDebug
    <|> arithP
    <|> invocationP
    <|> invocJsonpath
    <|> jsonpathArg
    <|> word


partsP :: Parser [Part]
partsP = many $ choice
    [ try needsEvalP
    , try escapedBraceP
    , literalP
    , singleBraceP
    , backslashP
    ]

-- The R parser.
literalP :: Parser Part
literalP = do
    t <- takeWhile1P Nothing (\c -> c /= '{' && c /= '\\')
    pure (R t)

escapedBraceP :: Parser Part
escapedBraceP = do
    _ <- C.string "\\{"
    pure (R "{")

singleBraceP :: Parser Part
singleBraceP = do
    _ <- C.char '{'
    pure (R "{")

backslashP :: Parser Part
backslashP = do
    _ <- C.char '\\'
    pure (R "\\")

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
    evaled :: Expr <- eval env t

    -- render necessitates for effective final values to be string.
    let str = case finalValue env evaled of
            Aeson.String txt -> show' txt
            Aeson.Object obj -> show obj
            Aeson.Number n -> case floatingOrInteger n of
                                  Right (i :: Integer) -> show i
                                  Left (_ :: Double)   -> show n
            _ -> ("unhandled render L" :: String)

    let av = Aeson.String $ Text.concat [acc, Text.pack str]
    render env av rest

-- render _ _ _ = pure $ Aeson.String ""
render _ _ _ = undefined


identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure [TIdentifier (Text.pack xs)]

--------------------------------------------------------------------------------
-- More lib than app code
--------------------------------------------------------------------------------
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
