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
-- import           Data.Aeson.QQ.Simple (aesonQQ)
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
import Data.Scientific (Scientific, fromFloatDigits)

type Env = HM.HashMap String Aeson.Value

toExpr :: Env -> [Token] -> IO Expr
toExpr _env [TIdentifier thunk, TParenOpn, TParenCls] = do
    tdy <- BEL.ioToday
    yr <- BEL.ioYear
    dom <- BEL.ioDayOfMonth
    pure $ case thunk of
        "today" -> Data $ Aeson.String (Text.pack tdy)
        "year" -> Data $ Aeson.String (Text.pack yr)
        "dayOfMonth" -> Data $ Aeson.String (Text.pack dom)
        -- "loremIpsum 5" -> Data $ Aeson.String $ Text.pack $ BEL.loremChars 5
        _ -> Data $ Aeson.Null

toExpr env els = pure $ asExpr env els

-- ??: document if warnings for fallback values are desirable
asExpr :: Env -> [Token] -> Expr
asExpr env [TIdentifier t] =
    Data $ case HM.lookup (Text.unpack t) env of
        Just v -> v
        _ -> Aeson.String t
asExpr _ [TBool v] = Data $ Aeson.Bool v

asExpr env [TNum v1, TMult, TIdentifier t] = 
    let numOr1 = case HM.lookup (Text.unpack t) env of
            Just (Aeson.Number n) -> n
            _ -> 1 in
    Num (numOr1 * fromFloatDigits v1)

asExpr env [TNum v1, TPlus, TIdentifier t] = 
    let numOr0 = case HM.lookup (Text.unpack t) env of
            Just (Aeson.Number n) -> n
            _ -> 0 in
    Num (numOr0 + fromFloatDigits v1)

asExpr env [l@(TIdentifier _), TPlus, r@(TNum _)] = asExpr env [r, TPlus, l]

asExpr _ [TNum v1, TPlus, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 + v2)
asExpr _ [TNum v1, TMult, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 * v2)

-- Eq (Data $ Aeson.String v1) (Data $ Aeson.String v2)

asExpr _ [TQuoted v1,     TEq, TQuoted v2] =     Data $ Aeson.Bool (v1 == v2)
asExpr env [TQuoted v1,     TEq, TIdentifier v2] = asExpr env [TIdentifier v2, TEq, TQuoted v1]
asExpr env [TIdentifier ti, TEq, TQuoted tq] =
    case HM.lookup (Text.unpack ti) env of
        Nothing -> Data $ Aeson.Bool False
        Just v -> Data $ Aeson.Bool (v == Aeson.String tq)
asExpr env [lhs@(TIdentifier _), TEq, rhs@(TIdentifier _)] =
    Data $ Aeson.Bool (unrefEqual env (lhs, rhs))

asExpr _ [TNum v1, TEq, TNum v2] = Eq (Data $ Aeson.Number (fromFloatDigits v1)) (Data $ Aeson.Number (fromFloatDigits v2))
asExpr _ [TNum v1, TNeq, TNum v2] = Data $ Aeson.Bool (v1 /= v2)
asExpr _ [TJsonpath, TQuoted t] = App (Fn "jsonpath") (Data $ Aeson.String t)

-- -- The pratt parsing technique.
-- asExpr _env tokens = fst $ parseExpr tokens 0
--     where
--     parseExpr :: [Token] -> Int -> (Expr, [Token])
--     parseExpr toks minPrec = 
--       let (lhs, toks1) = parsePrimary toks
--       in parseExpr' lhs toks1 minPrec
--     
--     parseExpr' :: Expr -> [Token] -> Int -> (Expr, [Token])
--     parseExpr' lhs [] _ = (lhs, [])
--     parseExpr' lhs (op:rest) minPrec
--       | prec op >= minPrec =
--           let (rhs, rest') = parseExpr rest (prec op + 1)
--               lhs' = applyOp op lhs rhs
--           in parseExpr' lhs' rest' minPrec
--       | otherwise = (lhs, op:rest)
--     
--     parsePrimary :: [Token] -> (Expr, [Token])
--     parsePrimary (TNum n : rest) = (Data $ Aeson.Number (fromFloatDigits n), rest)
--     -- parsePrimary (TIdentifier x : rest) = (EVar x, rest)
--     -- ??
--     -- parsePrimary (TNum n : rest) = (ENum n, rest)
--     -- parsePrimary (TIdentifier x : rest) = (EVar x, rest)
--     parsePrimary _ = error "Expected primary expression"
--     
--     prec :: Token -> Int
--     prec TPlus = 1
--     prec TMinus = 1
--     prec TMult = 2
--     prec TDiv = 2
--     prec _ = 0
--     
--     applyOp :: Token -> Expr -> Expr -> Expr
--     applyOp TPlus  l r = Add l r
--     applyOp TMinus l r = Sub l r
--     applyOp TMult  l r = Mul l r
--     applyOp TDiv   l r = Div l r
--     applyOp _ _ _ = undefined


-- True iff both TIdentifier found and the values match.
unrefEqual :: Env -> (Token, Token) -> Bool
unrefEqual env ((TIdentifier varA), (TIdentifier varB)) =
    case (HM.lookup (Text.unpack varA) env,
          HM.lookup (Text.unpack varB) env) of
        (Just v1, Just v2) -> v1 == v2
        _ -> False
unrefEqual _ _ = False


-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

float :: Parser Double
float = do
    L.signed sc (try L.float <|> fmap fromIntegral (L.decimal :: Parser Integer))

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



-- First assume that Aeson.Value here should distinct Aeson.Number and Aeson.String because
-- `eval`ed text is stored to Env; might look through it for arith.
eval :: Env -> Text -> IO Aeson.Value
eval env input =
    case runParser exprP "" input of
        Left _ -> pure $ Aeson.String input
        Right (tokens :: [Token]) -> do
            e <- toExpr env tokens
            pure $ finalValue env (match env e)


finalValue :: Env -> Expr -> Aeson.Value

finalValue env (Data x@(Aeson.String k)) =
    case HM.lookup (Text.unpack k) env of
        Just v -> v
        Nothing -> x

finalValue _ (Data final) = final
finalValue _ (Num s) = Aeson.Number s
finalValue _ e = Aeson.String (Text.pack $ show e)

data Expr
  = Data Aeson.Value
  | Fn   String
  | Num  Scientific

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

    go final@(Data _) = final
    go final@(Num _) = final

    go (Neg (Data (Aeson.Bool b))) = Data (Aeson.Bool (not b))
    go (Neg e) = go (Neg (go e))

    go (Eq (Data v1) (Data v2)) = Data (Aeson.Bool (v1 == v2))
    go (Eq e1 e2) = go (Eq (go e1) (go e2))
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (App (Fn "ident") arg@(Data _)) = arg

    go (App (Fn "jsonpath") (Data (Aeson.String q))) =
        case HM.lookup "RESP_BODY" env of
            Nothing -> Data $ Aeson.String ""
            Just root -> case queryBody (Text.unpack q) root of
                Nothing -> Data $ Aeson.String ""
                Just one -> Data one

    go (App (Fn "today") _) = Data (Aeson.String "?? merge thunks")

    go (Add (Num v1) (Num v2)) = Num (v1 + v2)
    go (Add e1 e2) =       go (Add (go e1) (go e2))

    go (Mul (Num v1) (Num v2)) = Num (v1 * v2)
    go (Mul e1 e2) =       go (Mul (go e1) (go e2))

    go (Sub (Num v1) (Num v2)) = Num (v1 - v2)
    go (Sub e1 e2) = go (Sub (go e1) (go e2))

    go (Div (Num v1) (Num v2)) = Num (v1 / v2)
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
  -- | TNum Scientific  -- ??: pro [Aeson] con [parseFloat]
  | TPlus | TMinus | TMult | TDiv
  | TNum Double
  deriving (Show, Eq)


-- bool :: Parser [Token]
-- bool = trace "bool entry.." $ try $ do
--     b <- tokenFalse <|> tokenTrue
--     trace "bool exit.." $ pure [b]

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
  [ TNum <$> try float
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


-- render env (Aeson.String "paragraph so far") [Right "."]
-- eval env "."
-- impureRenderTemplate :: Env -> Text -> IO (Either String Text)
-- BEL internal error
-- hh internal error
-- best effort template
-- input left intact
--
-- {{today()}}
-- {{sentence with space}}
-- {{unclosed
-- hello {{name}}
-- hello {{firstname}} {{undefined}} !
-- today()

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
    evaled :: Aeson.Value <- eval env t

    let str = case evaled of
            Aeson.String txt -> show' txt
            Aeson.Object obj -> show obj
            Aeson.Number n -> show n  -- ??: present point zero as (show n)[:-2]
            _ -> ("unhandled render L" :: String)

    let ss :: Aeson.Value = Aeson.String $ Text.concat [acc, Text.pack str]
    render env ss rest

render _ _ _ = pure $ Aeson.String ""


identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure [TIdentifier (Text.pack xs)]
