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


import           System.Random (randomR, mkStdGen)
import Data.Time (Day, fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
-- import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock
-- import Text.Regex.Posix ((=~))


-- import           Data.Either
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
import           Data.Aeson.QQ.Simple (aesonQQ)
-- import Data.Char (isAlphaNum)
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some
                                 -- , anySingle, choice, getInput, manyTill, lookAhead
                                 , many
                                 , try, eof, runParser
                                 , takeWhile1P, ParseErrorBundle
                                 , parse
                                 , oneOf
                                 , choice
                                 )

import Control.Applicative (empty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific (Scientific, fromFloatDigits)

_unusedBatteries :: IO ()
_unusedBatteries = do
    let _ = (isoDate, exampleDay, pureRandomInt)
    pure ()

_allowUnused :: IO ()
_allowUnused = do
    let _ = litP
    let _ = rawVarP
    let _ = varP
    let _ = lineP
    let _ = templateP
    let _ = asText
    let _ = (isPredicate, trimQuotesText, parseFloat)
    pure ()

-- Battery {

pureRandomInt :: Int -> Int -> Int -> Int
pureRandomInt seed minVal maxVal =
    let gen = mkStdGen seed
        (val, _) = randomR (minVal, maxVal) gen
    in val

formatISODate :: Day -> String
formatISODate day = formatTime defaultTimeLocale "%Y-%m-%d" day

exampleDay :: Day
exampleDay = fromGregorian 2025 9 18

isoDate :: String
isoDate = formatISODate exampleDay


ioToday :: IO String
ioToday = do
    t <- getCurrentTime
    -- pure $ formatISODate exampleDay
    pure $ formatISODate (utctDay t)


-- }

toExpr :: Env -> [Token] -> IO Expr
-- toExpr env [TIdentifier t] = trace "toExpr A" $ pure $ -- Not really an IO; just so asExpr don't require Env.
--     Data $ case HM.lookup (Text.unpack t) env of
--         Just v -> v
--         _ -> Aeson.String t
-- toExpr env [TQuoted v1, TEq, TQuoted v2] = do Data $ Aeson.Bool (v1 == v2)
toExpr _ [TIdentifier thunk, TParenOpn, TParenCls] = do
    tdy <- ioToday
    trace "toExpr B" $ pure $ case thunk of
        "today" -> Data $ Aeson.String (Text.pack tdy)
        "year" -> Data $ Aeson.String "2025"
        "dayOfMonth" -> Data $ Aeson.String "4"
        "loremIpsum" -> Data $ Aeson.String "lorem ipsum sit"  -- ??: 255 chars of lorem ipsum text
        _ -> Data $ Aeson.Null
toExpr env els = trace "toExpr C" $ pure $ asExpr env els


asExpr :: Env -> [Token] -> Expr
asExpr env [TIdentifier t] =
    Data $ case HM.lookup (Text.unpack t) env of
        Just v -> v
        _ -> Aeson.String t
asExpr _ [TBool v] = Data $ Aeson.Bool v
-- asExpr [TNum v] = Data $ Aeson.Number v

asExpr env [TNum v1, TPlus, TIdentifier t] = 
    let numOr0 = case HM.lookup (Text.unpack t) env of
            Just (Aeson.Number n) -> n
            _ -> 0 in
    Num (numOr0 + fromFloatDigits v1)
-- ??: for mul it's numOr1
asExpr env [l@(TIdentifier _), TPlus, r@(TNum _)] = asExpr env [r, TPlus, l]

asExpr _ [TNum v1, TPlus, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 + v2)
asExpr _ [TNum v1, TMult, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 * v2)

-- Eq (Data $ Aeson.String v1) (Data $ Aeson.String v2)

asExpr env [TQuoted v1,     TEq, TQuoted v2] =     Data $ Aeson.Bool (v1 == v2)
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

-- The pratt parsing technique.
asExpr env tokens = fst $ parseExpr tokens 0
    where
    parseExpr :: [Token] -> Int -> (Expr, [Token])
    parseExpr toks minPrec = 
      let (lhs, toks1) = parsePrimary toks
      in parseExpr' lhs toks1 minPrec
    
    parseExpr' :: Expr -> [Token] -> Int -> (Expr, [Token])
    parseExpr' lhs [] _ = (lhs, [])
    parseExpr' lhs (op:rest) minPrec
      | prec op >= minPrec =
          let (rhs, rest') = parseExpr rest (prec op + 1)
              lhs' = applyOp op lhs rhs
          in parseExpr' lhs' rest' minPrec
      | otherwise = (lhs, op:rest)
    
    parsePrimary :: [Token] -> (Expr, [Token])
    parsePrimary (TNum n : rest) = (Data $ Aeson.Number (fromFloatDigits n), rest)
    -- parsePrimary (TIdentifier x : rest) = (EVar x, rest)
    -- ??
    -- parsePrimary (TNum n : rest) = (ENum n, rest)
    -- parsePrimary (TIdentifier x : rest) = (EVar x, rest)
    parsePrimary _ = error "Expected primary expression"
    
    prec :: Token -> Int
    prec TPlus = 1
    prec TMinus = 1
    prec TMult = 2
    prec TDiv = 2
    prec _ = 0
    
    applyOp :: Token -> Expr -> Expr -> Expr
    applyOp TPlus  l r = Add l r
    applyOp TMinus l r = Sub l r
    applyOp TMult  l r = Mul l r
    applyOp TDiv   l r = Div l r


-- True iff both TIdentifier found and the values match.
unrefEqual :: Env -> (Token, Token) -> Bool
unrefEqual env ((TIdentifier varA), (TIdentifier varB)) =
    case (HM.lookup (Text.unpack varA) env,
          HM.lookup (Text.unpack varB) env) of
        (Just v1, Just v2) -> v1 == v2
        _ -> False


-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

-- ??: suppress fromIntegral type-defaults warning
float :: Parser Double
float = L.signed sc (try L.float <|> fmap fromIntegral L.decimal)

parseFloat :: Text -> Either (ParseErrorBundle Text Void) Double
parseFloat = parse (float <* eof) "<input>"


-- ??: can we avoid duplication of Env type defs using typeclasses sig or whatnot
-- type Env = HM.HashMap Text Aeson.Value
type Env = HM.HashMap String Aeson.Value
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

trimQuotesText :: Text -> Text
trimQuotesText t =
  case Text.uncons t of
    Just ('"', rest) ->
      case Text.unsnoc rest of
        Just (mid, '"') -> mid
        _               -> t
    _ -> t



isPredicate :: Expr -> Bool
isPredicate e =
    case e of
        Data (Aeson.Bool _) -> True
        _ -> False

-- First assume that Aeson.Value here should distinct Aeson.Number and Aeson.String because
-- `eval`ed text is stored to Env; might look through it for arith.
eval :: Env -> Text -> IO Aeson.Value
eval env input = do
    trace ("exprP input:\t" ++ show input) $ case runParser exprP "" input of
        -- Left _ -> pure $ Aeson.String input
        Left _ -> trace "you're dead" $ pure $ Aeson.String input
        Right (tokens :: [Token]) -> do
            e <- toExpr env (trace ("tokens:\t" ++ show tokens) tokens)
            pure $ (trace ("match input:\t" ++ show e) $ finalValue env (match env e))


finalValue :: Env -> Expr -> Aeson.Value

finalValue env (Data x@(Aeson.String k)) =
    trace ("matched:\t" ++ show x ++ "\tfinalValue!") (case HM.lookup (Text.unpack k) env of
        Just v -> v
        Nothing -> x)

finalValue _ (Data final) = trace "eval:final not a string" final
finalValue _ (Num s) = Aeson.Number s
finalValue _ e = trace "finalValue els" $ Aeson.String (Text.pack $ show e)

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
match env = trace ("match env:\t" ++ show env) go  -- ??: find necessity for runExcept
    where
    go :: Expr -> Expr

    go final@(Data _) = final
    go final@(Num _) = final  -- ??: as Aeson.Number always an option

    go (Neg (Data (Aeson.Bool b))) = Data (Aeson.Bool (not b))
    go (Neg e) = go (Neg (go e))

    go (Eq (Data v1) (Data v2)) = Data (Aeson.Bool (v1 == v2))
    go (Eq e1 e2) = go (Eq (go e1) (go e2))
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (App (Fn "ident") arg@(Data _)) = arg

    go (App (Fn "jsonpath") (Data (Aeson.String q))) =  -- ??: designate token for query arg (string; trimmed spaces)
        let Just root :: Maybe Aeson.Value = (HM.lookup "RESP_BODY" env) in
        case queryBody (Text.unpack q) root of
            Nothing -> Data $ Aeson.String ""
            Just one -> Data one

    -- -- Empty string if q not found in RESP_BODY.
    -- go (App (Fn "jsonpath") (Data (Aeson.String q))) =
    --     let nest :: Aeson.Value = [aesonQQ| { "data": { "token": "abcdefghi9" } } |] in
    --     -- ??: defaulting to nest is when mock doesn't provide valid json response
    --     -- let root :: Aeson.Value = (HM.lookupDefault (Aeson.String "hm") "RESP_BODY" env) in
    --     let root :: Aeson.Value = (HM.lookupDefault nest "RESP_BODY" env) in
    --     trace ("go:jsonpath\t" ++ (Text.unpack q) ++ "\nroot:\t" ++ show root) $ case queryBody (Text.unpack q) root of
    --         Nothing -> trace ("go:jsonpath:Nothing") (Data $ Aeson.String "")  -- ??: maybe due to space-prefixed q string
    --         Just one -> Data one

    go (App (Fn "today") _) = trace "go today:" (Data (Aeson.String "??"))

    -- go (Add (Data (Aeson.Number v1)) (Data (Aeson.Number v2))) = Data $ Aeson.Number (v1 + v2)
    -- go (Sub (Data (Aeson.Number v1)) (Data (Aeson.Number v2))) = Data $ Aeson.Number (v1 - v2)
    -- go (Mul (Data (Aeson.Number v1)) (Data (Aeson.Number v2))) = Data $ Aeson.Number (v1 * v2)
    -- go (Div (Data (Aeson.Number v1)) (Data (Aeson.Number v2))) = Data $ Aeson.Number (v1 / v2)

-- go (Mul (Data (Aeson.Number v1)) (Data (Aeson.Number v2))) = Num (v1 * v2)

    go (Add (Num v1) (Num v2)) = Num (v1 + v2)
    -- go (Add (Num v1) e2) = go (Add (Num v1) (go e2))
    -- go (Add e1 (Num v2)) = go (Add (Num v2) e1)
    go (Add e1 e2) =       go (Add (go e1) (go e2))

    go (Mul (Num v1) (Num v2)) = Num (v1 * v2)
    -- go (Mul (Num v1) e2) = go (Mul (Num v1) (go e2))
    -- go (Mul e1 (Num v2)) = go (Mul (Num v2) e1)
    go (Mul e1 e2) =       go (Mul (go e1) (go e2))

    go (Sub (Num v1) (Num v2)) = Num (v1 - v2)
    go (Sub e1 e2) = go (Sub (go e1) (go e2))

    go (Div (Num v1) (Num v2)) = Num (v1 / v2)
    go (Div e1 e2) = go (Div (go e1) (go e2))


    go _ = undefined

    -- -- go (Neg e) = negate <$> go e

    -- L.x
    -- | JSONPATH    AlexPosn String
    -- \$ $printable+            { tok (\p s -> JSONPATH p s) }
    --
    -- syn.hhs
    -- [Captures]
    -- TOKEN: jsonpath "$.data.token"
    -- [Asserts]
    -- jsonpath "$.data.name" == "alice"

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

-- Expect one matching Value or ??log.
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

tokenTrue :: Parser Token
tokenTrue = try $ do
    _ <- C.string "true"
    pure $ TBool True

tokenFalse :: Parser Token
tokenFalse = try $ do
    _ <- C.string "false"
    pure $ TBool False

-- bool :: Parser [Token]
-- bool = trace "bool entry.." $ try $ do
--     b <- tokenFalse <|> tokenTrue
--     trace "bool exit.." $ pure [b]

boolP :: Parser Token
boolP = choice
  [ (TBool False) <$ C.string "false"
  , (TBool True)  <$ C.string "true"
  ]


tokenEq :: Parser Token
tokenEq = try $ do
    sc
    _ <- C.string "=="
    sc
    pure TEq

opP' :: Parser Token
opP' = try $ do
    sc
    _ <- C.string "*"
    sc
    pure TMult

-- prop expressions simplify to a bool.

oldP :: Parser [Token]
-- oldP = try $ do
oldP = do
    lhs :: Double <- float
    sc
    rel <- relP
    sc
    rhs :: Double <- float
    pure [TNum lhs, rel, TNum rhs]

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

-- arithP1 :: Parser [Token]
-- arithP1 = try $ do
--     num1 :: Double <- float
--     sc
--     op <- operatorP
--     sc
--     num2 :: Double <- float
--     pure [TNum num1, op, TNum num2]

-- pratt or postfix whichever is cheaper/free. testing iface
-- -- ?? integrate this
-- import Text.Megaparsec.Expr
--
-- exprP :: Parser Expr
-- exprP = makeExprParser termP operatorTable
--   where
--     termP = choice
--       [ ENum <$> float
--       , EVar <$> identifier
--       , parens exprP
--       ]
--     
--     operatorTable = 
--       [ [ binary "*" EMul, binary "/" EDiv ]
--       , [ binary "+" EAdd, binary "-" ESub ]
--       ]
--     
--     binary name f = InfixL (f <$ symbol name)
-- arithExprP :: Parser Expr



-- arith expressions simplify to a number.
arithP :: Parser [Token]
arithP = trace "arithP entry.." $ try $ do
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
invocationP = trace "invocationP entry.." $ try $ do
    fn :: Text <- identifier
    opn <- TParenOpn <$ C.char '('
    cls <- TParenCls <$ C.char ')'
    pure [TIdentifier fn, opn, cls]

exprP :: Parser [Token]
exprP = try $ do
    -- sc
    trace "exprP propP"         propP
    <|> trace "exprP arithP"        arithP
    <|> trace "exprP invocationP"   invocationP
    -- <|> trace "exprP numEqNum"      numEqNum
    <|> trace "exprP invocJsonpath" invocJsonpath 
    <|> trace "exprP jsonpathArg"   jsonpathArg
    <|> trace "exprP word"          word

templateP :: Parser [Segment]
templateP = do
    segs <- many (try litP <|> varP)
    eof
    pure segs


partsP :: Parser [Part]
partsP = do
    -- parts :: [Part] <- many ((try needsEvalP) <|> literalP)
    many ((try needsEvalP) <|> literalP)

-- The L parser.
needsEvalP :: Parser Part
needsEvalP = try $ do
    m <- mustachedP
    pure (trace "am calledd" m)
    -- _ <- C.string "{{"
    -- sc
    -- w <- textP
    -- sc
    -- _ <- C.string "}}"
    -- pure (L w)

mustachedP :: Parser Part
mustachedP = try $ do
    _ <- C.string "{{"
    sc
    w <- textP
    sc
    _ <- C.string "}}"
    pure (L w)

-- The R parser.
literalP :: Parser Part
literalP = try $ do
    t <- takeWhile1P Nothing (/= '{')
    pure (R t)

asText :: Aeson.Value -> Text
asText x = decodeUtf8 (ByteString.toStrict (Aeson.encode x))

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
    case runParser partsP "??: unused?" input of
        Left _ -> trace "partitions 0" [R input]
        Right [] -> trace "partitions A" [R input]
        Right parts -> trace "partitions B" parts

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
            _ -> ("unhandled render L" :: String)

    render env (Aeson.String $ Text.concat [acc, Text.pack str]) rest

render _ _ _ = undefined


identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    trace ("word xs:" ++ xs) $ pure [TIdentifier (Text.pack xs)]

textP :: Parser Text
textP = do
    Text.pack <$> some (C.alphaNumChar <|> oneOf ("_(). \"\"$" :: String))

wordP :: Parser Text
wordP = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure (Text.pack xs)


-- P :: Parser Segment
-- P = try $ do
--   _ <- C.string "{{"
--   -- allow optional spaces inside
--   _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
--   name <- wordP
--   _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
--   _ <- C.string "}}"
--   pure (Var name)

lineP :: Parser Segment
lineP = try $ do
  _ <- many (C.char ' ' <|> C.char '\t')
  name <- wordP  -- ??
  _ <- many (C.char ' ' <|> C.char '\t')
  _ <- C.char '\n'
  pure (Var name)


-- {{...}} variable (escaped)
varP :: Parser Segment
varP = try $ do
  _ <- C.string "{{"
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n') -- allow optional spaces inside
  name <- wordP
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  _ <- C.string "}}"
  pure (Var name)

-- {{{...}}} raw variable (not escaped)
rawVarP :: Parser Segment
rawVarP = try $ do
  _ <- C.string "{{{"
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  name <- wordP
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  _ <- C.string "}}}"
  pure (RawVar name)


litP :: Parser Segment
litP = Lit <$> takeWhile1P Nothing (/= '{')
