{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module BEL
  -- ( render, eval
  -- ( renderTemplate
  -- , Env
  -- , litP, varP, templateP
  -- )
  where

import Debug.Trace


import           System.Random (randomR, mkStdGen)
import Data.Time (Day, fromGregorian)
import Data.Time.Format (formatTime, defaultTimeLocale)
-- import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock
import Text.Regex.Posix ((=~))


import           Data.Either
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
import Data.Char (isAlphaNum)
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some, anySingle, many, choice
                                 , manyTill, lookAhead, try, eof, runParser
                                 , getInput, takeWhile1P, ParseErrorBundle
                                 , parse
                                 , oneOf
                                 )

import Control.Applicative (empty)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Scientific (Scientific, fromFloatDigits)

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
toExpr env [TIdentifier t] = trace "toExpr A" $ pure $ -- Not really an IO; just so asExpr don't require Env.
    Data $ case HM.lookup (Text.unpack t) env of
        Just v -> v
        _ -> Aeson.String t
toExpr _ [TIdentifier thunk, TParenOpn, TParenCls] = do
-- [TIdentifier thunk, TUnit] =
    tdy <- ioToday
    trace "toExpr B" $ pure $ case thunk of
        "today" -> Data $ Aeson.String (Text.pack tdy)
        "year" -> Data $ Aeson.String "2025"
        "dayOfMonth" -> Data $ Aeson.String "4"
        "loremIpsum" -> Data $ Aeson.String "lorem ipsum sit"  -- ??: 255 chars of lorem ipsum text
        _ -> Data $ Aeson.Null
toExpr env els = trace "toExpr C" $ pure $ asExpr els


asExpr :: [Token] -> Expr
asExpr [TBool v] = Data $ Aeson.Bool v
-- asExpr [TNum v] = Data $ Aeson.Number v
asExpr [TNum v1, TPlus, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 + v2)
asExpr [TNum v1, TEq, TNum v2] = Eq (Data $ Aeson.Number (fromFloatDigits v1)) (Data $ Aeson.Number (fromFloatDigits v2))
    where
    zz :: Scientific = 9
asExpr [TJsonpath, TQuoted t] = App (Fn "jsonpath") (Data $ Aeson.String t)
asExpr tokens = trace ("asExpr:" ++ show tokens) (Data $ Aeson.Null)



-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

float :: Parser Double
float = L.signed sc (try L.float <|> fmap fromIntegral L.decimal)

parseFloat :: Text -> Either (ParseErrorBundle Text Void) Double
parseFloat = parse (float <* eof) "<input>"


-- ??: can we avoid duplication of Env type defs using typeclasses sig or whatnot
-- type Env = HM.HashMap Text Aeson.Value
type Env = HM.HashMap String Aeson.Value
type Parser = Parsec Void Text

trimQuotes :: Text -> Text
trimQuotes t =
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

eval :: Env -> Text -> IO Aeson.Value
eval env input = do
    trace ("exprP input:\t" ++ show input) $ case runParser exprP "" input of
        Left _ -> pure $ Aeson.String input
        Right (tokens :: [Token]) -> do
            e <- toExpr env (trace ("tokens:\t" ++ show tokens) tokens)
            pure $ (trace ("match input:\t" ++ show e) $ finalValue env (match env e))


finalValue :: Env -> Expr -> Aeson.Value

finalValue env (Data x@(Aeson.String k)) =
    trace ("matched:\t" ++ show x ++ "\tfinalValue!") (case HM.lookup (Text.unpack k) env of
        Just v -> v
        Nothing -> x)

finalValue _ (Data final) = trace "eval:final not a string" final
finalValue _ e = Aeson.String (Text.pack $ show e)

data Expr
  = Data Aeson.Value
  | Fn   String

  | Neg Expr
  | Eq  Expr Expr
  | Neq Expr Expr

  | Add Expr Expr

  | App Expr Expr
  deriving (Show, Eq)

-- headers: header, cookie
-- body: jsonpath, bytes

match :: Env -> Expr -> Expr
match env = go  -- ??: find necessity for runExcept
    where
    go :: Expr -> Expr

    go final@(Data _) = final

    go (Neg (Data (Aeson.Bool b))) = Data (Aeson.Bool (not b))
    go (Neg e) = go (Neg (go e))

    go (Eq (Data v1) (Data v2)) = Data (Aeson.Bool (v1 == v2))
    go (Eq e1 e2) = go (Eq (go e1) (go e2))
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (App (Fn "ident") arg@(Data _)) = arg

    go (App (Fn "jsonpath") (Data (Aeson.String q))) =
        let root :: Aeson.Value = (HM.lookupDefault Aeson.Null "RESP_BODY" env) in
        trace "go jsonpath called!!" $ case queryBody (Text.unpack q) root of
            Nothing -> Data $ Aeson.String ""  -- ??: maybe due to space-prefixed q string
            Just one -> Data one

    go (App (Fn "today") _) = trace "go today:" (Data (Aeson.String "??"))

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

invocJsonpath :: Parser [Token]
invocJsonpath = try $ do
    _ <- C.string "jsonpath"
    sc
    _ <- C.char '"'
    t <- takeWhile1P Nothing (/= '"')
    _ <- C.char '"'
    -- ??
    -- t <- Text.pack <$> (C.char '"' *> manyTill L.charLiteral (C.char '"'))
    pure [TJsonpath, TQuoted t]

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
  | TEq | TNeq
  | TJsonpath
  | TIdentifier Text
  | TQuoted Text
  | TParenOpn | TParenCls
  -- | TNum Scientific  -- ??: pro [Aeson] opp [parseFloat]
  | TPlus
  | TNum Double
  deriving (Show, Eq)

-- ??
tokenOpn :: Parser Token
tokenOpn = TParenOpn <$ C.char '('
-- tokenOpn :: Parser Token
-- tokenOpn = try $ do
--     _ <- C.char '('
--     pure TParenOpn

tokenUnit :: Parser Token
tokenUnit = try $ do
    _ <- C.string "()"
    pure TUnit

tokenTrue :: Parser Token
tokenTrue = try $ do
    _ <- C.string "true"
    pure $ TBool True

tokenFalse :: Parser Token
tokenFalse = try $ do
    _ <- C.string "false"
    pure $ TBool False

bool :: Parser [Token]
bool = try $ do
    b <- tokenFalse <|> tokenTrue
    pure [b]

tokenNeq :: Parser Token
tokenNeq = try $ do
    sc
    _ <- C.string "!="
    sc
    pure TNeq

tokenEq :: Parser Token
tokenEq = try $ do
    sc
    -- _ <- many (C.char ' ') -- `many` is zero or more.
    _ <- C.string "=="
    sc
    pure TEq

numEqNum :: Parser [Token]
numEqNum = try $ do
    num1 :: Double <- float
    eq <- tokenEq  -- ?? or tokenNeq
    num2 :: Double <- float
    pure [TNum num1, eq, TNum num2]

-- identifier (many arg)
-- today()
-- size(identifier)
invoc :: Parser [Token]
invoc = try $ do
    fn :: Text <- identifier
    opn <- TParenOpn <$ C.char '('
    cls <- TParenCls <$ C.char ')'
    pure [TIdentifier fn, opn, cls]

exprP :: Parser [Token]
exprP = try $ do
    sc
    -- tokens <- numEqNum <|> bool <|> invoc <|> word <|> invocJsonpath
    tokens <- invocJsonpath <|> numEqNum <|> bool <|> invoc <|> word
    pure tokens

templateP :: Parser [Segment]
templateP = do
    segs <- many (try litP <|> varP)
    eof
    pure segs

-- "{{whole_thing_in_mustache}}"
-- "may have prefix {{subst to eval}} or suffix"

partsP :: Parser [Part]
partsP = do
    -- parts :: [Part] <- many ((try needsEvalP') <|> literalP)
    -- pure parts
    -- many ((try needsEvalP') <|> literalP)
    many ((try needsEvalP) <|> literalP)

-- {{...}} variable (escaped) ??
needsEvalP' :: Parser Part
needsEvalP' = try $ do
    _ <- C.string "{{"
    sc
    w <- wordP
    sc
    _ <- C.string "}}"
    pure (L w)

needsEvalP :: Parser Part
needsEvalP = try $ do
    _ <- C.string "{{"
    sc
    w <- textP
    -- w <- wordP
    sc
    _ <- C.string "}}"
    pure (L w)

literalP :: Parser Part
literalP = try $ do
    t <- takeWhile1P Nothing (/= '{')
    pure (R t)

asText :: Aeson.Value -> Text
asText x = decodeUtf8 (ByteString.toStrict (Aeson.encode x))

-- render :: Aeson.Value -> [Arg] -> IO Aeson.Value
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
--
-- render-template requires double brace on both ends
--    render({{eval( )}} and {{eval( )}})
--   frender(Aeson.Value, Text, Aeson.Value)
-- frender :: [Aeson.Value] -> Aeson.Value

partitions :: Text -> [Part]
partitions input =
    case runParser partsP "??: unused?" input of
        Left _ -> trace "partitions 0" [R input]
        Right [] -> trace "partitions A" [R input]
        Right parts -> trace "partitions B" parts

-- Argument either needs evaluation (Left) or already just "Right".
render :: Env -> Aeson.Value -> [Part] -> IO Aeson.Value
render env accStr [] =
    pure accStr

render env (Aeson.String acc) ((R t):rest) =
    let grow :: Text = Text.concat [acc, t] in
    render env (Aeson.String grow) rest

render env (Aeson.String acc) ((L t):rest) = do
    evaled :: Aeson.Value <- eval env t

    -- Non-String `eval` result is internal error.
    -- ??: handle Aeson.Object
    let Aeson.String txt = trace ("evaled:\t" ++ show evaled) evaled

    render env (Aeson.String $ Text.concat [acc, txt]) rest


impureRenderTemplate :: Env -> Text -> IO (Either String Text)
impureRenderTemplate env input = do
    pure (Right "")

identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure [TIdentifier (Text.pack xs)]

textP :: Parser Text
textP = do
    Text.pack <$> some (C.alphaNumChar <|> oneOf ("_(). \"\"$" :: String))

wordP :: Parser Text
wordP = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure (Text.pack xs)


relP :: Parser Segment
relP = try $ do
  _ <- C.string "{{"
  -- allow optional spaces inside
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  name <- wordP
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  _ <- C.string "}}"
  pure (Var name)

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
