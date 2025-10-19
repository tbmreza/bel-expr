{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import           Data.Aeson as Aeson (encode)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some, anySingle, many, choice
                                 , manyTill, lookAhead, try, eof, runParser
                                 , getInput, takeWhile1P, ParseErrorBundle
                                 , parse
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
toExpr env [TIdentifier t] = pure $ -- Not really an IO; just so asExpr don't require Env.
    Data $ case HM.lookup (Text.unpack t) env of
        Just v -> v
        _ -> Aeson.String t
toExpr _ [TIdentifier thunk, TParenOpn, TParenCls] = do
-- [TIdentifier thunk, TUnit] =
    tdy <- ioToday
    pure $ case thunk of
        "today" -> Data $ Aeson.String (Text.pack tdy)
        "year" -> Data $ Aeson.String "2025"
        "dayOfMonth" -> Data $ Aeson.String "4"
        "loremIpsum" -> Data $ Aeson.String "lorem ipsum sit"  -- ??: 255 chars of lorem ipsum text
        _ -> Data $ Aeson.Null
toExpr env els = pure $ asExpr els
-- toExpr env els = pure $ asExpr env els


asExpr :: [Token] -> Expr
asExpr [TBool v] = Data $ Aeson.Bool v
-- asExpr [TNum v] = Data $ Aeson.Number v
asExpr [TNum v1, TPlus, TNum v2] = Data $ Aeson.Number (fromFloatDigits $ v1 + v2)
asExpr [TNum v1, TEq, TNum v2] = Eq (Data $ Aeson.Number (fromFloatDigits v1)) (Data $ Aeson.Number (fromFloatDigits v2))
    where
    zz :: Scientific = 9

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



-- renderTemplateText :: Env -> Text -> Either Text Text
-- renderTemplateText env input = do
--     case runParser templateP "template" input of
--         Left err -> trace "upstream rtt Left" (Right input)
--
--         Right segs ->
--             let ret = (trimQuotes $ trimQuotes $ render1 env segs) in
--             trace ("upstream rtt Rite") $ Right ret


-- -- Either echoes input (on error) or evaluates template given Env.
-- renderTemplate :: Env -> String -> Either String String
-- renderTemplate env input =
--     case runParser templateP "template" (Text.pack input) of
--         Left err -> Right input
--         Right segs -> Right (Text.unpack $ (trimQuotes $ render1 env segs))

isPredicate :: Expr -> Bool
isPredicate e =
    case e of
        Data (Aeson.Bool _) -> True
        _ -> False

eval :: Env -> Text -> IO Aeson.Value
eval env input = do
    let tokens = case runParser exprP "todo" input of
            Left _ -> trace ("eval atas") []
            Right tokens -> trace "eval bawah" tokens
    e <- toExpr env tokens
    pure $ finalValue env (match e)

-- eval1 :: Text -> Aeson.Value
-- eval1 t =
--     -- http://localhost:9999/echo.php
--     case runParser exprP "todo" t of
--         -- Left _ -> Aeson.Null  ??
--         -- Left msg -> Aeson.String (Text.pack $ show msg)
--         -- Left msg -> Aeson.String (mconcat ["bel_msg:\t", t])
--         Left msg -> Aeson.String t
--         Right tokens -> dataFrom tokens
--     where
--     dataFrom :: [Token] -> Aeson.Value
--     dataFrom tokens =
--         -- let Data v = match $ asExpr tokens in
--         -- v
--         let Data v = match $ asExpr tokens in
--         v

finalValue :: Env -> Expr -> Aeson.Value

finalValue env (Data x@(Aeson.String k)) =
    trace "finalValue!" (case HM.lookup (Text.unpack k) env of
        Just v -> v
        Nothing -> x)

finalValue _ (Data final) = trace "not ae string" final
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

match :: Expr -> Expr
match = go  -- ??: find necessity for runExcept
    where
    go :: Expr -> Expr

    go final@(Data _) = final

    go (Neg (Data (Aeson.Bool b))) = Data (Aeson.Bool (not b))
    go (Neg e) = go (Neg (go e))

    go (Eq (Data v1) (Data v2)) = Data (Aeson.Bool (v1 == v2))
    go (Eq e1 e2) = go (Eq (go e1) (go e2))
    go (Neq e1 e2) = go (Neg (Eq e1 e2))

    go (App (Fn "ident") arg@(Data _)) = arg

    -- ??: acquire response body & headers
    go (App (Fn "jsonpath") (Data (Aeson.String s))) = Data Aeson.Null

    go (App (Fn "today") _) = trace "go today:" (Data (Aeson.String "??"))

    -- -- go (Neg e) = negate <$> go e


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
  | TIdentifier Text
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
    parts <- numEqNum <|> bool <|> invoc <|> word
    -- parts <- numEqNum <|> bool <|> invoc
    pure parts

templateP :: Parser [Segment]
templateP = do
    segs <- many (try litP <|> varP)
    eof
    pure segs

-- "{{whole_thing_in_mustache}}"
-- "may have prefix {{subst to eval}} or suffix"

partsP :: Parser [Part]
partsP = do
    -- parts :: [Part] <- many ((try needsEvalP) <|> literalP)
    -- pure parts
    many ((try needsEvalP) <|> literalP)

-- {{...}} variable (escaped)
needsEvalP :: Parser Part
needsEvalP = try $ do
    _ <- C.string "{{"
    sc
    w <- wordP
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
        Right parts -> parts
        _ -> []

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
    let Aeson.String txt = evaled

    render env (Aeson.String $ Text.concat [acc, txt]) rest

impureRenderTemplate :: Env -> Text -> IO (Either String Text)
impureRenderTemplate env input = do
    pure (Right "")

-- render' :: Env -> [Segment] -> Text
-- -- render' env = (Text.concat . fmap renderSeg)
-- render' env = do
--     res
--     where
--     res :: [Segment] -> Text
--     res = Text.concat . mapped
--     -- mapped :: f0 Segment -> f0 Text
--     mapped = fmap renderSeg
--     renderSeg :: Segment -> Text
--     renderSeg (Lit t)     = asText $ eval1 t
--     renderSeg _ = ""
--     -- renderSeg (Var k)  =
--     --     let val :: Aeson.Value = case HM.lookup (Text.unpack k) env of
--     --          Just defined -> defined
--     --          Nothing -> eval k
--     --     in
--     --     decodeUtf8 (ByteString.toStrict (Aeson.encode val))
--     -- renderSeg (RawVar k)  =
--     --     let val :: Aeson.Value = (HM.lookupDefault Aeson.Null (Text.unpack k) env) in
--     --     decodeUtf8 (ByteString.toStrict (Aeson.encode val))

-- render1 :: Env -> [Segment] -> Text
-- render1 env = Text.concat . fmap renderSeg
--     where
--     renderSeg :: Segment -> Text
--     renderSeg (Lit t)     = asText $ eval1 t
--     renderSeg (Var k)  =
--         let val :: Aeson.Value = case HM.lookup (Text.unpack k) env of
--              Just defined -> defined
--              Nothing -> eval1 k
--         in
--         decodeUtf8 (ByteString.toStrict (Aeson.encode val))
--     renderSeg (RawVar k)  =
--         let val :: Aeson.Value = (HM.lookupDefault Aeson.Null (Text.unpack k) env) in
--         decodeUtf8 (ByteString.toStrict (Aeson.encode val))

identifier :: Parser Text
identifier = do
    xs <- some $ C.alphaNumChar <|> C.char '_'
    pure (Text.pack xs)


word :: Parser [Token]
word = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.' <|> C.char '(' <|> C.char ')'
    pure [TIdentifier (Text.pack xs)]


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
