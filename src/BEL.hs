{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}

module BEL
  ( Env
  , partitions, Part(..)
  , render
  , mapEval
  -- For testing:
  -- , toExpr
  , Token(..), Expr(..), match, finalValue, queryEnvRespBody
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
                                 , takeWhileP
                                 , choice
                                 , eof
                                 )

import qualified BEL.BatteriesMain as BEL
import           BEL.Pratt


-- ?? : parametrized fn invocation "loremIpsum 5" -> ... BEL.loremChars 5

-- toExpr :: Env -> [Token] -> Expr
-- toExpr env toks =
--     let (expr, _rest) = pratt 0 toks
--         res = match env expr
--     case res of
--
--         EPrint e -> do
--             print (finalValue e)
--             pure (VBool True)
--
--         _ -> pure res


-- toExpr :: Env -> [Token] -> IO Expr
--
-- -- toExpr _env (TIdentifier "debug" : TQuoted s : []) = do
-- --     print (finalValue (VString "hardcodid"))
-- --     pure (VBool True)
--
-- -- toExpr _env (TIdentifier thunk : TParenOpn : TParenCls : []) = do
-- --     case thunk of
-- --         "today" -> do
-- --             tdy <- BEL.ioToday
-- --             pure $ VString (Text.pack tdy)
-- --         "year" -> do
-- --             yr <- BEL.ioYear
-- --             pure $ VString (Text.pack yr)
-- --         "dayOfMonth" -> do
-- --             dom <- BEL.ioDayOfMonth
-- --             pure $ VString (Text.pack dom)
-- --         _ -> pure $ VString ""
--
-- toExpr env toks = do
--     let (expr, _rest) = pratt 0 toks
--         res = match env expr
--     case res of
--
--         EPrint e -> do
--             print (finalValue e)
--             pure (VBool True)
--
--         _ -> pure res


-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

number :: Parser Scientific
number = do
    L.signed sc L.scientific

type Parser = Parsec Void Text

-- (auto) gemini's untangling proposal:
-- pratt ::  Env -> Int -> [Token] -> (Expr, [Token])
-- toExpr :: Env -> [Token] -> (Expr, [Token])
-- match ::  Env -> Expr -> Expr
-- eval ::   Env -> Expr -> IO Expr
-- eval env expr = case expr of
--     EPrint arg -> do
--         val <- eval env arg
--         print val 
--         return val
--     _ -> return (match env expr)

-- efal :: Env -> Expr -> IO Expr
-- efal env expr = 
--     case expr of
--         -- Call name args -> match env (Call name args)
--         _ -> pure expr

mapEval :: Env -> [Text] -> IO [Expr]
mapEval env lines = do
    mapM (h env) lines
    where
    h :: Env -> Text -> IO Expr
    h env input = do
        case runParser (exprP <* eof) "" input of
            Left _ -> pure $ VString input
            Right tokens -> do
                let (expr, _rest) = pratt 0 tokens

                case match env expr of
                    res -> pure res

runExprP input = runParser (exprP <* eof) "" input

eval :: Env -> Expr -> IO Expr
eval env expr = do
    undefined

    -- case runExprP of
    --     _ -> undefined

    -- case expr of
    --     EPrint arg -> do
    --         val <- eva env arg
    --         print val
    --         pure val
    --     _ -> do
    --         pure (match env expr)

finalValue :: Expr -> Aeson.Value

finalValue (VString k) = Aeson.String k
finalValue (VBool s) =   Aeson.Bool s
finalValue (VNum s) =    Aeson.Number s
finalValue (VObj s) =    Aeson.Object s
finalValue (VArray s) =  Aeson.Array s
finalValue VNull =       Aeson.Null

finalValue e = Aeson.String (Text.pack $ show e)


aesonToExpr :: Aeson.Value -> Expr
aesonToExpr (Aeson.Bool v)   = VBool v
aesonToExpr (Aeson.String v) = VString v
aesonToExpr (Aeson.Number v) = VNum v
aesonToExpr (Aeson.Object v) = VObj v
aesonToExpr (Aeson.Array v)  = VArray v
aesonToExpr Aeson.Null       = VNull


queryEnvRespBody :: Env -> Text -> Expr
queryEnvRespBody env q =
    -- PICKUP lens after it's alive
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

    go (Neg e) =
        case go e of
            VBool b -> VBool (not b)
            e' -> Neg e'

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

    -- ??: generalize $ @ %  >debug "$.method"
    -- ?? : pattern match when queryBody returns none, then lookup, else literal
    -- debug $.method
    go (App (Fn "debug") (VString q)) =
        EPrint (queryEnvRespBody env q)


    -- ??
    -- go (App (Fn "request") (VString q)) =
    --     Expr "POST"

    go (App (Fn "jsonpath") (VString q)) =
        queryEnvRespBody env q

    go (Add e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 + v2)
            (r1, r2) -> Add r1 r2

    go (Mul e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 * v2)
            (r1, r2) -> Mul r1 r2

    go (Sub e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 - v2)
            (r1, r2) -> Sub r1 r2

    go (Div e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 / v2)
            (r1, r2) -> Div r1 r2

    go e = e

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

relP :: Parser Token
relP = choice
  [ TNeq <$ C.string "!="
  , TEq  <$ C.string "=="
  , TLte <$ C.string "<="
  , TGte <$ C.string ">="
  ]

operatorP :: Parser Token
operatorP = choice
  [ TPlus  <$ C.char '+'
  , TMinus <$ C.char '-'
  , TMult  <$ C.char '*'
  , TDiv   <$ C.char '/'
  ]

tokenP :: Parser Token
tokenP = choice
  [ try $ TNeq <$ C.string "!="
  , try $ TEq  <$ C.string "=="
  , try $ TLte <$ C.string "<="
  , try $ TGte <$ C.string ">="
  , try $ TPlus  <$ C.char '+'
  , try $ TMinus <$ C.char '-'
  , try $ TMult  <$ C.char '*'
  , try $ TDiv   <$ C.char '/'
  , try $ TParenOpn <$ C.char '('
  , try $ TParenCls <$ C.char ')'
  , try $ TNum <$> number
  , try $ boolP
  , try $ TJsonpath <$ C.string "jsonpath"
  , try $ TQuoted <$> (C.char '"' *> takeWhileP Nothing (/= '"') <* C.char '"')
  , try $ TIdentifier <$> identifier'
  ]

identifier' :: Parser Text
identifier' = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.'
    pure (Text.pack xs)

exprP :: Parser [Token]
exprP = sc *> many (tokenP <* sc)

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

-- Argument either needs evaluation (Left) or already just "Right". Doesn't
-- bubble exceptions further up; render failure echoes the input.
render :: Env -> Aeson.Value -> [Part] -> IO Aeson.Value
render _env accStr [] =
    pure accStr

render env (Aeson.String acc) ((R t):rest) =
    let grow :: Text = Text.concat [acc, t] in
    render env (Aeson.String grow) rest

render env (Aeson.String acc) ((L t):rest) = do
    xp :: Expr <- case runExprP t of
            Left _ -> pure $ VString t
            Right tokens -> do
                let (expr, _rest) = pratt 0 tokens
                pure expr

    -- evaled :: Expr <- eval env t
    let evaled = match env xp

    -- render necessitates for effective final values to be string.
    let str = case finalValue evaled of
            Aeson.String txt -> show' txt
            Aeson.Object obj -> show obj
            Aeson.Number n -> case floatingOrInteger n of
                                  Right (i :: Integer) -> show i
                                  Left (_ :: Double)   -> show n
            _ -> ("unhandled render L" :: String)

    let av = Aeson.String $ Text.concat [acc, Text.pack str]
    render env av rest

render _ _ _ = pure $ Aeson.String ""
-- render _ _ _ = undefined  -- debugging


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
