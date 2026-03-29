{-# LANGUAGE BangPatterns, OverloadedStrings, ScopedTypeVariables, QuasiQuotes #-}
{-# LANGUAGE GADTs #-}

module BEL
  ( Env(..)
  , run
  , partitions, Part(..)
  , render
  , mapEval
  , eval
  , Token(..), Expr(..), match, finalValue, queryEnvRespBody
  , dummy
  ) where


import Debug.Trace

import           Control.Lens
import           Control.Applicative (empty)
import           Data.Scientific (Scientific, floatingOrInteger)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Vector as Vec
import qualified Data.Aeson as Aeson (Value(..), decode)
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
                                 , notFollowedBy
                                 )

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client (Response (..), Request (..))
import qualified Data.Text.Encoding as TE

import Network.HTTP.Client          (Response (..), Request (..), defaultRequest,
                                      RequestBody (..)
                                      , createCookieJar)
import Network.HTTP.Client.Internal (ResponseClose (..), Response (..))
import Network.HTTP.Types.Status    (mkStatus)
import Network.HTTP.Types.Version   (http11)


import qualified BEL.BatteriesMain as BEL
import           BEL.Pratt



-- Space consumer
sc :: Parser ()
sc = L.space C.space1 empty empty

number :: Parser Scientific
number = L.scientific

type Parser = Parsec Void Text

tokenize :: Text -> [Token]
tokenize input =
    -- From BEL's perspective, runParser is a muddy terminology. In any case we
    -- combinatorically defined exprP and get tokens here.
    case runParser (exprP <* eof) "" input of
        Left _ -> []
        Right tokens -> tokens

run :: Env -> Text -> IO Expr
run env input = do
    let (expr, _rest) = pratt 0 (tokenize input)
    eval env expr

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
                -- matched <- eval env expr

                -- case matched of
                case match env expr of
                    res -> pure res

runExprP input = runParser (exprP <* eof) "" input

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


-- ??: String responseBody assumption can get us a long way.
queryEnvRespBody :: Env -> Text -> Expr
queryEnvRespBody env q =
    let lbs :: LBS.ByteString = responseBody (responseCopy env) in
    let Just root :: Maybe Aeson.Value = Aeson.decode lbs in
    case queryBody (show' q) root of
        Just av -> aesonToExpr av
        _ -> VNull

showRespBody :: Env -> Expr
showRespBody env =
    -- ??: not all responseBody is textual. also, "print N first characters" seems useful
    let lbs = responseBody (responseCopy env) in
    VString (TE.decodeUtf8 (LBS.toStrict lbs))

dummy :: Env
dummy = Env
  { responseCopy = Response
      { responseStatus     = mkStatus 200 "OK"
      , responseVersion    = http11
      , responseHeaders    =
          [ ("Content-Type",     "application/json")
          , ("X-Request-Id",     "a]bc-1234-def0-5678")
          , ("Cache-Control",    "no-store")
          ]
      -- , responseBody       = "{\"userId\":42,\"name\":\"Alice\",\"roles\":[\"admin\",\"editor\"],\"meta\":{\"theme\":\"dark\"}}"
      , responseBody       = "{\"page\":1,\"userId\":42,\"name\":\"Alice\",\"roles\":[\"admin\",\"editor\"],\"meta\":{\"theme\":\"dark\"}}"
      , responseCookieJar  = createCookieJar []
      -- , responseClose'     = ResponseClose (pure ())
      -- , responseOriginalRequest = defaultRequest
      --     { host   = "api.example.com"
      --     , port   = 443
      --     , secure = True
      --     , path   = "/v1/users/42"
      --     , method = "GET"
      --     }
      }

  , requestCopy = defaultRequest
      { method         = "POST"
      , host           = "api.example.com"
      , port           = 443
      , secure         = True
      , path           = "/v1/users"
      , requestHeaders =
          [ ("Content-Type",  "application/json")
          , ("Authorization", "Bearer tok_live_abc123")
          , ("Accept",        "application/json")
          ]
      , requestBody    = RequestBodyLBS
          "{\"name\":\"Bob\",\"email\":\"bob@example.com\"}"
      , queryString    = "?sort=asc&limit=10"
      }

  , bindings = HM.fromList
      [ ("BASE_URL",    Aeson.String "https://api.example.com")
      , ("TOKEN",       Aeson.String "tok_live_abc123")
      , ("MAX_RETRIES", Aeson.Number 3)
      -- , ("USER_META",   Aeson.Object (HM.fromList
      --       [ ("theme", Aeson.String "dark")
      --       , ("lang",  Aeson.String "en")
      --       , ("prefs", Aeson.Object (HM.fromList
      --             [ ("notifications", Aeson.Bool True)
      --             , ("fontSize",      Aeson.Number 14)
      --             ]))
      --       ]))
      , ("TAGS",        Aeson.Array (Vec.fromList
            [ Aeson.String "production"
            , Aeson.String "v2"
            ]))
      ]
  }


eval :: Env -> Expr -> IO Expr
eval env = rec
    where
    rec :: Expr -> IO Expr

    -- Implement hhs debug as special case of VTrace ("trace" as in haskell
    -- Debug.Trace tradition).
    rec (VTrace arg opt) = do
        v <- rec arg
        print v
        case opt of
            Nothing -> pure v
            Just custom -> pure custom

    rec e = case match env e of
        res@(VTrace _ _) -> rec res
        res            -> do
                print e
                pure res

match :: Env -> Expr -> Expr
match env = go
    where
    go :: Expr -> Expr

    go final@(VNum _) = final

    go (VIdent t) =
        case HM.lookup (Text.unpack t) (bindings env) of
            Just val -> aesonToExpr val
            Nothing -> VNull

    go (ENeg e) =
        case go e of
            VBool b -> VBool (not b)
            VNum n  -> VNum (-n)
            e' -> ENeg e'

    go (EEq e1 e2) = VBool (go e1 == go e2)
    go (ENeq e1 e2) = go (ENeg (EEq e1 e2))

    go (ELte e1 e2) =
        case (go e1, go e2) of
            (VNum n1, VNum n2) -> VBool (n1 <= n2)
            _ -> VBool False

    go (EGte e1 e2) =
        case (go e1, go e2) of
            (VNum n1, VNum n2) -> VBool (n1 >= n2)
            _ -> VBool False

    -- ??: generalize $ @ %  >debug "$.method"

    go (EDebug e) =
        VTrace (go e) (Just $ VBool True)

    -- jsonpath-query accepting Expr variant
    -- Querying Expr
    -- go (App (Fn "jsonpath") (VString "$.page")) =
    -- go (App (Fn "jsonpath") (VString "$")) =
    -- go (App (Fn "jsonpath") (VString "USER_META.theme")) =
    -- go (App (Fn "jsonpath") (VString q)) =
    go (EJsonpath (VString q)) =
        queryEnvRespBody env q

    go (EAdd e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 + v2)
            (r1, r2) -> EAdd r1 r2

    go (EMul e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 * v2)
            (r1, r2) -> EMul r1 r2

    go (ESub e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 - v2)
            (r1, r2) -> ESub r1 r2

    go (EDiv e1 e2) =
        case (go e1, go e2) of
            (VNum v1, VNum v2) -> VNum (v1 / v2)
            (r1, r2) -> EDiv r1 r2
    go e = trace ("match::" ++ show e) e

-- Expect one matching Value. Hoogle Data.Aeson.JSONPath to explore other
-- interfaces.
queryBody :: String -> Aeson.Value -> Maybe Aeson.Value
queryBody q root = case Aeson.query q root of
    Left _ -> trace ("qbn1;q=" ++ show q ++ ";root=" ++ show root) Nothing
    Right v -> case Vec.uncons v of
        Nothing ->       trace "qbn2" Nothing
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


keyword :: Text -> Parser ()
keyword w = C.string w *> notFollowedBy (C.alphaNumChar <|> C.char '_' <|> C.char '.')

boolP :: Parser Token
boolP = choice
  [ TFalse <$ keyword "false"
  , TTrue  <$ keyword "true"
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
  [ try relP
  , operatorP
  , TParenOpn <$ C.char '('
  , TParenCls <$ C.char ')'
  , TNum <$> number
  , try boolP
  , try $ TJsonpath <$ keyword "jsonpath"
  , try $ TDebug <$ keyword "debug"
  , TQuoted <$> (C.char '"' *> takeWhileP Nothing (/= '"') <* C.char '"')
  , TIdentifier <$> identifier'
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
