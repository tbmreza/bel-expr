{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BEL
    ( someFunc
    -- , eval
    , renderTemplate
    , Env
    ) where

import Debug.Trace
import Data.Aeson (Value, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)

import qualified Data.HashMap.Strict as HM (HashMap, lookupDefault)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some, anySingle, many, choice
                                 , manyTill, lookAhead, try, eof, runParser
                                 )

-- ??: can we avoid duplication of Env type defs using typeclasses or whatnot
type Env = HM.HashMap String Aeson.Value
type Parser = Parsec Void Text

-- Either echoes input (on error) or evaluates template given Env.
renderTemplate :: Env -> String -> Either String String
renderTemplate env input =
    case runParser templateP "template" (Text.pack input) of
        Left err -> Left ("kiri:\t" ++ show err)
        Right segs -> Right ("kanan:\t" ++ (Text.unpack $ render env segs))


renderStarter :: HM.HashMap Text Text -> Text -> Either String Text
renderStarter env input =
  case runParser templateP "template" input of
    Left err -> Left (show err)
    Right segs -> Right (render1 env segs)

-- eval :: Env -> String -> String
-- eval _ s = s
eval :: Env -> String -> Either String String
eval _ s =
    Right s

data Segment =
    Lit Text
  | Var Text
  | RawVar Text

templateP :: Parser [Segment]
-- templateP = many (choice [rawVarP, varP, litP]) <* eof
templateP = do
    r1 <- litP
    -- _ <- varP
    r3 <- litP
    -- pure [Lit "prefix", Var "yyyymmdd", Lit "suffix"]
    pure [r1, Var "yyyymmdd", r1]


-- render with an environment (HashMap)
render :: Env -> [Segment] -> Text
-- render env = Text.concat . fmap renderSeg
render env = trace "enter render" (Text.concat . fmap renderSeg)
    where
    renderSeg (Lit t)     = trace "renderA{{isi}}" t
    renderSeg (Var k)  =
        let val :: Aeson.Value = (HM.lookupDefault Aeson.Null (Text.unpack k) env) in
        decodeUtf8 (toStrict (encode val))
    renderSeg (RawVar k)  =
        let val :: Aeson.Value = (HM.lookupDefault Aeson.Null (Text.unpack k) env) in
        decodeUtf8 (toStrict (encode val))

render1 :: HM.HashMap Text Text -> [Segment] -> Text
render1 env = Text.concat . fmap renderSeg
  where
    -- renderSeg (Lit t)     = t
    -- renderSeg (RawVar k)  = HM.lookupDefault "" k env
    -- -- renderSeg (Var k)     = escapeHtml (HM.lookupDefault "" k env)  -- ??
    renderSeg (Var k)     = HM.lookupDefault "" k env


-- Letters, digits, underscore and dot for dotted names
ident :: Parser Text
ident = do
    xs <- some $ C.alphaNumChar <|> C.char '_' <|> C.char '.'
    pure (Text.pack xs)

-- {{...}} variable (escaped)
varP :: Parser Segment
varP = try $ do
  _ <- C.string "{{"
  -- allow optional spaces inside
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  name <- ident
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  _ <- C.string "}}"
  pure (Var name)

-- {{{...}}} raw variable (not escaped)
rawVarP :: Parser Segment
rawVarP = try $ do
  _ <- C.string "{{{"
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  name <- ident
  _ <- many (C.char ' ' <|> C.char '\t' <|> C.char '\n')
  _ <- C.string "}}}"
  pure (RawVar name)

-- parse a literal chunk: everything until next "{{" or "{{{" or EOF
litP :: Parser Segment
litP = do
  txt <- (Text.pack <$> manyTill anySingle (lookAhead (C.string "{{" <|> C.string "{{{" ) ))
         <|> (Text.pack <$> many anySingle)  -- fallback: consume rest
  pure (Lit txt)



someFunc :: IO ()
someFunc = putStrLn "hello from local bel"
