{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (forever)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)

import Lib

-- main :: IO ()
-- main = pure ()


-- Parser using Text
type Parser = Parsec Void T.Text

-- AST
data Expr
  = EInt Integer
  | EVar T.Text
  | EAdd Expr Expr
  | ESub Expr Expr
  | EMul Expr Expr
  | EDiv Expr Expr
  deriving (Show, Eq)

-- Top-level command: either an expression to evaluate, or a let-binding
data Command
  = CExpr Expr
  | CLet T.Text Expr
  deriving (Show, Eq)

-- Lexer helpers
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pIdentifier :: Parser T.Text
pIdentifier = lexeme $ do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_' <|> char '\'')
  return $ T.pack (first:rest)

pInteger :: Parser Integer
pInteger = lexeme (L.signed sc L.decimal)

-- -- Expression parser (precedence aware)
-- pTerm :: Parser Expr
-- pTerm = choice
--   [ EInt <$> pInteger
--   , EVar <$> pIdentifier
--   , pParens pExpr
--   ]

-- pFactor :: Parser Expr
-- pFactor = makeExprParser pTerm operators
--   where
--     operators =
--       [ [ InfixL (EMul <$ symbol "*"), InfixL (EDiv <$ symbol "/") ]
--       , [ InfixL (EAdd <$ symbol "+"), InfixL (ESub <$ symbol "-") ]
--       ]

-- pExpr :: Parser Expr
-- pExpr = pFactor

-- -- Command parser
-- pLet :: Parser Command
-- pLet = do
--   _ <- symbol "let"
--   name <- pIdentifier
--   _ <- symbol "="
--   e <- pExpr
--   return (CLet name e)

-- pCommand :: Parser Command
-- pCommand = sc *> choice [pLet, CExpr <$> pExpr] <* eof

-- Evaluation
type Env = HM.HashMap T.Text Integer

eval :: Env -> Expr -> Either T.Text Integer
eval env = go
  where
    go (EInt n) = Right n
    go (EVar name) = case HM.lookup name env of
      Just v -> Right v
      Nothing -> Left $ "undefined variable: " <> name
    go (EAdd a b) = binOp (+) a b
    go (ESub a b) = binOp (-) a b
    go (EMul a b) = binOp (*) a b
    go (EDiv a b) = do
      vb <- go b
      if vb == 0
        then Left "division by zero"
        else do
          va <- go a
          Right (va `div` vb)
    binOp f x y = do
      vx <- go x
      vy <- go y
      Right (f vx vy)

-- REPL loop (StateT Env IO)
repl :: StateT Env IO ()
repl = do
  liftIO $ TIO.putStr "> "
  -- liftIO $ TIO.hFlush stdout
  line <- liftIO TIO.getLine
  let input = T.strip line
  if input == "quit" || input == "exit"
    then liftIO $ putStrLn "bye"
    else liftIO $ putStrLn "bye"
    -- else do
    --   env <- get
    --   case parse pCommand "<stdin>" input of
    --     Left err -> liftIO $ putStrLn (errorBundlePretty err)
    --     Right cmd -> case cmd of
    --       CExpr e -> case eval env e of
    --         Left msg -> liftIO $ TIO.putStrLn ("Error: " <> msg)
    --         Right v -> liftIO $ print v
    --       CLet name e -> case eval env e of
    --         Left msg -> liftIO $ TIO.putStrLn ("Error: " <> msg)
    --         Right v -> do
    --           modify (HM.insert name v)
    --           liftIO $ TIO.putStrLn ("<set> " <> name <> " = " <> T.pack (show v))
    --   repl

main :: IO ()
main = do
  putStrLn "Megaparsec simple interpreter. Type expressions, or `let x = expr`. Type `quit` or `exit` to leave."
  _ <- evalStateT repl HM.empty
  return ()

