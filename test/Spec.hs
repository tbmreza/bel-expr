{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Debug.Trace

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Text.Megaparsec.Char as C
import           Text.Megaparsec ( Parsec, (<|>), some, anySingle, many, choice
                                 , manyTill, lookAhead, try, eof, runParser
                                 , parseTest
                                 )

import BEL

-- main :: IO ()
-- main = do
--     let envNew :: Env = HM.fromList [("yyyymmdd", Aeson.String "19700101"), ("year", Aeson.String "2025")]
--     case renderAesonTemplate envNew (Aeson.String "{{today}}") of
--         _ -> putStrLn "atas"
--
--     -- case renderTemplate envNew "http://localhost:9999/path{{yyyymmdd}}tail" of
--     case renderTemplate envNew "aa{{today}}bbb" of  -- ok templateP1
--     -- case renderTemplate envNew "{ "start": "{{today}}" }" of
--     -- case renderTemplate envNew "{{year}}betw{{yyyymmdd}}" of
--         Left res -> putStrLn res
--         Right res -> putStrLn res

-- # jsonpath "$.data.name" == "alice"

-- main :: IO ()
-- main = do
--     -- let e = App (Fn "ident") (Data (Aeson.String "Alice"))
--     let e = App (Fn "today") (Data (Aeson.String "()"))
--     -- let e = App (Fn "jsonpath") (Data (Aeson.String "$.data"))
--     -- let e = Data (Aeson.String "Alice")
--     -- let e = Neq (Data (Aeson.Number 14)) (Data (Aeson.Number 12))
--     -- let e = Neg (Data $ Aeson.Bool False)
--     -- putStrLn $ show $ isPredicate (match e)
--     putStrLn $ show $ (match e)

-- main :: IO ()
-- main = do
--     -- res <- parseTest litP "hello()"
--     res <- parseTest templateP "hello(){{today()}}bb"
--     let envNew :: Env = HM.fromList [("yyyymmdd", Aeson.String "19700101"), ("year", Aeson.String "2025")]
--     -- ok templateP1
--     -- case renderTemplate envNew "aa{{today}}bbb" of
--     -- case renderTemplate envNew "12 != 12" of
--     -- case renderTemplate envNew "true" of
--     case renderTemplate envNew "today()" of
--         Left id -> putStrLn id
--         Right res -> putStrLn res



main :: IO ()
main = do
    let envNew :: Env = HM.fromList [("year", Aeson.String "2025")]
    res <- render envNew (Aeson.String "") (partitions "  {{year}}  another sen {{year}} tence")
    putStrLn $ show res
    -- ?? arith tests

-- ok
-- main :: IO ()
-- main = do
--     let tokens = case runParser exprP "todo" "14.2 == 14.2  " of
--     -- let tokens = case runParser exprP "todo" "today()" of
--             Right tokens -> trace "test:tokens" tokens
--             Left msg -> trace "test:msg" []
--
--     putStrLn $ show tokens
--
--     let ast :: Expr = asExpr tokens
--     putStrLn $ "ast:\t" ++ show ast
--
--     let result = match ast
--     putStrLn $ "result:\t" ++ show result


-- main :: IO ()
-- main = do
--     case parseFloat "-04.14009" of
--         Left _ -> putStrLn "left"
--         Right d -> putStrLn $ show d
