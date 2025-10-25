{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


import Debug.Trace

import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Void (Void)
import qualified Data.Aeson.Types as Aeson (Value(..))
import           Data.Aeson.QQ.Simple (aesonQQ)
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



-- -- ok
-- main :: IO ()
-- main = do
--     let root :: Aeson.Value = [aesonQQ| { "data": { "token": "abcdefghi9" } } |]
--     let envNew :: Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]
--
--     -- ok:
--     -- res <- render envNew (Aeson.String "") (partitions "  {{year}}  another sen {{year}} tence")
--     -- res <- render envNew (Aeson.String "") (partitions "{ \"user\": 12 }")
--     -- res <- render envNew (Aeson.String "") (partitions "jsonpath \"$.data\"")
--     let parted = partitions "{{jsonpath \"$.data\"}}"  -- expect L "jsonpath \"$.data\""
--     putStrLn $ show parted

-- ok
main :: IO ()
main = do
    let root :: Aeson.Value = [aesonQQ| { "data": { "token": "abcdefghi9" } } |]
    let envNew :: Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]
    -- ?? higher order Expr don't yield space-prefixed query string

    let parted = partitions "{{jsonpath \"$.data.token\"}}"  -- expect L "jsonpath \"$.data\""
    putStrLn $ show parted

    rendered <- render envNew (Aeson.String "") parted
    putStrLn $ show rendered

    -- -- let prog :: Expr = asExpr [TIdentifier "jsonpath", TQuoted "$.data.token"]
    -- let prog :: Expr = asExpr [TJsonpath, TQuoted "$.data.token"]
    -- -- let prog :: Expr = App (Fn "jsonpath") (Data $ Aeson.String "$.data.token")
    -- let matched = match envNew prog
    -- let final = finalValue envNew matched
    --
    -- putStrLn $ show final


-- ?? arith tests

-- rendered :: Aeson.Value <- BEL.render env (Aeson.String "") (BEL.partitions $ Text.pack s)

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
