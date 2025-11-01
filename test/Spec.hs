{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


import Debug.Trace

import Test.Tasty
import Test.Tasty.HUnit
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

import qualified BEL

main :: IO ()
main = defaultMain $ testGroup "Happy tests"
  -- [ test2 ]

  [ test0
  , test1
  , test2
  , test5
  ]



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

-- ?? higher order Expr don't yield space-prefixed query string
test0 :: TestTree
test0 = testCase "valid bel program" $ do
    let parted = BEL.partitions "{{jsonpath \"$.data.token\"}}"

    case parted == [BEL.L "jsonpath \"$.data.token\""] of
        True -> pure ()
        _ -> assertFailure $ show parted

test1 :: TestTree
test1 = testCase "jsonpathArg" $ do
    let root :: Aeson.Value = [aesonQQ| { "data": { "token": "abcdefghi9" } } |]
    let envNew :: BEL.Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]

    avPositive <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{jsonpath \"$.data.token\"}}")
    avNegative <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{jsonpath \"$.data.doesntexist\"}}")

    case (avPositive, avNegative) of
        (Aeson.String "abcdefghi9", Aeson.String "") -> pure ()
        (av, _) -> assertFailure $ show av

test2 :: TestTree
test2 = testCase "partition sentence" $ do
    let parted = BEL.partitions "been up for {{dur}} minutes."

    case parted == [BEL.R "been up for ", BEL.L "dur", BEL.R " minutes."] of
        True -> pure ()
        _ -> assertFailure $ show parted

test3 :: TestTree
test3 = testCase "arith basic" $ do
    let envNew :: BEL.Env = HM.fromList []

    av <- BEL.eval envNew "2 * 1000"  -- PICKUP exprP body

    case av of
        Aeson.Number 2000 -> pure ()
        _ -> assertFailure $ show av

-- test4 :: TestTree
-- test4 = testCase "arith complex" $ do
    -- let envNew :: BEL.Env = HM.fromList [("kibi", Aeson.Number 1024)]
    -- av <- BEL.eval envNew "2 * kibi"

test5 :: TestTree
test5 = testCase "bool literals" $ do
    avTrue <-  BEL.eval HM.empty "true"
    avFalse <- BEL.eval HM.empty "false"
    case (avTrue, avFalse) of
        (Aeson.Bool True, Aeson.Bool False) -> pure ()
        got -> assertFailure $ show got

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
