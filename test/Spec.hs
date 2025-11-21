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
import BEL

main :: IO ()
main = defaultMain $ testGroup "Happy tests"
  -- [ test1 ]

  [ test0
  -- , test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
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

test0 :: TestTree
test0 = testCase "valid bel program" $ do
    let parted = BEL.partitions "{{jsonpath \"$.data.token\"}}"

    case parted == [BEL.L "jsonpath \"$.data.token\""] of
        True -> pure ()
        _ -> assertFailure $ show parted

test1 :: TestTree
test1 = testCase "render url parts" $ do
    let envNew :: BEL.Env = HM.fromList [("CAT", Aeson.String "animals")]

    -- all <- BEL.render envNew (Aeson.String "") (BEL.partitions "https://kernel.org/{{CAT}}/route.php?prefilt=9&lim={{10 * 2}}&filt=another")
    -- PICKUP
    all <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{10 * 2}}")
    -- all <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{100}}")
    -- all <- BEL.render envNew (Aeson.String "") (BEL.partitions "https://kernel.org/{{CAT}}/route.php?prefilt=9&lim={{10}}&filt=another")
    assertFailure $ show all

    -- let root :: Aeson.Value = [aesonQQ| { "data": { "token": "abcdefghi9" } } |]
    -- let envNew :: BEL.Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]
    --
    -- avPositive <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{jsonpath \"$.data.token\"}}")
    -- -- avNegative <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{jsonpath \"$.data.doesntexist\"}}")
    --
    -- assertFailure $ show avPositive
    --
    -- -- case (avPositive, avNegative) of
    -- --     (Aeson.String "abcdefghi9", Aeson.String "") -> pure ()
    -- --     all -> assertFailure $ show all

test2 :: TestTree
test2 = testCase "partition sentence" $ do
    let parted = BEL.partitions "been up for {{dur}} minutes."

    case parted == [BEL.R "been up for ", BEL.L "dur", BEL.R " minutes."] of
        True -> pure ()
        _ -> assertFailure $ show parted

test3 :: TestTree
test3 = testCase "arith basic" $ do
    let envNew :: BEL.Env = HM.fromList [("margin", Aeson.Number 3)]

    av1 <- BEL.eval envNew "2 * 1000"
    av2 <- BEL.eval envNew "47 + margin"

    case (av1, av2) of
        (Aeson.Number 2000, Aeson.Number 50) -> pure ()
        all -> assertFailure $ show all

test4 :: TestTree
test4 = testCase "arith long" $ do
    let envNew :: BEL.Env = HM.fromList []

    let prog :: Expr = Add (Num 0) (Mul (Num 3) (Num 3))
    let matched = finalValue envNew $ match envNew prog

    evaled <- BEL.eval envNew "9"

    case (matched, evaled) of
        (Aeson.Number 9, Aeson.Number 9) -> pure ()
        all -> assertFailure $ show all

test5 :: TestTree
test5 = testCase "bool literals" $ do
    avTrue <-  BEL.eval HM.empty "true"
    avFalse <- BEL.eval HM.empty "false"
    case (avTrue, avFalse) of
        (Aeson.Bool True, Aeson.Bool False) -> pure ()
        all -> assertFailure $ show all

test6 :: TestTree
test6 = testCase "assertion line" $ do
    av0 <-  BEL.eval HM.empty "200 !=  200"
    av1 <-  BEL.eval HM.empty "14 ==  14"

    case (av0, av1) of
        (Aeson.Bool False, Aeson.Bool True) -> pure ()
        all -> assertFailure $ show all

-- toExpr :: Env -> [Token] -> IO Expr
    -- let envNew = HM.fromList [("const", Aeson.String "several words with spaces")]
    -- e <- BEL.toExpr envNew [TIdentifier "const", TEq, TQuoted "several words with spaces"]
test7 :: TestTree
test7 = testCase "toExpr unit" $ do
    let envNew = HM.fromList [("nama", Aeson.String "contents")]
    e <- BEL.toExpr envNew [TIdentifier "nama", TEq, TQuoted "contents"]
    f <- BEL.toExpr envNew [TQuoted "wrong answer", TEq, TQuoted "wrong"]
    case (e, f) of
        (Data (Aeson.Bool True), Data (Aeson.Bool False)) -> pure ()
        els -> assertFailure $ show els

-- ??: test aesonQQ json bool literals testN = testCase "jsonpath invocation evals a bool" $ do
test8 :: TestTree
test8 = testCase "jsonpath invocation" $ do
    let root :: Aeson.Value = [aesonQQ| { "data": { "unchecked": 2005 } } |]
    let envNew :: BEL.Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]

    -- jsonpath "$.data.unchecked" == 2005
    let prog1 = Eq (App (Fn "jsonpath") (Data $ Aeson.String "$.data.unchecked")) (Data $ Aeson.Number 2005)
    let prog2 = App (Fn "jsonpath") (Data $ Aeson.String "$.data.unchecked")

    case (match envNew prog1, match envNew prog2) of
        (Data (Aeson.Bool True), Data (Aeson.Number 2005)) -> pure ()
        all -> assertFailure $ show all


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
