{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as Aeson (Value(..))
import           Data.Aeson.QQ.Simple (aesonQQ)

import qualified BEL
import BEL

main :: IO ()
main = defaultMain $ testGroup "Happy tests"
  [ test0
  -- , test1  -- pratt
  , test2
  , test3
  -- , test4  -- pratt
  , test5
  , test6
  , test7
  , test8
  -- , test9
  -- , test10
  , test11
  , test12
  ]

test0 :: TestTree
test0 = testCase "valid bel program" $ do
    let parted = BEL.partitions "{{jsonpath \"$.data.token\"}}"

    case parted == [BEL.L "jsonpath \"$.data.token\""] of
        True -> pure ()
        _ -> assertFailure $ show parted

test1 :: TestTree
test1 = testCase "render url parts" $ do
    let envNew :: BEL.Env = HM.fromList [("CAT", Aeson.String "animals")]

    av1 <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{10 * 2}}")
    av2 <- BEL.render envNew (Aeson.String "") (BEL.partitions "score {{100}}")
    av3 <- BEL.render envNew (Aeson.String "") (BEL.partitions "https://kernel.org/{{CAT}}/route.php?prefilt=9&lim={{10}}&filt=another")

    case (av1, av2, av3) of
        (Aeson.String "20.0", Aeson.String "score 100.0", Aeson.String "https://kernel.org/animals/route.php?prefilt=9&lim=10.0&filt=another") -> pure ()
        all -> assertFailure $ show all

test2 :: TestTree
test2 = testCase "partition sentence" $ do
    let parted = BEL.partitions "been up for {{dur}} minutes."

    case parted == [BEL.R "been up for ", BEL.L "dur", BEL.R " minutes."] of
        True -> pure ()
        _ -> assertFailure $ show parted

-- ??: testN pratt arith
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

-- ?? chance it's stack overflow
test9 :: TestTree
test9 = testCase "arith precedence" $ do
    -- 3 + 4 * 5 = 23
    av1 <- BEL.eval HM.empty "3 + 4 * 5"
    -- 4 * 5 + 3 = 23
    av2 <- BEL.eval HM.empty "4 * 5 + 3"

    case (av1, av2) of
        (Aeson.Number 23, Aeson.Number 23) -> pure ()
        all -> assertFailure $ show all

test10 :: TestTree
test10 = testCase "arith sub div" $ do
    -- 10 - 2 = 8
    av1 <- BEL.eval HM.empty "10 - 2"
    -- 20 / 4 = 5
    av2 <- BEL.eval HM.empty "20 / 4"
    -- 10 - 2 * 3 = 4
    av3 <- BEL.eval HM.empty "10 - 2 * 3"

    case (av1, av2, av3) of
        (Aeson.Number 8, Aeson.Number 5, Aeson.Number 4) -> pure ()
        all -> assertFailure $ show all

test11 :: TestTree
test11 = testCase "mixed number types" $ do
    let env = HM.fromList [("base", Aeson.Number 10)]
    -- base + 5.5 = 15.5
    av1 <- BEL.eval env "base + 5.5"
    
    case av1 of
        Aeson.Number 15.5 -> pure ()
        _ -> assertFailure $ show av1

test12 :: TestTree
test12 = testCase "today function" $ do
    -- just check it evaluates to a string
    av1 <- BEL.eval HM.empty "today()"
    case av1 of
        Aeson.String _ -> pure ()
        _ -> assertFailure $ show av1

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
