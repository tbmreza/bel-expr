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
import Data.Text (Text)

-- ??: don define a new callsite in tests code
evalValue :: BEL.Env -> Text -> IO Aeson.Value
evalValue env input = do
    e <- BEL.eval env input
    pure $ BEL.finalValue env e

main :: IO ()
main = defaultMain $ testGroup "Happy tests"
    -- [ test13 ]

  [ test0
  , test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  ]

test0 :: TestTree
test0 = testCase "part kinds" $ do
    case BEL.partitions "key: {{jsonpath \"$.data.token\"}}," of
        [BEL.R "key: ", BEL.L "jsonpath \"$.data.token\"", BEL.R ","] -> pure ()
        all -> assertFailure $ show all

test1 :: TestTree
test1 = testCase "partition sentence" $ do
    case BEL.partitions "been up for {{dur}} minutes." of
        [BEL.R "been up for ", BEL.L "dur", BEL.R " minutes."] -> pure ()
        all -> assertFailure $ show all

test2 :: TestTree
test2 = testCase "render template parts" $ do
    let envNew :: BEL.Env = HM.fromList [("CAT", Aeson.String "animals")]

    av1 <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{10 * 2}}")
    av2 <- BEL.render envNew (Aeson.String "") (BEL.partitions "score {{100}}")
    av3 <- BEL.render envNew (Aeson.String "") (BEL.partitions "https://kernel.org/{{CAT}}/route.php?prefilt=9&lim={{10}}&filt=another")

    case (av1, av2, av3) of
        (Aeson.String "20", Aeson.String "score 100", Aeson.String "https://kernel.org/animals/route.php?prefilt=9&lim=10&filt=another") -> pure ()
        all -> assertFailure $ show all

test3 :: TestTree
test3 = testCase "arith basic" $ do
    let envNew :: BEL.Env = HM.fromList [("margin", Aeson.Number 3)]

    av1 <- evalValue envNew "2 * 1000"
    av2 <- evalValue envNew "47 + margin"

    case (av1, av2) of
        (Aeson.Number 2000, Aeson.Number 50) -> pure ()
        all -> assertFailure $ show all

test4 :: TestTree
test4 = testCase "arith nest" $ do
    let av0 = finalMatch $ Add (VNum 0) (Mul (VNum 3) (VNum 3))
        av1 = finalMatch $ Add (VNum 3) (Mul (VNum 50) (VNum 2))

    case (av0, av1) of
        (Aeson.Number 9, Aeson.Number 103) -> pure ()
        all -> assertFailure $ show all

    where
    finalMatch :: Expr -> Aeson.Value
    finalMatch prog = finalValue envNew $ match envNew prog

    envNew :: BEL.Env
    envNew = HM.fromList []

test5 :: TestTree
test5 = testCase "bool literals" $ do
    vTrue <-  BEL.eval HM.empty "true"
    vFalse <- BEL.eval HM.empty "false"

    case (vTrue, vFalse) of
        (VBool True, VBool False) -> pure ()
        all -> assertFailure $ show all

test6 :: TestTree
test6 = testCase "assertion line" $ do
    av0 <-  BEL.eval HM.empty "200 !=  200"
    av1 <-  BEL.eval HM.empty "14 ==  14"

    case (av0, av1) of
        (VBool False, VBool True) -> pure ()
        all -> assertFailure $ show all

test7 :: TestTree
test7 = testCase "toExpr unit" $ do
    let envNew = HM.fromList [("nama", Aeson.String "contents")]
    e <- BEL.toExpr envNew [TIdentifier "nama", TEq, TQuoted "contents"]
    f <- BEL.toExpr envNew [TQuoted "wrong answer", TEq, TQuoted "wrong"]
    case (e, f) of
        (VBool True, VBool False) -> pure ()
        els -> assertFailure $ show els

-- ??: test aesonQQ json bool literals test? = testCase "jsonpath invocation evals a bool" $ do
test8 :: TestTree
test8 = testCase "jsonpath invocation" $ do
    let root :: Aeson.Value = [aesonQQ| { "data": { "unchecked": 2005 } } |]
    let envNew :: BEL.Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]

    -- jsonpath "$.data.unchecked" == 2005
    let prog1 = Eq (App (Fn "jsonpath") (VString "$.data.unchecked")) (VNum 2005)
    let prog2 = App (Fn "jsonpath") (VString "$.data.unchecked")

    case (match envNew prog1, match envNew prog2) of
        (VBool False, VNum 2005) -> pure ()  -- ?? bool sus
        all -> assertFailure $ show all

-- ?? chance it's stack overflow
test9 :: TestTree
test9 = testCase "arith precedence" $ do
    -- 3 + 4 * 5 = 23
    av1 <- evalValue HM.empty "3 + 4 * 5"
    -- 4 * 5 + 3 = 23
    av2 <- evalValue HM.empty "4 * 5 + 3"

    case (av1, av2) of
        (Aeson.Number 23, Aeson.Number 23) -> pure ()
        all -> assertFailure $ show all

test10 :: TestTree
test10 = testCase "arith sub div" $ do
    -- 10 - 2 = 8
    av1 <- evalValue HM.empty "10 - 2"
    -- 20 / 4 = 5
    av2 <- evalValue HM.empty "20 / 4"
    -- 10 - 2 * 3 = 4
    av3 <- evalValue HM.empty "10 - 2 * 3"

    case (av1, av2, av3) of
        (Aeson.Number 8, Aeson.Number 5, Aeson.Number 4) -> pure ()
        all -> assertFailure $ show all

test11 :: TestTree
test11 = testCase "mixed number types" $ do
    let env = HM.fromList [("base", Aeson.Number 10)]
    -- base + 5.5 = 15.5
    av1 <- evalValue env "base + 5.5"
    
    case av1 of
        Aeson.Number 15.5 -> pure ()
        _ -> assertFailure $ show av1

test12 :: TestTree
test12 = testCase "today function" $ do
    -- just check it evaluates to a string
    av1 <- evalValue HM.empty "today()"
    case av1 of
        Aeson.String _ -> pure ()
        _ -> assertFailure $ show av1

test13 :: TestTree
test13 = testCase "curly braces behavior" $ do
    let p1 = BEL.partitions "foo { bar"
    let p2 = BEL.partitions "foo \\{ bar"

    let expected = [BEL.R "foo ", BEL.R "{", BEL.R " bar"]

    case (p1 == expected, p2 == expected) of
        (True, True) -> pure ()
        all -> assertFailure $ show all


-- main :: IO ()
-- main = do
--     case parseFloat "-04.14009" of
--         Left _ -> putStrLn "left"
--         Right d -> putStrLn $ show d
