{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import Debug.Trace

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fn)
import           Test.QuickCheck hiding (Fn)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as Aeson (Value(..))
import           Data.Aeson.QQ.Simple (aesonQQ)
import           Data.Text (Text)
import qualified Data.Text as Text

import           BEL

-- ??: don define a new callsite in tests code
evalValue :: BEL.Env -> Text -> IO Aeson.Value
evalValue env input = do
    e <- BEL.eval env input
    pure $ BEL.finalValue env e

main :: IO ()
main = defaultMain $ testGroup "Tests"
  -- [ test7 ]

  [ testGroup "Examples"   [ test0
                           , test1 , test2 , test3 , test4 , test5 , test6
                           , test7 , test8 , test9 , test10 , test11 , test12
                           , test13 , test14 , test15 , testAmbiguity
                           -- , testDebug
                           ]
  -- , testGroup "Properties" [ testProperty "eval doesn't crash" prop_eval_doesnt_crash
  --                          , testProperty "render doesn't crash" prop_render_doesnt_crash
  --                          , testProperty "render identity simple" prop_render_identity_simple
  --                          ]
  ]

prop_eval_doesnt_crash :: String -> Property
prop_eval_doesnt_crash input = ioProperty $ do
    _ <- BEL.eval HM.empty (Text.pack input)
    pure True

prop_render_doesnt_crash :: String -> Property
prop_render_doesnt_crash input = ioProperty $ do
    let parts = BEL.partitions (Text.pack input)
    val <- BEL.render HM.empty (Aeson.String "") parts
    case val of
        Aeson.String _ -> pure True
        _ -> pure False

prop_render_identity_simple :: Property
prop_render_identity_simple = forAll (arbitrary `suchThat` isSimple) $ \input -> ioProperty $ do
    let txt = Text.pack input
    let parts = BEL.partitions txt
    val <- BEL.render HM.empty (Aeson.String "") parts
    pure $ val == Aeson.String txt
  where
    isSimple s = not $ any (\c -> c `elem` ("{}\\"::String)) s

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

    -- nama == "contents"
    e <- BEL.toExpr envNew [TIdentifier "nama", TEq, TQuoted "contents"]

    -- "wrong answer" == "wrong"
    f <- BEL.toExpr envNew [TQuoted "wrong answer", TEq, TQuoted "wrong"]

    -- today()
    g <- BEL.toExpr envNew [TIdentifier "today", TParenOpn, TParenCls]

    case (e, f, g) of
        (VBool True, VBool False, VString resG) -> trace ("today=" ++ show resG) $ pure ()
        els -> assertFailure $ show els

test8 :: TestTree
test8 = testCase "jsonpath invocation" $ do
    let root :: Aeson.Value = [aesonQQ| { "data": { "unchecked": 2005 } } |]
        envNew :: BEL.Env = HM.fromList [("year", Aeson.String "2025"), ("RESP_BODY", root)]

    -- jsonpath "$.data.unchecked" == 2005
    let prog1 = Eq (App (Fn "jsonpath") (VString "$.data.unchecked")) (VNum 2005)
        prog2 = App (Fn "jsonpath") (VString "$.data.unchecked")

    case (match envNew prog1, match envNew prog2) of
        (VBool True, VNum 2005) -> pure ()
        all -> assertFailure $ show all

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

test14 :: TestTree
test14 = testCase "object equality" $ do
    let obj :: Aeson.Value = [aesonQQ| { "a": 1 } |]
        env = HM.fromList [("o1", obj), ("o2", obj)]

    -- "o1 == o2" should be true
    res <- BEL.eval env "o1 == o2"
    case res of
        VBool True -> pure ()
        _ -> assertFailure $ "Expected True, got: " ++ show res

-- (auto)
test15 :: TestTree
test15 = testCase "array and null values" $ do
    let arr :: Aeson.Value = [aesonQQ| [1, 2, 3] |]
        nul :: Aeson.Value = Aeson.Null
        env = HM.fromList [("a", arr), ("n", nul), ("a2", arr), ("n2", nul)]

    -- Check equality
    res1 <- BEL.eval env "a == a2"
    res2 <- BEL.eval env "n == n2"
    res3 <- BEL.eval env "a != n"
    
    -- Check value retrieval
    valA <- evalValue env "a"
    valN <- evalValue env "n"

    case (res1, res2, res3, valA, valN) of
        (VBool True, VBool True, VBool True, vArr, vNull) 
            | vArr == arr && vNull == nul -> pure ()
        results -> assertFailure $ "Failed array/null test: " ++ show results

-- (auto)
testAmbiguity :: TestTree
testAmbiguity = testCase "ambiguity between variables and strings" $ do
    -- 'foo' is not in env, so 'foo' evaluates to VString "foo".
    -- This test documents the CURRENT behavior.
    let envEmpty = HM.empty
    res1 <- BEL.eval envEmpty "foo"
    
    case res1 of
        VString "foo" -> pure () 
        _ -> assertFailure $ "Expected VString \"foo\" (current behavior), got: " ++ show res1

-- (auto)
-- testDebug :: TestTree
-- testDebug = testCase "debug side effect" $ do
--     -- should print (side effect) and return true.
--     resNum <- BEL.eval HM.empty "debug 5"
--     resBool <- BEL.eval HM.empty "debug true"
--     
--     case (resNum, resBool) of
--         -- (VNum 5, VBool True) -> pure ()
--         (VBool True, VBool True) -> pure ()
--         (n, b) -> assertFailure $ "Debug did not pass through values. Got: " ++ show n ++ ", " ++ show b
