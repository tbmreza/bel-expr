{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception (Exception(..), SomeException, try)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fn)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.Text as Text

import           BEL
import           BEL.Pratt

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Properties"
      [ testProperty "mapEval preserves length" propMapEvalLength
      , testProperty "mapEval doesn't crash" propMapEvalCalled
      ]
  , testGroup "Basic"
      [ testLiteralsEval
      , testLiteralsRun
      , testArithPratt
      , testUnaryMinus
      , testDivByZero
      , testArithMismatch
      , testIdentRun
      , testIdentEval
      , testIdentMissing
      , testTokensKeywords
      ]
  , testGroup "Query Language"
      [ testJsonpathPratt
      , testJsonpathEval
      , testJsonpathRun
      , testJsonpathArrayRun
      , testJsonpathNestedRun
      , testJsonpathMissingRun
      , testJsonpathCompareRun
      , testHeadersNotExists
      , testDebugPratt
      , testDebugRun
      ]
  , testGroup "Error Handling"
      [ testMissingParen
      , testMalformedJsonpath
      , testJsonpathNonObject
      , testNegString
      , testIncompatibleEq
      , testEmptyInput
      ]
  , testGroup "API"
      [ apiMapEval
      , apiRender
      ]
  ]


start :: Env
start = Env { bindings = HM.empty }

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

propMapEvalLength :: [String] -> Property
propMapEvalLength inputs = ioProperty $ do
    let textInputs = map (Text.pack . (\s -> if null s then "0" else s)) inputs
    results <- mapEval dummy textInputs
    pure $ length results == length inputs

propMapEvalCalled :: NonEmptyList String -> Property
propMapEvalCalled (NonEmpty inputs) = ioProperty $ do
    let textInputs = map (Text.pack . (\s -> if null s then "0" else s)) inputs
    results <- mapEval dummy textInputs
    pure $ not (null results) && all (\r -> case r of
        VNum _     -> True
        VString _  -> True
        VBool _    -> True
        VNull      -> True
        _          -> True) results

--------------------------------------------------------------------------------
-- Basic
--------------------------------------------------------------------------------

testLiteralsEval :: TestTree
testLiteralsEval = testCase "identity eval" $ do
    r0 <- eval start (VNum 0)
    r1 <- eval start (VTrace (VNum 1572) Nothing)
    case (r0, r1) of
        (VNum 0, VNum 1572) -> pure ()

testLiteralsRun :: TestTree
testLiteralsRun = testCase "identity run" $ do
    r0 <- run start "0"
    r1 <- run start "1572"
    case (r0, r1) of
        (VNum 0, VNum 1572) -> pure ()

testArithPratt :: TestTree
testArithPratt = testCase "arith pratt" $ do
    case pratt 0 [TNum 421] of
        (VNum 421, []) -> pure ()

    case pratt 0 [TNum 11, TPlus, TNum 22] of
        (EAdd (VNum 11.0) (VNum 22.0), []) -> pure ()

    case pratt 100 [TNum 11, TPlus, TNum 22] of
        (VNum 11.0, [TPlus, TNum 22]) -> pure ()

testUnaryMinus :: TestTree
testUnaryMinus = testCase "unary minus" $ do
    r1 <- run dummy "-2"
    r2 <- run dummy "5 - -2"

    case (r1, r2) of
        (VNum (-2.0), VNum 7.0) -> pure ()

testDivByZero :: TestTree
testDivByZero = testCase "division by zero" $
    assertException (run dummy "1 / 0") (\(_ :: SomeException) -> True)

testArithMismatch :: TestTree
testArithMismatch = testCase "arithmetic mismatch" $ do
    r0 <- run dummy "\"a\" + 1"
    case r0 of
        VNum _ -> assertFailure "Should not be a number"
        EAdd _ _ -> assertFailure "Should be reduced or handle error"
        _ -> pure ()

testIdentRun :: TestTree
testIdentRun = testCase "ident run" $ do
    r0 <- run dummy "BASE_URL"
    case r0 of
        VString "https://api.example.com" -> pure ()

testIdentEval :: TestTree
testIdentEval = testCase "ident eval" $ do
    r0 <- eval dummy (VIdent "MAX_RETRIES")
    case r0 of
        VNum 3.0 -> pure ()

testIdentMissing :: TestTree
testIdentMissing = testCase "ident missing" $ do
    r0 <- run dummy "UNKNOWN_VAR"
    case r0 of
        VNull -> pure ()

testTokensKeywords :: TestTree
testTokensKeywords = testCase "tokens keywords no overlap" $ do
    r1 <- run dummy "debug_var"
    r2 <- run dummy "falsehood"
    r3 <- run dummy "jsonpath_1"

    case (r1, r2, r3) of
        (VNull, VNull, VNull) -> pure ()

--------------------------------------------------------------------------------
-- Query Language
--------------------------------------------------------------------------------

testHeadersNotExists :: TestTree
testHeadersNotExists = testCase "hqe >headers str not exists" $ do
    res <- mapEval dummy [ "headers \"Cache-Control\" not exists"
                         , "headers \"Cache-Control\" exists"]
    case res of
        [VBool False, VBool True] -> pure ()
        els -> assertFailure $ "got:\t" ++ show els

testJsonpathPratt :: TestTree
testJsonpathPratt = testCase "jsonpath pratt" $ do
    case pratt 0 [TJsonpath, TQuoted "$.data.page", TEq, TNum 1] of
        (EEq (EJsonpath (VString "$.data.page")) (VNum 1.0), []) -> pure ()

testJsonpathEval :: TestTree
testJsonpathEval = testCase "jsonpath eval" $ do
    r0 <- eval dummy (EEq (EJsonpath (VString "$.page")) (VNum 1.0))
    case r0 of
        (VBool True) -> pure ()

testJsonpathRun :: TestTree
testJsonpathRun = testCase "jsonpath run" $ do
    r0 <- run dummy "jsonpath \"$.page\""
    case r0 of
        (VNum 1.0) -> pure ()

testJsonpathArrayRun :: TestTree
testJsonpathArrayRun = testCase "jsonpath array run" $ do
    r0 <- run dummy "jsonpath \"$.roles[0]\""
    r1 <- run dummy "jsonpath \"$.roles[1]\""
    case (r0, r1) of
        (VString "admin", VString "editor") -> pure ()
        _ -> assertFailure $ "got " ++ show (r0, r1)

testJsonpathNestedRun :: TestTree
testJsonpathNestedRun = testCase "jsonpath nested run" $ do
    r0 <- run dummy "jsonpath \"$.meta.theme\""
    case r0 of
        (VString "dark") -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testJsonpathMissingRun :: TestTree
testJsonpathMissingRun = testCase "jsonpath missing run" $ do
    r0 <- run dummy "jsonpath \"$.missing\""
    case r0 of
        VNull -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testJsonpathCompareRun :: TestTree
testJsonpathCompareRun = testCase "jsonpath compare run" $ do
    r0 <- run dummy "jsonpath \"$.userId\" == 42"
    r1 <- run dummy "jsonpath \"$.name\" == \"Alice\""
    case (r0, r1) of
        (VBool True, VBool True) -> pure ()
        _ -> assertFailure $ "got " ++ show (r0, r1)

testDebugPratt :: TestTree
testDebugPratt = testCase "debug pratt" $ do
    case pratt 0 [TDebug, TNum 13] of
        (EDebug (VNum 13.0), []) -> pure ()

testDebugRun :: TestTree
testDebugRun = testCase "debug run" $ do
    r0 <- run dummy "debug 534"
    case r0 of
        (VBool True) -> pure ()

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

testMissingParen :: TestTree
testMissingParen = testCase "missing parenthesis" $ do
    r0 <- run dummy "(1 + 2"
    case r0 of
        VNum 3.0 -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testMalformedJsonpath :: TestTree
testMalformedJsonpath = testCase "malformed jsonpath" $ do
    r0 <- run dummy "jsonpath \"$.[\""
    case r0 of
        VNull -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testJsonpathNonObject :: TestTree
testJsonpathNonObject = testCase "jsonpath on non-object" $ do
    r0 <- run dummy "jsonpath \"$.page.foo\""
    case r0 of
        VNull -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testNegString :: TestTree
testNegString = testCase "negate string" $ do
    r0 <- run dummy "-\"hello\""
    case r0 of
        ENeg (VString "hello") -> assertFailure "Should probably be an error or VNull"
        _ -> pure ()

testIncompatibleEq :: TestTree
testIncompatibleEq = testCase "incompatible equality" $ do
    r0 <- run dummy "1 == \"1\""
    case r0 of
        VBool False -> pure ()
        _ -> assertFailure $ "got " ++ show r0

testEmptyInput :: TestTree
testEmptyInput = testCase "empty input" $ do
    r0 <- run dummy ""
    case r0 of
        VString "" -> pure ()
        _ -> assertFailure $ "got " ++ show r0

--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------

apiMapEval :: TestTree
apiMapEval = testCase "mapEval" $ do
    res <- mapEval dummy ["1 + 2", "10 - 5", "invalid syntax!"]
    case res of
        [VNum 3.0, VNum 5.0, VString "invalid syntax!"] -> pure ()
        _ -> assertFailure $ "mapEval got: " ++ show res

apiRender :: TestTree
apiRender = testCase "render template parts" $ do
    let envNew = BEL.Env { bindings = HM.fromList [("CAT", Aeson.String "animals")] }

    av1 <- BEL.render envNew (Aeson.String "") (BEL.partitions "{{10 * 2}}")
    av2 <- BEL.render envNew (Aeson.String "") (BEL.partitions "score {{100}}")
    av3 <- BEL.render envNew (Aeson.String "") (BEL.partitions "https://kernel.org/{{CAT}}/route.php?prefilt=9&lim={{10}}&filt=another")

    case (av1, av2, av3) of
        (Aeson.String "20", Aeson.String "score 100", Aeson.String "https://kernel.org/animals/route.php?prefilt=9&lim=10&filt=another") -> pure ()
        all -> assertFailure $ show all

--------------------------------------------------------------------------------
-- More lib than app code
--------------------------------------------------------------------------------

assertException :: Exception e => IO a -> (e -> Bool) -> IO ()
assertException action check = do
    result <- try @SomeException action
    case result of
        Left exc -> case fromException exc of
            Just e -> if check e then pure () else assertFailure $ "Exception predicate failed for: " ++ show exc
            Nothing -> assertFailure $ "Wrong exception type: " ++ show exc
        Right _ -> assertFailure "Expected exception but none was thrown"
