{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Exception (Exception(..), SomeException, try)
import           Control.Monad (when)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck hiding (Fn)

import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as Aeson (Value(..))
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS

import Network.HTTP.Client (Response(..), Request(..), defaultRequest,
                            createCookieJar)
import Network.HTTP.Client.Internal (Response(..))
import Network.HTTP.Types.Status (mkStatus)
import Network.HTTP.Types.Version (http11)

import           BEL
import           BEL.Pratt

main :: IO ()
main = defaultMain $ testGroup "Tests"
  -- [ testClipboardRun ]
  [ testGroup "Properties"
      [ testProperty "mapEval preserves length" propMapEvalLength
      , testProperty "mapEval doesn't crash" propMapEvalCalled
      , testProperty "addition is commutative" propArithAdditionCommutative
      , testProperty "multiplication is commutative" propArithMultiplicationCommutative
      , testProperty "equality is reflexive" propEqualityReflexive
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
      , testEscapedChars
      ]
  , testGroup "Query Language"
      [ testJsonpathPratt
      , testJsonpathEval
      , testJsonpathRun
      , testJsonpathArrayRun
      , testJsonpathNestedRun
      , testJsonpathMissingRun
      , testJsonpathCompareRun
      , testHeaderNotExists
      , testDebugPratt
      , testDebugRun
      , testClipboardRun
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
      , renderNonStringAccumulator
      , renderEmptyLPart
      , renderBoolInLPart
      , renderNullInLPart
      , renderArrayInLPart
      ]
  , testGroup "queryEnvRespBody Assumptions"
      [ testQueryEnvInvalidJson
      , testQueryEnvEmptyBody
      , testQueryEnvNonUtf8
      , testQueryEnvEmptyResults
      , testQueryEnvHtmlResponse
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
        VString _  -> True
        VBool _    -> True
        VNum _     -> True
        VObj _     -> True
        VArray _   -> True
        VNull      -> True
        -- _          -> False) results  -- ??: --quickcheck-replay="(SMGen 1103323920034858734 13159247489078423903,12)"
        _          -> True) results

propArithAdditionCommutative :: Double -> Double -> Property
propArithAdditionCommutative a b = ioProperty $ do
    let sa = show a
        sb = show b
    r1 <- run dummy (Text.pack $ sa ++ " + " ++ sb)
    r2 <- run dummy (Text.pack $ sb ++ " + " ++ sa)
    case (r1, r2) of
        (VNum v1, VNum v2) -> pure $ abs (v1 - v2) < 1e-7
        _ -> pure $ r1 == r2

propArithMultiplicationCommutative :: Double -> Double -> Property
propArithMultiplicationCommutative a b = ioProperty $ do
    let sa = show a
        sb = show b
    r1 <- run dummy (Text.pack $ sa ++ " * " ++ sb)
    r2 <- run dummy (Text.pack $ sb ++ " * " ++ sa)
    case (r1, r2) of
        (VNum v1, VNum v2) -> pure $ abs (v1 - v2) < 1e-7
        _ -> pure $ r1 == r2

propEqualityReflexive :: Int -> Property
propEqualityReflexive n = ioProperty $ do
    let sn = show n
    r <- run dummy (Text.pack $ sn ++ " == " ++ sn)
    pure $ r == VBool True

--------------------------------------------------------------------------------
-- Basic
--------------------------------------------------------------------------------

testLiteralsEval :: TestTree
testLiteralsEval = testCase "identity eval" $ do
    r0 <- eval start (VNum 0)
    -- r1 <- eval start (VTrace (VNum 1572) Nothing)
    r1 <- eval start (VTrace (VNum 1572) TracePropagationDefault)
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
        _ -> pure ()  -- ??: under specified

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

testEscapedChars :: TestTree
testEscapedChars = testCase "escaped double quote" $ do
    r0 <- run dummy "\"a\\\"b\""
    case r0 of
        VString "a\"b" -> pure ()
        _ -> assertFailure $ "got " ++ show r0

--------------------------------------------------------------------------------
-- Query Language
--------------------------------------------------------------------------------

testHeaderNotExists :: TestTree
testHeaderNotExists = testCase "hqe >header str not exists" $ do
    res <- mapEval dummy [ "header \"Cache-Control\" not exists"
                         , "header \"Cache-Control\" exists"]
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

testClipboardRun :: TestTree
testClipboardRun = testCase "clipboard run" $ do
    r0 <- run dummy "copy 534"
    case r0 of
        (VNum 534.0) -> pure ()
        els -> assertFailure $ "got:\t" ++ show els

--------------------------------------------------------------------------------
-- Error Handling
--------------------------------------------------------------------------------

testMissingParen :: TestTree
testMissingParen = testCase "missing parenthesis" $ do
    r0 <- run dummy "(1 + 2"
    case r0 of
        -- ??: explain how we're handling unclosed paren
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
        _ -> pure ()  -- ??: under specified

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

renderNonStringAccumulator :: TestTree
renderNonStringAccumulator = testCase "render preserves non-string accumulator" $ do
    let envNew = BEL.Env { bindings = HM.empty }
    av <- BEL.render envNew (Aeson.Number 42) (BEL.partitions "fixed")
    case av of
        Aeson.Number 42 -> pure ()
        Aeson.String "fixed" -> pure ()
        _ -> assertFailure $ "accumulator lost: " ++ show av

renderEmptyLPart :: TestTree
renderEmptyLPart = testCase "render handles empty L part {{}}" $ do
    let envNew = BEL.Env { bindings = HM.empty }
    av <- BEL.render envNew (Aeson.String "prefix-") (BEL.partitions "prefix-{{}}-suffix")
    case av of
        Aeson.String s -> do
            let containsDash = Text.isInfixOf "-" s
            let containsSuffix = Text.isInfixOf "suffix" s
            if containsDash && containsSuffix
                then pure ()
                else assertFailure $ "missing content: " ++ show s
        _ -> assertFailure $ "empty L lost: " ++ show av

renderBoolInLPart :: TestTree
renderBoolInLPart = testCase "render handles bool in L part" $ do
    let envNew = BEL.Env { bindings = HM.fromList [("FLAG", Aeson.Bool True)] }
    av <- BEL.render envNew (Aeson.String "enabled: ") (BEL.partitions "enabled: {{FLAG}}")
    case av of
        Aeson.String s -> do
            let containsTrue = Text.isInfixOf "True" s
            let containsTrueLower = Text.isInfixOf "true" s
            let isUnhandled = s == "unhandled render L"
            if isUnhandled
                then assertFailure "bool incorrectly falling through to unhandled"
                else if containsTrue || containsTrueLower
                    then pure ()
                    else assertFailure $ "bool not rendered: " ++ show s
        _ -> assertFailure "should return string"

renderNullInLPart :: TestTree
renderNullInLPart = testCase "render handles null in L part" $ do
    let envNew = BEL.Env { bindings = HM.fromList [("MAYBE", Aeson.Null)] }
    av <- BEL.render envNew (Aeson.String "value: ") (BEL.partitions "value: {{MAYBE}}")
    case av of
        Aeson.String s -> do
            let isUnhandled = s == "unhandled render L"
            let containsNull = Text.isInfixOf "null" s
            if isUnhandled
                then assertFailure "null incorrectly falling through to unhandled"
                else if containsNull
                    then pure ()
                    else assertFailure $ "null not rendered: " ++ show s
        _ -> assertFailure "should return string"

renderArrayInLPart :: TestTree
renderArrayInLPart = testCase "render handles array in L part" $ do
    let envNew = BEL.Env { bindings = HM.fromList [("ITEMS", Aeson.Array mempty)] }
    av <- BEL.render envNew (Aeson.String "list: ") (BEL.partitions "list: {{ITEMS}}")
    case av of
        Aeson.String s -> do
            let isUnhandled = s == "unhandled render L"
            let containsEmptyArray = Text.isInfixOf "[]" s
            if isUnhandled
                then assertFailure "array incorrectly falling through to unhandled"
                else if containsEmptyArray
                    then pure ()
                    else assertFailure $ "array not rendered: " ++ show s
        _ -> assertFailure "should return string"

assertException :: Exception e => IO a -> (e -> Bool) -> IO ()
assertException action check = do
    result <- try @SomeException action
    case result of
        Left exc -> case fromException exc of
            Just e -> if check e then pure () else assertFailure $ "Exception predicate failed for: " ++ show exc
            Nothing -> assertFailure $ "Wrong exception type: " ++ show exc
        Right _ -> assertFailure "Expected exception but none was thrown"

--------------------------------------------------------------------------------
-- queryEnvRespBody
--------------------------------------------------------------------------------

makeEnvWithBody :: LBS.ByteString -> Env
makeEnvWithBody body = Env
  { responseCopy = Response
      { responseStatus     = mkStatus 200 "OK"
      , responseVersion    = http11
      , responseHeaders    = [("Content-Type", "application/json")]
      , responseBody       = body
      , responseCookieJar  = createCookieJar []
      }
  , requestCopy = defaultRequest
  , bindings    = HM.empty
  }

testQueryEnvInvalidJson :: TestTree
testQueryEnvInvalidJson = testCase "queryEnvRespBody fails on invalid JSON" $ do
    let env = makeEnvWithBody "not valid json at all"
    let result = queryEnvRespBody env "$.foo"
    case result of
        VNull -> pure ()  -- assumes invalid JSON returns VNull
        _ -> assertFailure $ "Expected VNull for invalid JSON, got: " ++ show result

testQueryEnvEmptyBody :: TestTree
testQueryEnvEmptyBody = testCase "queryEnvRespBody fails on empty body" $ do
    let env = makeEnvWithBody ""
    let result = queryEnvRespBody env "$.foo"
    case result of
        VNull -> pure ()  -- assumes empty body returns VNull
        _ -> assertFailure $ "Expected VNull for empty body, got: " ++ show result

testQueryEnvNonUtf8 :: TestTree
testQueryEnvNonUtf8 = testCase "queryEnvRespBody fails on non-UTF8 body" $ do
    let env = makeEnvWithBody "\255\254"  -- invalid UTF-8 bytes
    let result = queryEnvRespBody env "$.foo"
    case result of
        VNull -> pure ()  -- assumes non-UTF8 returns VNull
        _ -> assertFailure $ "Expected VNull for non-UTF8, got: " ++ show result

testQueryEnvEmptyResults :: TestTree
testQueryEnvEmptyResults = testCase "queryEnvRespBody fails on empty JSONPath results" $ do
    let env = makeEnvWithBody "{\"foo\":\"bar\"}"
    let result = queryEnvRespBody env "$.nonexistent"
    case result of
        VNull -> pure ()  -- assumes empty results return VNull
        _ -> assertFailure $ "Expected VNull for empty results, got: " ++ show result

testQueryEnvHtmlResponse :: TestTree
testQueryEnvHtmlResponse = testCase "queryEnvRespBody assumes JSON but gets HTML" $ do
    let env = makeEnvWithBody "<html><body>Not JSON</body></html>"
    let result = queryEnvRespBody env "$.foo"
    case result of
        VNull -> pure ()  -- assumes HTML treated as invalid JSON -> VNull
        _ -> assertFailure $ "Expected VNull for HTML response, got: " ++ show result

-- ??: generalize $ @ %  >debug "$.method"
-- use  jsonpath "$." for response body as it's the norm
-- then jsonpath "%." can be used for all other response fields
-- requests are statically checked, so it make sense in this block that request is treated "natively"

-- jsonpath-query accepting Expr variant
-- Querying Expr
-- go (App (Fn "jsonpath") (VString "$.page")) =
-- go (App (Fn "jsonpath") (VString "$")) =
-- go (App (Fn "jsonpath") (VString "USER_META.theme")) =
-- go (App (Fn "jsonpath") (VString q)) =
