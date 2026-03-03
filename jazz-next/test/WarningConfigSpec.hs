module Main (main) where

import JazzNext.Compiler.WarningConfig
  ( WarningDirective (..),
    WarningSettings,
    isWarningEnabled,
    isWarningError,
    parseCliWarningDirective,
    resolveWarningSettings
  )
import JazzNext.Compiler.Warnings
  ( WarningCategory (..),
    parseWarningCategory
  )
import JazzNext.TestHarness
  ( NamedTest,
    assertEqual,
    assertLeftContains,
    assertRight,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "WarningConfig" tests

tests :: [NamedTest]
tests =
  [ ("parseWarningCategory accepts known category", testParseWarningCategoryKnown),
    ("parseWarningCategory rejects unknown category", testParseWarningCategoryUnknown),
    ("parseCliWarningDirective parses all phase-1 forms", testParseCliWarningDirectiveForms),
    ("resolveWarningSettings handles standalone -Werror=<category>", testCliPromoteCategoryStandalone),
    ("resolveWarningSettings promotes enabled warnings via -Werror", testPromoteAllEnabledToError),
    ("resolveWarningSettings applies CLI > env > config > default", testPrecedenceOrder),
    ("resolveWarningSettings applies env error directives after env warning directives", testEnvErrorOverridesEnvWarning),
    ("resolveWarningSettings defaults to all warnings disabled", testDefaultDisabled),
    ("resolveWarningSettings rejects malformed env warning token list", testMalformedEnvWarningTokenList),
    ("resolveWarningSettings fails on unknown CLI category", testUnknownCliCategory),
    ("resolveWarningSettings fails on unknown env category", testUnknownEnvCategory),
    ("resolveWarningSettings fails on unknown config category", testUnknownConfigCategory)
  ]

testParseWarningCategoryKnown :: IO ()
testParseWarningCategoryKnown =
  assertEqual
    "same-scope-rebinding"
    (Right SameScopeRebinding)
    (parseWarningCategory "same-scope-rebinding")

testParseWarningCategoryUnknown :: IO ()
testParseWarningCategoryUnknown =
  assertLeftContains "unknown category" "unknown warning category" (parseWarningCategory "nope")

testParseCliWarningDirectiveForms :: IO ()
testParseCliWarningDirectiveForms = do
  assertEqual
    "-Wsame-scope-rebinding"
    (Right (EnableCategory SameScopeRebinding))
    (parseCliWarningDirective "-Wsame-scope-rebinding")
  assertEqual
    "-Wno-same-scope-rebinding"
    (Right (DisableCategory SameScopeRebinding))
    (parseCliWarningDirective "-Wno-same-scope-rebinding")
  assertEqual
    "-Werror=same-scope-rebinding"
    (Right (PromoteCategoryToError SameScopeRebinding))
    (parseCliWarningDirective "-Werror=same-scope-rebinding")
  assertEqual
    "-Werror"
    (Right PromoteAllEnabledToError)
    (parseCliWarningDirective "-Werror")
  assertEqual
    "-Wnone"
    (Right DisableAllCategories)
    (parseCliWarningDirective "-Wnone")

testPrecedenceOrder :: IO ()
testPrecedenceOrder =
  assertRight
    "resolve precedence"
    ( resolveWarningSettings
        ["-Wsame-scope-rebinding"]
        (Just "-same-scope-rebinding")
        Nothing
        (Just "same-scope-rebinding")
    )
    (\settings -> assertWarningState settings True False)

testCliPromoteCategoryStandalone :: IO ()
testCliPromoteCategoryStandalone =
  assertRight
    "standalone -Werror promotion"
    (resolveWarningSettings ["-Werror=same-scope-rebinding"] Nothing Nothing Nothing)
    (\settings -> do
        assertWarningState settings True True
        assertCategoryState settings ShadowingOuterScope False False
        assertCategoryState settings UnusedBinding False False
        assertCategoryState settings DeprecatedSyntax False False
    )

testPromoteAllEnabledToError :: IO ()
testPromoteAllEnabledToError =
  assertRight
    "-Werror promotes enabled warnings"
    (resolveWarningSettings ["-Wsame-scope-rebinding", "-Werror"] Nothing Nothing Nothing)
    (\settings -> assertWarningState settings True True)

testEnvErrorOverridesEnvWarning :: IO ()
testEnvErrorOverridesEnvWarning =
  assertRight
    "env error overrides env warning for same category"
    ( resolveWarningSettings
        []
        (Just "-same-scope-rebinding")
        (Just "same-scope-rebinding")
        Nothing
    )
    (\settings -> assertWarningState settings True True)

testDefaultDisabled :: IO ()
testDefaultDisabled =
  assertRight
    "default config"
    (resolveWarningSettings [] Nothing Nothing Nothing)
    (\settings -> assertWarningState settings False False)

testMalformedEnvWarningTokenList :: IO ()
testMalformedEnvWarningTokenList =
  assertLeftContains
    "malformed env warning token list"
    "empty warning token"
    (resolveWarningSettings [] (Just "same-scope-rebinding,,unused-binding") Nothing Nothing)

testUnknownCliCategory :: IO ()
testUnknownCliCategory =
  assertLeftContains
    "unknown cli category"
    "unknown warning category"
    (resolveWarningSettings ["-Wnot-real"] Nothing Nothing Nothing)

testUnknownEnvCategory :: IO ()
testUnknownEnvCategory =
  assertLeftContains
    "unknown env category"
    "unknown warning category"
    (resolveWarningSettings [] (Just "not-real") Nothing Nothing)

testUnknownConfigCategory :: IO ()
testUnknownConfigCategory =
  assertLeftContains
    "unknown config category"
    "unknown warning category"
    (resolveWarningSettings [] Nothing Nothing (Just "not-real\n"))

assertWarningState :: WarningSettings -> Bool -> Bool -> IO ()
assertWarningState settings expectedEnabled expectedError = do
  assertEqual
    "enabled state"
    expectedEnabled
    (isWarningEnabled settings SameScopeRebinding)
  assertEqual
    "error state"
    expectedError
    (isWarningError settings SameScopeRebinding)

assertCategoryState :: WarningSettings -> WarningCategory -> Bool -> Bool -> IO ()
assertCategoryState settings category expectedEnabled expectedError = do
  assertEqual
    ("enabled state for " ++ show category)
    expectedEnabled
    (isWarningEnabled settings category)
  assertEqual
    ("error state for " ++ show category)
    expectedError
    (isWarningError settings category)
