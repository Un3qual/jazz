{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import JazzNext.Compiler.Bootstrap.CanonicalLoweredIRComparison
  ( canonicalLoweredProgramRuntimeValue,
    canonicalLoweredProgramsRuntimeValue
  )
import JazzNext.Compiler.LoweredIR
import JazzNext.Compiler.Runtime (renderRuntimeValue)
import JazzNext.TestHarness
  ( NamedTest,
    assertContains,
    assertEqual,
    runTestSuite
  )

main :: IO ()
main = runTestSuite "JazzLoweredIRContract" tests

tests :: [NamedTest]
tests =
  [ ("audits the fixed valid fixture manifest", testValidFixtureManifest),
    ("renders the scalar contract deterministically", testScalarContractRendering),
    ("renders the complete valid contract deterministically", testValidContractRendering)
  ]

testValidFixtureManifest :: IO ()
testValidFixtureManifest = do
  assertEqual "valid fixture names" expectedValidFixtureNames (map validFixtureName validFixtures)
  assertEqual "valid fixture count" 10 (length validFixtures)

testValidContractRendering :: IO ()
testValidContractRendering = do
  let programs = map validFixtureProgram validFixtures
      first = renderRuntimeValue (canonicalLoweredProgramsRuntimeValue programs)
      second = renderRuntimeValue (canonicalLoweredProgramsRuntimeValue programs)
  mapM_ (\constructorName -> assertContains constructorName constructorName first) validConstructorInventory
  assertEqual "valid batch deterministic rendering" first second

testScalarContractRendering :: IO ()
testScalarContractRendering = do
  let first = renderRuntimeValue (canonicalLoweredProgramRuntimeValue minimalScalarProgram)
      second = renderRuntimeValue (canonicalLoweredProgramRuntimeValue minimalScalarProgram)
  assertEqual "scalar canonical rendering" expectedScalarRendering first
  assertEqual "scalar deterministic rendering" first second

scalarProgram :: LoweredProgram
scalarProgram =
  program
    []
    []
    ( scalarFunction "main" LoweredUnitRepresentation LoweredUnitImmediate
        : [ scalarFunction "bool" LoweredBoolRepresentation (LoweredBoolImmediate True),
            scalarFunction "i8" (signed LoweredIntegerWidth8) (LoweredSignedIntegerImmediate LoweredIntegerWidth8 (-8)),
            scalarFunction "i16" (signed LoweredIntegerWidth16) (LoweredSignedIntegerImmediate LoweredIntegerWidth16 (-16)),
            scalarFunction "i32" (signed LoweredIntegerWidth32) (LoweredSignedIntegerImmediate LoweredIntegerWidth32 (-32)),
            scalarFunction "i64" (signed LoweredIntegerWidth64) (LoweredSignedIntegerImmediate LoweredIntegerWidth64 (-64)),
            scalarFunction "u8" (unsigned LoweredIntegerWidth8) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth8 8),
            scalarFunction "u16" (unsigned LoweredIntegerWidth16) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth16 16),
            scalarFunction "u32" (unsigned LoweredIntegerWidth32) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth32 32),
            scalarFunction "u64" (unsigned LoweredIntegerWidth64) (LoweredUnsignedIntegerImmediate LoweredIntegerWidth64 64),
            scalarFunction "f16" (float LoweredFloatWidth16) (LoweredFloatImmediate LoweredFloatWidth16 "1.5"),
            scalarFunction "f32" (float LoweredFloatWidth32) (LoweredFloatImmediate LoweredFloatWidth32 "2.5"),
            scalarFunction "f64" (float LoweredFloatWidth64) (LoweredFloatImmediate LoweredFloatWidth64 "3.5"),
            scalarFunction "char" LoweredCharRepresentation (LoweredCharImmediate 'λ')
          ]
    )
    "main"

minimalScalarProgram :: LoweredProgram
minimalScalarProgram =
  program
    []
    []
    [scalarFunction "main" LoweredUnitRepresentation LoweredUnitImmediate]
    "main"

branchJoinProgram :: LoweredProgram
branchJoinProgram =
  program
    []
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            []
            ( LoweredBranch
                (immediate (LoweredBoolImmediate True))
                (blockId "left")
                []
                (blockId "right")
                []
            ),
          block "left" [] [] (LoweredJump (blockId "join") [int64 1]),
          block "right" [] [] (LoweredJump (blockId "join") [int64 2]),
          block
            "join"
            [parameter "value" i64]
            []
            (LoweredReturn (blockParameter "value" i64))
        ]
        "entry"
    ]
    "main"

productProjectionProgram :: LoweredProgram
productProjectionProgram =
  program
    [LoweredLayout (layoutId "pair") (LoweredProductLayout [i64, LoweredBoolRepresentation])]
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            [ instruction "pair" (managed "pair") (LoweredConstructProduct (layoutId "pair") [int64 7, immediate (LoweredBoolImmediate True)]),
              instruction "value" i64 (LoweredProjectField (layoutId "pair") 0 (temporary "pair" (managed "pair")))
            ]
            (LoweredReturn (temporary "value" i64))
        ]
        "entry"
    ]
    "main"

variantSwitchProgram :: LoweredProgram
variantSwitchProgram =
  program
    [ LoweredLayout
        (layoutId "option")
        (LoweredVariantLayouts [LoweredVariantLayout 0 [], LoweredVariantLayout 1 [i64]])
    ]
    []
    [ function
        "main"
        Nothing
        []
        i64
        [ block
            "entry"
            []
            [instruction "option" (managed "option") (LoweredConstructVariant (layoutId "option") 1 [int64 9])]
            ( LoweredSwitch
                (temporary "option" (managed "option"))
                [ LoweredSwitchCase 0 (blockId "none") [],
                  LoweredSwitchCase 1 (blockId "some") [temporary "option" (managed "option")]
                ]
                (Just (LoweredSwitchDefault (blockId "default") []))
            ),
          block "none" [] [] (LoweredReturn (int64 0)),
          block
            "some"
            [parameter "option" (managed "option")]
            [ instruction
                "payload"
                i64
                (LoweredProjectVariantField (layoutId "option") 1 0 (blockParameter "option" (managed "option")))
            ]
            (LoweredReturn (temporary "payload" i64)),
          block "default" [] [] (LoweredReturn (int64 (-1)))
        ]
        "entry"
    ]
    "main"

directCallProgram :: LoweredProgram
directCallProgram =
  program [] [] [identityFunction, directCaller LoweredDirectCall LoweredReturn] "main"

directTailCallProgram :: LoweredProgram
directTailCallProgram =
  program
    []
    []
    [ identityFunction,
      function "main" Nothing [] i64 [block "entry" [] [] (LoweredDirectTailCall (functionId "identity") [int64 12])] "entry"
    ]
    "main"

closureCallProgram :: LoweredProgram
closureCallProgram = closureProgram False

closureTailCallProgram :: LoweredProgram
closureTailCallProgram = closureProgram True

runtimeServiceCallProgram :: LoweredProgram
runtimeServiceCallProgram =
  program
    [LoweredLayout (layoutId "text") LoweredTextLayout]
    [LoweredRuntimeService (serviceId "write-text") (LoweredCallSignature [managed "text"] LoweredUnitRepresentation)]
    [ function
        "main"
        Nothing
        []
        LoweredUnitRepresentation
        [ block
            "entry"
            []
            [ instruction "text" (managed "text") (LoweredConstructText (layoutId "text") "hello"),
              instruction "result" LoweredUnitRepresentation (LoweredRuntimeCall (serviceId "write-text") [temporary "text" (managed "text")])
            ]
            (LoweredReturn (temporary "result" LoweredUnitRepresentation))
        ]
        "entry"
    ]
    "main"

textListLayoutsProgram :: LoweredProgram
textListLayoutsProgram =
  program
    [ LoweredLayout (layoutId "text") LoweredTextLayout,
      LoweredLayout (layoutId "texts") (LoweredListLayout (managed "text"))
    ]
    []
    [ function
        "main"
        Nothing
        []
        (managed "texts")
        [ block
            "entry"
            []
            [ instruction "text" (managed "text") (LoweredConstructText (layoutId "text") "item"),
              instruction "texts" (managed "texts") (LoweredConstructList (layoutId "texts") [temporary "text" (managed "text")])
            ]
            (LoweredReturn (temporary "texts" (managed "texts")))
        ]
        "entry"
    ]
    "main"

data ValidFixture = ValidFixture
  { validFixtureName :: Text,
    validFixtureProgram :: LoweredProgram
  }

validFixtures :: [ValidFixture]
validFixtures =
  [ ValidFixture "scalar-representations" scalarProgram,
    ValidFixture "branch-join" branchJoinProgram,
    ValidFixture "product-projection" productProjectionProgram,
    ValidFixture "variant-switch" variantSwitchProgram,
    ValidFixture "direct-call" directCallProgram,
    ValidFixture "direct-tail-call" directTailCallProgram,
    ValidFixture "closure-call" closureCallProgram,
    ValidFixture "closure-tail-call" closureTailCallProgram,
    ValidFixture "runtime-service-call" runtimeServiceCallProgram,
    ValidFixture "text-list-layouts" textListLayoutsProgram
  ]

expectedValidFixtureNames :: [Text]
expectedValidFixtureNames =
  [ "scalar-representations",
    "branch-join",
    "product-projection",
    "variant-switch",
    "direct-call",
    "direct-tail-call",
    "closure-call",
    "closure-tail-call",
    "runtime-service-call",
    "text-list-layouts"
  ]

validConstructorInventory :: [Text]
validConstructorInventory =
  [ "LoweredProductLayout",
    "LoweredVariantLayouts",
    "LoweredClosureEnvironmentLayout",
    "LoweredTextLayout",
    "LoweredListLayout",
    "LoweredFunctionParameterOperand",
    "LoweredBlockParameterOperand",
    "LoweredTemporaryOperand",
    "LoweredImmediateOperand",
    "LoweredConstructProduct",
    "LoweredConstructVariant",
    "LoweredConstructList",
    "LoweredConstructText",
    "LoweredConstructClosure",
    "LoweredProjectField",
    "LoweredProjectVariantField",
    "LoweredDirectCall",
    "LoweredClosureCall",
    "LoweredRuntimeCall",
    "LoweredReturn",
    "LoweredJump",
    "LoweredBranch",
    "LoweredSwitch",
    "LoweredDirectTailCall",
    "LoweredClosureTailCall"
  ]

scalarFunction :: Text -> LoweredRepresentation -> LoweredImmediate -> LoweredFunction
scalarFunction name representation value =
  function name Nothing [] representation [block "entry" [] [] (LoweredReturn (immediate value))] "entry"

identityFunction :: LoweredFunction
identityFunction =
  function
    "identity"
    Nothing
    [parameter "value" i64]
    i64
    [block "entry" [] [] (LoweredReturn (functionParameter "value" i64))]
    "entry"

directCaller :: (LoweredFunctionId -> [LoweredOperand] -> LoweredOperation) -> (LoweredOperand -> LoweredTerminator) -> LoweredFunction
directCaller callOperation returnTerminator =
  function
    "main"
    Nothing
    []
    i64
    [ block
        "entry"
        []
        [instruction "result" i64 (callOperation (functionId "identity") [int64 11])]
        (returnTerminator (temporary "result" i64))
    ]
    "entry"

closureProgram :: Bool -> LoweredProgram
closureProgram useTailCall =
  program
    [LoweredLayout (layoutId "environment") (LoweredClosureEnvironmentLayout [i64])]
    []
    [ function
        "captured"
        (Just (parameter "environment" (managed "environment")))
        [parameter "ignored" i64]
        i64
        [ block
            "entry"
            []
            [ instruction
                "captured-value"
                i64
                (LoweredProjectField (layoutId "environment") 0 (functionParameter "environment" (managed "environment")))
            ]
            (LoweredReturn (temporary "captured-value" i64))
        ]
        "entry",
      closureMain
    ]
    "main"
  where
    signature = LoweredCallSignature [i64] i64
    commonInstructions =
      [ instruction "environment" (managed "environment") (LoweredConstructProduct (layoutId "environment") [int64 42]),
        instruction
          "closure"
          (LoweredClosureRepresentation signature)
          (LoweredConstructClosure (functionId "captured") (temporary "environment" (managed "environment")))
      ]
    closureMain
      | useTailCall =
          function
            "main"
            Nothing
            []
            i64
            [ block
                "entry"
                []
                commonInstructions
                (LoweredClosureTailCall (temporary "closure" (LoweredClosureRepresentation signature)) [int64 0])
            ]
            "entry"
      | otherwise =
          function
            "main"
            Nothing
            []
            i64
            [ block
                "entry"
                []
                ( commonInstructions
                    <> [ instruction
                           "result"
                           i64
                           (LoweredClosureCall (temporary "closure" (LoweredClosureRepresentation signature)) [int64 0])
                       ]
                )
                (LoweredReturn (temporary "result" i64))
            ]
            "entry"

program :: [LoweredLayout] -> [LoweredRuntimeService] -> [LoweredFunction] -> Text -> LoweredProgram
program layouts services functions entryFunction =
  LoweredProgram (LoweredIRVersion 1) layouts services functions (functionId entryFunction)

function :: Text -> Maybe LoweredParameter -> [LoweredParameter] -> LoweredRepresentation -> [LoweredBlock] -> Text -> LoweredFunction
function name environment parameters resultRepresentation blocks entryBlock =
  LoweredFunction (functionId name) environment parameters resultRepresentation blocks (blockId entryBlock)

block :: Text -> [LoweredParameter] -> [LoweredInstruction] -> LoweredTerminator -> LoweredBlock
block name parameters instructions terminator =
  LoweredBlock (blockId name) parameters instructions (Just terminator)

instruction :: Text -> LoweredRepresentation -> LoweredOperation -> LoweredInstruction
instruction name = LoweredInstruction (temporaryId name)

parameter :: Text -> LoweredRepresentation -> LoweredParameter
parameter name = LoweredParameter (parameterId name)

functionParameter :: Text -> LoweredRepresentation -> LoweredOperand
functionParameter name = LoweredFunctionParameterOperand (parameterId name)

blockParameter :: Text -> LoweredRepresentation -> LoweredOperand
blockParameter name = LoweredBlockParameterOperand (parameterId name)

temporary :: Text -> LoweredRepresentation -> LoweredOperand
temporary name = LoweredTemporaryOperand (temporaryId name)

immediate :: LoweredImmediate -> LoweredOperand
immediate = LoweredImmediateOperand

int64 :: Integer -> LoweredOperand
int64 = immediate . LoweredSignedIntegerImmediate LoweredIntegerWidth64

signed :: LoweredIntegerWidth -> LoweredRepresentation
signed = LoweredSignedIntegerRepresentation

unsigned :: LoweredIntegerWidth -> LoweredRepresentation
unsigned = LoweredUnsignedIntegerRepresentation

float :: LoweredFloatWidth -> LoweredRepresentation
float = LoweredFloatRepresentation

i64 :: LoweredRepresentation
i64 = signed LoweredIntegerWidth64

managed :: Text -> LoweredRepresentation
managed = LoweredManagedReferenceRepresentation . layoutId

functionId :: Text -> LoweredFunctionId
functionId = LoweredFunctionId

blockId :: Text -> LoweredBlockId
blockId = LoweredBlockId

temporaryId :: Text -> LoweredTemporaryId
temporaryId = LoweredTemporaryId

layoutId :: Text -> LoweredLayoutId
layoutId = LoweredLayoutId

serviceId :: Text -> LoweredRuntimeServiceId
serviceId = LoweredRuntimeServiceId

parameterId :: Text -> LoweredParameterId
parameterId = LoweredParameterId

expectedScalarRendering :: Text
expectedScalarRendering =
  "LoweredProgram(LoweredIRVersion(1), [], [], [LoweredFunction(LoweredFunctionId(\"main\"), Nothing, [], LoweredUnitRepresentation, [LoweredBlock(LoweredBlockId(\"entry\"), [], [], Just(LoweredReturn(LoweredImmediateOperand(LoweredUnitImmediate))))], LoweredBlockId(\"entry\"))], LoweredFunctionId(\"main\"))"
