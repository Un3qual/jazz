{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bifunctor (bimap)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Void (Void)
import Jazz.Compiler.AST
  ( Expr (EBlock, ELit),
    Literal (LInt),
    Statement (SLet, SSignature),
  )
import qualified Jazz.Compiler.AST as AST
import Jazz.Compiler.CapabilityFacts
  ( ConcreteImplFact (ConcreteImplFact),
  )
import Jazz.Compiler.CoreIdentity (CapabilityId (..), CoreBinderId (..), CoreNodeId (..), ImplId (..), MethodId (..), ResolvedNodeFacts (..), emptyResolvedNodeFacts)
import Jazz.Compiler.Diagnostics
  ( SourceSpan (SourceSpan),
    isErrorDiagnostic,
  )
import Jazz.Compiler.ModuleExports
  ( ModuleExport (ModuleExport),
    ModuleExportInventory,
    exportInventory,
    exportInventoryEntries,
  )
import Jazz.Compiler.ModuleIdentity (SourceUnitOwner (StandaloneSourceUnit), mkModulePath, standaloneModulePath)
import Jazz.Compiler.Name
  ( NameNamespace (CapabilityNamespace, TypeNamespace, ValueNamespace),
    ResolvedName,
    mkIdentifier,
    resolvedImportedName,
    resolvedLocalName,
  )
import Jazz.Compiler.RecursiveBindings (publishResolvedCaptures, resolveLexicalScopes)
import Jazz.Compiler.StableSet
  ( StableSet,
    stableSetDelete,
    stableSetDifference,
    stableSetEmpty,
    stableSetFromPreferred,
    stableSetFromSet,
    stableSetInsert,
    stableSetMembershipSet,
    stableSetOrderedList,
    stableSetSingleton,
  )
import Jazz.Compiler.TypeInference
  ( InferenceInputs (..),
    inferExpressionWithInputs,
    inferredDiagnostics,
  )
import Jazz.Compiler.TypeInference.Types
  ( ClassMethodType (ClassMethodType),
    DataTypeBinding (DataTypeBinding),
    ImplMethodType (ImplMethodType),
    ScopeCapabilityFacts (..),
    emptyScopeCapabilityFacts,
  )
import Jazz.Compiler.TypeRepresentation
  ( SignatureConstraint (..),
    SignaturePayload (..),
    SignatureType (..),
  )
import qualified Jazz.Compiler.TypeRepresentation as TypeRepresentation
import Jazz.Compiler.WarningConfig
  ( defaultWarningSettings,
  )
import Jazz.TestHarness
  ( NamedTest,
    assertEqual,
    runTestSuite,
  )

main :: IO ()
main = runTestSuite "Haskell typeclass contracts" tests

tests :: [NamedTest]
tests =
  [ ("stable sets preserve membership and first-occurrence order", testStableSetMembershipAndOrder),
    ("stable sets normalize preferred order literals", testStableSetPreferredNormalization),
    ("stable set insertion is idempotent", testStableSetInsertion),
    ("stable set deletion and difference preserve retained order", testStableSetRemoval),
    ("stable sets form their intended left-biased monoid", testStableSetMonoid),
    ("scope capability facts preserve collision order", testScopeCapabilityFacts),
    ("concrete implementation facts use rendered identity", testConcreteImplFactsUseRenderedIdentity),
    ("inference accepts imported TypeName facts for source-origin constraints", testInferenceAcceptsImportedTypeNameFact),
    ("inference accepts imported TypeApplication facts for source-origin constraints", testInferenceAcceptsImportedTypeApplicationFact),
    ("signature types traverse constructor names and variables exactly once", testSignatureTypeBitraversal),
    ("semantic types obey bifunctor laws and traverse both identities exactly once", testSemanticTypeBitraversal),
    ("module export inventories union without duplicates", testModuleExportInventory)
  ]

testSignatureTypeBitraversal :: IO ()
testSignatureTypeBitraversal =
  assertEqual
    "constructor names and variables are transformed throughout a nested signature"
    ( TypeRepresentation.TypeApplication
        "RESULT"
        [ TypeRepresentation.TypeVariable 11,
          TypeRepresentation.TypeList
            ( TypeRepresentation.TypeFunction
                (TypeRepresentation.TypeName "ITEM")
                ( TypeRepresentation.TypeTuple
                    [ TypeRepresentation.TypeVariable 12,
                      TypeRepresentation.TypeApplication "MAYBE" [TypeRepresentation.TypeVariable 13]
                    ]
                )
            )
        ]
    )
    (bimap Text.toUpper (+ 10) signature)
  where
    signature :: TypeRepresentation.SignatureType Text Int
    signature =
      TypeRepresentation.TypeApplication
        "result"
        [ TypeRepresentation.TypeVariable 1,
          TypeRepresentation.TypeList
            ( TypeRepresentation.TypeFunction
                (TypeRepresentation.TypeName "item")
                ( TypeRepresentation.TypeTuple
                    [ TypeRepresentation.TypeVariable 2,
                      TypeRepresentation.TypeApplication "maybe" [TypeRepresentation.TypeVariable 3]
                    ]
                )
            )
        ]

testSemanticTypeBitraversal :: IO ()
testSemanticTypeBitraversal = do
  assertEqual "bimap identity" semanticType (bimap id id semanticType)
  assertEqual
    "bimap composition"
    (bimap (Text.reverse . Text.toUpper) ((* 2) . (+ 10)) semanticType)
    (bimap Text.reverse (* 2) (bimap Text.toUpper (+ 10) semanticType))
  assertEqual
    "type names and variables are transformed throughout a nested semantic type"
    ( TypeRepresentation.SemanticData
        "RESULT"
        [ TypeRepresentation.SemanticVariable 11,
          TypeRepresentation.SemanticFunction
            (TypeRepresentation.SemanticData "ITEM" [])
            ( TypeRepresentation.SemanticTuple
                [ TypeRepresentation.SemanticVariable 12,
                  TypeRepresentation.SemanticList
                    (TypeRepresentation.SemanticData "MAYBE" [TypeRepresentation.SemanticVariable 13])
                ]
            )
        ]
    )
    (bimap Text.toUpper (+ 10) semanticType)
  where
    semanticType :: TypeRepresentation.SemanticType Text Int
    semanticType =
      TypeRepresentation.SemanticData
        "result"
        [ TypeRepresentation.SemanticVariable 1,
          TypeRepresentation.SemanticFunction
            (TypeRepresentation.SemanticData "item" [])
            ( TypeRepresentation.SemanticTuple
                [ TypeRepresentation.SemanticVariable 2,
                  TypeRepresentation.SemanticList
                    (TypeRepresentation.SemanticData "maybe" [TypeRepresentation.SemanticVariable 3])
                ]
            )
        ]

testStableSetMembershipAndOrder :: IO ()
testStableSetMembershipAndOrder = do
  assertEqual "membership projection agrees with the ordered projection" expectedMembers (stableSetMembershipSet stable)
  assertEqual "first occurrences determine order" [3, 1, 2] (stableSetOrderedList stable)
  assertEqual "from-set uses deterministic set order" [1, 2, 3] (stableSetOrderedList (stableSetFromSet expectedMembers))
  where
    expectedMembers :: Set.Set Int
    expectedMembers = Set.fromList [1, 2, 3]
    stable = stableSetFromPreferred [3, 1, 3, 2] expectedMembers

testStableSetPreferredNormalization :: IO ()
testStableSetPreferredNormalization =
  assertEqual
    "duplicates and foreign preferred values are removed before set-ordered remainder"
    [3, 1, 2, 4]
    (stableSetOrderedList (stableSetFromPreferred [3, 1, 3, 99] (Set.fromList [1, 2, 3, 4] :: Set.Set Int)))

testStableSetInsertion :: IO ()
testStableSetInsertion = do
  assertEqual "inserting an existing member is idempotent" once (stableSetInsert 2 once)
  assertEqual "insertion appends a new member" [2, 1] (stableSetOrderedList (stableSetInsert 1 once))
  assertEqual "empty and singleton constructors agree with insertion" singleton (stableSetInsert 2 stableSetEmpty)
  where
    once :: StableSet Int
    once = stableSetInsert 2 (stableSetInsert 2 stableSetEmpty)
    singleton :: StableSet Int
    singleton = stableSetSingleton 2

testStableSetRemoval :: IO ()
testStableSetRemoval = do
  assertEqual "deletion removes one member without reordering" [3, 2, 4] (stableSetOrderedList (stableSetDelete 1 stable))
  assertEqual "difference removes all requested members without reordering" [1, 4] (stableSetOrderedList (stableSetDifference stable (Set.fromList [3, 2])))
  where
    stable :: StableSet Int
    stable = stableSetFromPreferred [3, 1] (Set.fromList [1, 2, 3, 4])

testStableSetMonoid :: IO ()
testStableSetMonoid = do
  assertMonoidLaws "stable set" first second third
  assertEqual "union is left-biased by first occurrence" [3, 1, 2, 4] (stableSetOrderedList (first <> second))
  assertEqual "union membership agrees with its order" (Set.fromList [1, 2, 3, 4]) (stableSetMembershipSet (first <> second))
  where
    first = stableSetFromPreferred [3, 1] (Set.fromList [1, 3])
    second = stableSetFromPreferred [1, 2, 4] (Set.fromList [1, 2, 4])
    third :: StableSet Int
    third = stableSetFromPreferred [4, 3] (Set.fromList [3, 4])

assertMonoidLaws :: (Eq value, Show value, Monoid value) => Text -> value -> value -> value -> IO ()
assertMonoidLaws label first second third = do
  assertEqual (label <> " left identity") first (mempty <> first)
  assertEqual (label <> " right identity") first (first <> mempty)
  assertEqual
    (label <> " associativity")
    ((first <> second) <> third)
    (first <> (second <> third))

testScopeCapabilityFacts :: IO ()
testScopeCapabilityFacts = do
  assertMonoidLaws "scope capability facts" first second third
  assertEqual
    "class facts remain left-biased"
    (Just 1)
    (Map.lookup "Comparable" (scopeClassFacts combined))
  assertEqual
    "method facts remain left-biased"
    (Just (ClassMethodType "Left" TypeRepresentation.SemanticInt))
    (Map.lookup "compare" (scopeClassMethodSignatures combined))
  assertEqual
    "implementation methods preserve left-to-right order"
    (Just [fixtureImplMethod TypeRepresentation.SemanticInt, fixtureImplMethod TypeRepresentation.SemanticBool])
    (Map.lookup "Comparable" (scopeConcreteImplMethods combined))
  assertEqual
    "three-way implementation collisions preserve left-to-right order"
    (Just [fixtureImplMethod TypeRepresentation.SemanticInt, fixtureImplMethod TypeRepresentation.SemanticBool, fixtureImplMethod TypeRepresentation.SemanticBool])
    (Map.lookup "Comparable" (scopeConcreteImplMethods (first <> second <> third)))
  where
    combined = first <> second
    first =
      mempty
        { scopeClassFacts = Map.singleton "Comparable" 1,
          scopeClassMethodSignatures =
            Map.singleton "compare" (ClassMethodType "Left" TypeRepresentation.SemanticInt),
          scopeConcreteImplMethods =
            Map.singleton "Comparable" [fixtureImplMethod TypeRepresentation.SemanticInt]
        }
    second =
      mempty
        { scopeClassFacts = Map.singleton "Comparable" 2,
          scopeClassMethodSignatures =
            Map.singleton "compare" (ClassMethodType "Right" TypeRepresentation.SemanticBool),
          scopeConcreteImplMethods =
            Map.singleton "Comparable" [fixtureImplMethod TypeRepresentation.SemanticBool]
        }
    third =
      mempty
        { scopeClassFacts = Map.singleton "Comparable" 3,
          scopeClassMethodSignatures =
            Map.singleton "compare" (ClassMethodType "Third" TypeRepresentation.SemanticInt),
          scopeConcreteImplMethods =
            Map.singleton "Comparable" [fixtureImplMethod TypeRepresentation.SemanticBool],
          scopeGeneratedEqualityClassFacts = Set.singleton "Eq",
          scopeConcreteImplFacts = Set.singleton (ConcreteImplFact (localCapabilityName "Comparable") TypeInt)
        }

testConcreteImplFactsUseRenderedIdentity :: IO ()
testConcreteImplFactsUseRenderedIdentity = do
  assertEqual "rendered capability facts compare equal" True (sourceFact == importedFact)
  assertEqual "rendered capability facts share set membership" True (Set.member sourceFact (Set.singleton importedFact))
  assertEqual "nested TypeName origins share set membership" True (Set.member sourceTypeNameFact (Set.singleton importedTypeNameFact))
  assertEqual "nested TypeApplication origins share set membership" True (Set.member sourceTypeApplicationFact (Set.singleton importedTypeApplicationFact))
  assertEqual
    "legacy rendered argument collisions remain equal"
    True
    ( ConcreteImplFact (localCapabilityName "Marked") TypeInt
        == ConcreteImplFact (localCapabilityName "Marked") (TypeName (localTypeName "Int"))
    )
  where
    sourceFact = ConcreteImplFact (localCapabilityName "Lib::Marked::Marked!") TypeInt
    importedFact =
      ConcreteImplFact
        (resolvedImportedName (mkModulePath (mkIdentifier "Lib" :| [mkIdentifier "Marked"])) CapabilityNamespace (mkIdentifier "Marked!"))
        TypeInt
    sourceTypeNameFact = ConcreteImplFact (localCapabilityName "Marked") (TypeName (localTypeName "Lib::Types::Tagged"))
    importedTypeNameFact = ConcreteImplFact (localCapabilityName "Marked") (TypeName (importedTypeName "Tagged"))
    sourceTypeApplicationFact =
      ConcreteImplFact
        (localCapabilityName "Marked")
        (TypeApplication (localTypeName "Lib::Types::Box") [TypeName (localTypeName "Lib::Types::Tagged")])
    importedTypeApplicationFact =
      ConcreteImplFact
        (localCapabilityName "Marked")
        (TypeApplication (importedTypeName "Box") [TypeName (importedTypeName "Tagged")])

testInferenceAcceptsImportedTypeNameFact :: IO ()
testInferenceAcceptsImportedTypeNameFact =
  assertImportedConstraintFactAccepted
    "TypeName imported fact"
    (TypeName (localTypeName "Lib::Types::Tagged"))
    (TypeName (importedTypeName "Tagged"))

testInferenceAcceptsImportedTypeApplicationFact :: IO ()
testInferenceAcceptsImportedTypeApplicationFact =
  assertImportedConstraintFactAccepted
    "TypeApplication imported fact"
    (TypeApplication (localTypeName "Lib::Types::Box") [TypeName (localTypeName "Lib::Types::Tagged")])
    (TypeApplication (importedTypeName "Box") [TypeName (importedTypeName "Tagged")])

assertImportedConstraintFactAccepted :: Text -> AST.SignatureType 'AST.Resolved -> AST.SignatureType 'AST.Resolved -> IO ()
assertImportedConstraintFactAccepted label sourceArgument importedArgument = do
  sourceResult <- inferExpressionWithInputs (inferenceInputs sourceArgument) (constrainedProgram sourceArgument)
  importedResult <- inferExpressionWithInputs (inferenceInputs importedArgument) (constrainedProgram sourceArgument)
  assertEqual (label <> " source-origin control errors") [] (filter isErrorDiagnostic (inferredDiagnostics sourceResult))
  assertEqual (label <> " imported-origin errors") [] (filter isErrorDiagnostic (inferredDiagnostics importedResult))
  where
    inferenceInputs factArgument =
      InferenceInputs
        { inferenceWarningSettings = defaultWarningSettings,
          inferenceExternalUses = Set.empty,
          inferenceImportedTypes = Map.empty,
          inferenceImportedDataTypes =
            Map.fromList
              [ ("Lib::Types::Box", DataTypeBinding [localTypeName "item"] []),
                ("Lib::Types::Tagged", DataTypeBinding [] [])
              ],
          inferenceImportedConstructorWitnessNames = Map.empty,
          inferenceImportedCapabilities =
            emptyScopeCapabilityFacts
              { scopeClassFacts = Map.singleton "Marked" 1,
                scopeConcreteImplFacts = Set.singleton (ConcreteImplFact (localCapabilityName "Marked") factArgument)
              },
          inferenceImportedClassNames = Set.singleton "Marked",
          inferenceCurrentModulePath = Nothing
        }

    constrainedProgram constraintArgument =
      publishResolvedCaptures . resolveLexicalScopes Map.empty Set.empty $
        EBlock
          (fixtureExpressionNode 0)
          [ SSignature
              (fixtureStatementNode 1 (SourceSpan 1 1))
              (localValueName "value")
              (ConstrainedSignature [SignatureConstraint (localCapabilityName "Marked") [constraintArgument]] TypeInt),
            SLet
              (fixtureStatementNode 2 (SourceSpan 2 1))
              (localValueName "value")
              (ELit (fixtureExpressionNode 3) (LInt 1))
          ]

fixtureExpressionNode :: Int -> AST.CoreNode 'AST.Resolved sort
fixtureExpressionNode index = AST.CoreNode (AST.CoreNodeId index) (SourceSpan 1 1) (emptyResolvedNodeFacts fixtureOwner)

fixtureStatementNode :: Int -> SourceSpan -> AST.CoreNode 'AST.Resolved sort
fixtureStatementNode index spanValue = AST.CoreNode nodeId spanValue ((emptyResolvedNodeFacts fixtureOwner) {resolvedNodeBinder = Just (CoreBinderId (fixtureOwner, nodeId))})
  where
    nodeId = AST.CoreNodeId index

fixtureOwner :: SourceUnitOwner
fixtureOwner = StandaloneSourceUnit standaloneModulePath

localValueName :: Text -> ResolvedName
localValueName = resolvedLocalName ValueNamespace . mkIdentifier

localTypeName :: Text -> ResolvedName
localTypeName = resolvedLocalName TypeNamespace . mkIdentifier

localCapabilityName :: Text -> ResolvedName
localCapabilityName = resolvedLocalName CapabilityNamespace . mkIdentifier

importedTypeName :: Text -> ResolvedName
importedTypeName name =
  resolvedImportedName
    (mkModulePath (mkIdentifier "Lib" :| [mkIdentifier "Types"]))
    TypeNamespace
    (mkIdentifier name)

testModuleExportInventory :: IO ()
testModuleExportInventory = do
  assertMonoidLaws "module export inventory" first second third
  assertEqual
    "module export inventory union"
    ( Set.fromList
        [ ModuleExport ValueNamespace "answer",
          ModuleExport TypeNamespace "Answer"
        ]
    )
    (exportInventoryEntries (first <> second))
  where
    first = exportInventory [ModuleExport ValueNamespace "answer"]
    second =
      exportInventory
        [ ModuleExport ValueNamespace "answer",
          ModuleExport TypeNamespace "Answer"
        ]
    third :: ModuleExportInventory
    third = exportInventory [ModuleExport ValueNamespace "other"]

fixtureImplMethod :: TypeRepresentation.SemanticType ResolvedName Void -> ImplMethodType
fixtureImplMethod target = ImplMethodType target (CapabilityId (resolvedLocalName CapabilityNamespace (mkIdentifier "Comparable"))) (MethodId (ImplId (StandaloneSourceUnit standaloneModulePath, CoreNodeId 0), mkIdentifier "compare"))
