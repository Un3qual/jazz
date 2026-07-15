{-# LANGUAGE OverloadedStrings #-}

module JazzNext.ProgramCorpus.Manifest
  ( canonicalizeValidatedPath,
    loadProgramCorpus,
    loadProgramCorpusAt,
    loadProgramCorpusAtWithManifestReader,
    loadProgramCorpusAtWithRootCanonicalizer,
    loadProgramCorpusWithRootCanonicalizer,
    programCaseById,
    renderProgramCorpusViolation,
  )
where

import Control.Exception (IOException, try)
import Data.Aeson
  ( Value,
    eitherDecodeStrict',
    withObject,
    (.:),
    (.:?),
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as ByteString
import Data.Either (partitionEithers)
import Data.List (group, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import JazzNext.ProgramCorpus.Types
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
  )
import System.FilePath
  ( dropExtension,
    isAbsolute,
    makeRelative,
    normalise,
    splitDirectories,
    takeDirectory,
    takeExtension,
    (</>),
  )

supportedSchemaVersion :: Int
supportedSchemaVersion = 1

parseDocument :: Value -> Parser ProgramCorpusDocument
parseDocument = withObject "program corpus" $ \object ->
  do
    caseValues <- object .: "cases"
    ProgramCorpusDocument
      <$> object .: "schemaVersion"
      <*> mapM parseCase caseValues

parseCase :: Value -> Parser ProgramCaseDocument
parseCase = withObject "program corpus case" $ \object ->
  do
    budgetValue <- object .: "budgets"
    ProgramCaseDocument
      <$> object .: "id"
      <*> object .: "directory"
      <*> object .: "entrySource"
      <*> object .: "moduleRoot"
      <*> object .: "expectedTermination"
      <*> object .: "expectedStdout"
      <*> object .: "workload"
      <*> object .: "features"
      <*> object .: "benchmarks"
      <*> parseBudgets budgetValue

parseBudgets :: Value -> Parser ProgramBudgets
parseBudgets = withObject "program budgets" $ \object -> do
  case unknownBudgetFields object of
    [] -> pure ()
    fields -> fail ("unknown program budget field: " <> Text.unpack (Text.intercalate ", " fields))
  optionalLimits <-
    Map.fromList . catMaybes
      <$> sequence
        [ optionalBudget ForcedValuesBudget <$> object .:? "forcedValues",
          optionalBudget ClosureApplicationsBudget <$> object .:? "closureApplications",
          optionalBudget BuiltinApplicationsBudget <$> object .:? "builtinApplications",
          optionalBudget OperatorApplicationsBudget <$> object .:? "operatorApplications",
          optionalBudget ConstructorApplicationsBudget <$> object .:? "constructorApplications",
          optionalBudget MethodApplicationsBudget <$> object .:? "methodApplications",
          optionalBudget ClosuresCreatedBudget <$> object .:? "closuresCreated",
          optionalBudget BindingsCapturedBudget <$> object .:? "bindingsCaptured",
          optionalBudget MaximumCaptureWidthBudget <$> object .:? "maximumCaptureWidth",
          optionalBudget ListCellsConstructedBudget <$> object .:? "listCellsConstructed",
          optionalBudget TuplesConstructedBudget <$> object .:? "tuplesConstructed",
          optionalBudget SaturatedAdtValuesConstructedBudget <$> object .:? "saturatedAdtValuesConstructed",
          optionalBudget PatternAttemptsBudget <$> object .:? "patternAttempts",
          optionalBudget PatternMatchesBudget <$> object .:? "patternMatches",
          optionalBudget PatternBindingsBudget <$> object .:? "patternBindings",
          optionalBudget BuiltinCallsBudget <$> object .:? "builtinCalls",
          optionalBudget HostOperationsBudget <$> object .:? "hostOperations",
          optionalBudget DeferredCacheHitsBudget <$> object .:? "deferredCacheHits",
          optionalBudget DeferredCacheMissesBudget <$> object .:? "deferredCacheMisses",
          optionalBudget DeferredCacheRecursiveEvaluationsBudget <$> object .:? "deferredCacheRecursiveEvaluations"
        ]
  ProgramBudgets
    <$> object .: "steps"
    <*> object .: "applications"
    <*> object .: "maxContinuationDepth"
    <*> pure optionalLimits

unknownBudgetFields :: KeyMap.KeyMap Value -> [Text]
unknownBudgetFields object =
  sort
    [ field
    | key <- KeyMap.keys object,
      let field = Key.toText key,
      field `notElem` knownBudgetFields
    ]
  where
    knownBudgetFields = map programBudgetMetricName ([minBound .. maxBound] :: [ProgramBudgetMetric])

optionalBudget :: ProgramBudgetMetric -> Maybe limit -> Maybe (ProgramBudgetMetric, limit)
optionalBudget metric maybeLimit =
  case maybeLimit of
    Nothing -> Nothing
    Just limit -> Just (metric, limit)

loadProgramCorpus :: IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpus = loadProgramCorpusWithRootCanonicalizer canonicalizePath

loadProgramCorpusWithRootCanonicalizer ::
  (FilePath -> IO FilePath) ->
  IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpusWithRootCanonicalizer canonicalizeRoot = do
  packageRootResult <- findPackageRoot canonicalizeRoot
  case packageRootResult of
    Left violation -> pure (Left [violation])
    Right packageRoot ->
      loadProgramCorpusAtWithRootCanonicalizer
        canonicalizeRoot
        (packageRoot </> "programs")

loadProgramCorpusAt :: FilePath -> IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpusAt = loadProgramCorpusAtWithDependencies canonicalizePath ByteString.readFile

loadProgramCorpusAtWithManifestReader ::
  (FilePath -> IO ByteString.ByteString) ->
  FilePath ->
  IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpusAtWithManifestReader = loadProgramCorpusAtWithDependencies canonicalizePath

loadProgramCorpusAtWithRootCanonicalizer ::
  (FilePath -> IO FilePath) ->
  FilePath ->
  IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpusAtWithRootCanonicalizer canonicalizeRoot =
  loadProgramCorpusAtWithDependencies canonicalizeRoot ByteString.readFile

loadProgramCorpusAtWithDependencies ::
  (FilePath -> IO FilePath) ->
  (FilePath -> IO ByteString.ByteString) ->
  FilePath ->
  IO (Either [ProgramCorpusViolation] ProgramCorpus)
loadProgramCorpusAtWithDependencies canonicalizeRoot readManifest requestedRoot = do
  let manifestPath = requestedRoot </> "corpus.json"
  manifestExists <- doesFileExist manifestPath
  if not manifestExists
    then pure (Left [MissingCorpusManifest manifestPath])
    else do
      manifestResult <- try (readManifest manifestPath) :: IO (Either IOException ByteString.ByteString)
      case manifestResult of
        Left exception ->
          pure
            ( Left
                [ UnreadableCorpusManifest
                    manifestPath
                    (Text.pack (show exception))
                ]
            )
        Right bytes ->
          case eitherDecodeStrict' bytes of
            Left message -> pure (Left [ManifestDecodeFailure (Text.pack message)])
            Right value ->
              case parseEither parseDocument value of
                Left message -> pure (Left [ManifestDecodeFailure (Text.pack message)])
                Right document -> validateDocument canonicalizeRoot requestedRoot document

programCaseById :: Text -> ProgramCorpus -> Maybe ProgramCase
programCaseById identifier corpus =
  case filter ((== identifier) . programCaseIdentifier) (programCorpusCases corpus) of
    programCase : _ -> Just programCase
    [] -> Nothing

validateDocument ::
  (FilePath -> IO FilePath) ->
  FilePath ->
  ProgramCorpusDocument ->
  IO (Either [ProgramCorpusViolation] ProgramCorpus)
validateDocument canonicalizeRoot requestedRoot document = do
  canonicalRootResult <- canonicalizeCorpusRoot canonicalizeRoot requestedRoot
  case canonicalRootResult of
    Left violation -> pure (Left [violation])
    Right canonicalRoot -> do
      caseResults <- mapM (validateCase canonicalRoot) (programCorpusDocumentCases document)
      let documentViolations =
            [ UnsupportedSchemaVersion (programCorpusDocumentSchemaVersion document)
            | programCorpusDocumentSchemaVersion document /= supportedSchemaVersion
            ]
          duplicateViolations =
            map DuplicateCaseIdentifier (duplicates (map programCaseDocumentIdentifier (programCorpusDocumentCases document)))
              <> map
                (DuplicateCaseDirectory . makeRelative canonicalRoot)
                (duplicates (mapMaybe validatedCaseDirectory caseResults))
          caseViolations = concatMap validatedCaseViolations caseResults
          violations = sortOn renderProgramCorpusViolation (documentViolations <> duplicateViolations <> caseViolations)
      if null violations
        then
          pure
            ( Right
                ProgramCorpus
                  { programCorpusRoot = canonicalRoot,
                    programCorpusSchemaVersion = programCorpusDocumentSchemaVersion document,
                    programCorpusCases = mapMaybe validatedProgramCase caseResults
                  }
            )
        else pure (Left violations)

data ValidatedCase = ValidatedCase
  { validatedCaseViolations :: [ProgramCorpusViolation],
    validatedCaseDirectory :: Maybe FilePath,
    validatedProgramCase :: Maybe ProgramCase
  }

validateCase :: FilePath -> ProgramCaseDocument -> IO ValidatedCase
validateCase corpusRoot document = do
  directoryResult <- validateDirectoryPath corpusRoot identifier CaseDirectoryPath "" (programCaseDocumentDirectory document)
  entryResult <- validateFilePath corpusRoot identifier EntrySourcePath (programCaseDocumentDirectory document) (programCaseDocumentEntrySource document)
  moduleRootResult <- validateDirectoryPath corpusRoot identifier ModuleRootPath (programCaseDocumentDirectory document) (programCaseDocumentModuleRoot document)
  expectedResult <- validateFilePath corpusRoot identifier ExpectedStdoutPath (programCaseDocumentDirectory document) (programCaseDocumentExpectedStdout document)
  expectedContentsResult <- readExpectedStdout identifier expectedResult
  let terminationResult =
        requiredValue
          (UnknownTerminationCategory identifier (programCaseDocumentExpectedTermination document))
          (parseProgramTermination (programCaseDocumentExpectedTermination document))
      workloadResult =
        requiredValue
          (UnknownWorkloadClass identifier (programCaseDocumentWorkload document))
          (parseWorkloadClass (programCaseDocumentWorkload document))
      featureResults =
        map
          (\name -> requiredValue (UnknownFeatureTag identifier name) (parseFeatureTag name))
          (programCaseDocumentFeatures document)
      benchmarkResults =
        map
          (\name -> requiredValue (UnknownBenchmarkGroup identifier name) (parseBenchmarkGroup name))
          (programCaseDocumentBenchmarks document)
      (featureViolations, features) = partitionEithers featureResults
      (benchmarkViolations, benchmarks) = partitionEithers benchmarkResults
      valueViolations =
        leftValue terminationResult
          <> leftValue workloadResult
          <> featureViolations
          <> benchmarkViolations
      pathViolations =
        leftValues [directoryResult, entryResult, moduleRootResult, expectedResult]
          <> leftValue expectedContentsResult
      extensionViolations =
        [ InvalidEntrySourceExtension identifier (programCaseDocumentEntrySource document)
        | takeExtension (programCaseDocumentEntrySource document) /= ".jz"
        ]
      moduleViolations =
        case (entryResult, moduleRootResult) of
          (Right entryPath, Right moduleRoot)
            | not (isContainedBy moduleRoot entryPath) ->
                [ EntrySourceOutsideModuleRoot
                    identifier
                    (programCaseDocumentModuleRoot document)
                    (programCaseDocumentEntrySource document)
                ]
          _ -> []
      violations = valueViolations <> pathViolations <> extensionViolations <> moduleViolations
  pure
    ValidatedCase
      { validatedCaseViolations = violations,
        validatedCaseDirectory = either (const Nothing) Just directoryResult,
        validatedProgramCase =
          buildCase
            corpusRoot
            document
            terminationResult
            workloadResult
            features
            benchmarks
            directoryResult
            entryResult
            moduleRootResult
            expectedResult
            expectedContentsResult
            violations
      }
  where
    identifier = programCaseDocumentIdentifier document

buildCase ::
  FilePath ->
  ProgramCaseDocument ->
  Either ProgramCorpusViolation ProgramTermination ->
  Either ProgramCorpusViolation WorkloadClass ->
  [FeatureTag] ->
  [BenchmarkGroup] ->
  Either ProgramCorpusViolation FilePath ->
  Either ProgramCorpusViolation FilePath ->
  Either ProgramCorpusViolation FilePath ->
  Either ProgramCorpusViolation FilePath ->
  Either ProgramCorpusViolation Text ->
  [ProgramCorpusViolation] ->
  Maybe ProgramCase
buildCase corpusRoot document terminationResult workloadResult features benchmarks directoryResult entryResult moduleRootResult expectedResult expectedContentsResult violations =
  case (terminationResult, workloadResult, directoryResult, entryResult, moduleRootResult, expectedResult, expectedContentsResult) of
    (Right termination, Right workload, Right directory, Right entrySource, Right moduleRoot, Right expectedPath, Right expectedContents)
      | null violations ->
          Just
            ProgramCase
              { programCaseIdentifier = programCaseDocumentIdentifier document,
                programCasePackageRoot = takeDirectory corpusRoot,
                programCaseDirectory = directory,
                programCaseEntrySource = entrySource,
                programCaseModuleRoot = moduleRoot,
                programCaseEntryModulePath = modulePathFromFile moduleRoot entrySource,
                programCaseExpectedTermination = termination,
                programCaseExpectedStdoutPath = expectedPath,
                programCaseExpectedStdout = expectedContents,
                programCaseWorkload = workload,
                programCaseFeatures = features,
                programCaseBenchmarks = benchmarks,
                programCaseBudgets = programCaseDocumentBudgets document
              }
    _ -> Nothing

validateDirectoryPath :: FilePath -> Text -> ProgramPathField -> FilePath -> FilePath -> IO (Either ProgramCorpusViolation FilePath)
validateDirectoryPath = validatePath doesDirectoryExist

validateFilePath :: FilePath -> Text -> ProgramPathField -> FilePath -> FilePath -> IO (Either ProgramCorpusViolation FilePath)
validateFilePath = validatePath doesFileExist

validatePath ::
  (FilePath -> IO Bool) ->
  FilePath ->
  Text ->
  ProgramPathField ->
  FilePath ->
  FilePath ->
  IO (Either ProgramCorpusViolation FilePath)
validatePath pathExists corpusRoot identifier field parent rawPath
  | isAbsolute rawPath = pure (Left (AbsoluteCorpusPath identifier field rawPath))
  | pathEscapes rawPath = pure (Left (EscapingCorpusPath identifier field rawPath))
  | otherwise = do
      let relativePath = normalise (parent </> rawPath)
          candidatePath = corpusRoot </> relativePath
      exists <- pathExists candidatePath
      if not exists
        then pure (Left (MissingCorpusPath identifier field relativePath))
        else canonicalizeValidatedPath canonicalizePath corpusRoot identifier field relativePath

canonicalizeValidatedPath ::
  (FilePath -> IO FilePath) ->
  FilePath ->
  Text ->
  ProgramPathField ->
  FilePath ->
  IO (Either ProgramCorpusViolation FilePath)
canonicalizeValidatedPath canonicalize corpusRoot identifier field relativePath = do
  canonicalResult <- try (canonicalize (corpusRoot </> relativePath)) :: IO (Either IOException FilePath)
  pure $
    case canonicalResult of
      Left exception ->
        Left
          ( UnreadableCorpusPath
              identifier
              field
              relativePath
              (Text.pack (show exception))
          )
      Right canonicalPath
        | isContainedBy corpusRoot canonicalPath -> Right canonicalPath
        | otherwise -> Left (EscapingCorpusPath identifier field relativePath)

readExpectedStdout :: Text -> Either ProgramCorpusViolation FilePath -> IO (Either ProgramCorpusViolation Text)
readExpectedStdout _ (Left _) = pure (Right "")
readExpectedStdout identifier (Right path) = do
  readResult <- try (TextIO.readFile path) :: IO (Either IOException Text)
  pure $
    case readResult of
      Left exception ->
        Left
          ( UnreadableCorpusPath
              identifier
              ExpectedStdoutPath
              path
              (Text.pack (show exception))
          )
      Right contents -> Right contents

requiredValue :: ProgramCorpusViolation -> Maybe value -> Either ProgramCorpusViolation value
requiredValue violation maybeValue =
  case maybeValue of
    Nothing -> Left violation
    Just value -> Right value

leftValues :: [Either left right] -> [left]
leftValues values = fst (partitionEithers values)

leftValue :: Either left right -> [left]
leftValue value =
  case value of
    Left left -> [left]
    Right _ -> []

duplicates :: (Ord value) => [value] -> [value]
duplicates values = [value | value : _ : _ <- group (sort values)]

pathEscapes :: FilePath -> Bool
pathEscapes path = ".." `elem` splitDirectories (normalise path)

isContainedBy :: FilePath -> FilePath -> Bool
isContainedBy parent child =
  let relative = makeRelative parent child
   in not (isAbsolute relative) && not (pathEscapes relative)

modulePathFromFile :: FilePath -> FilePath -> [Text]
modulePathFromFile moduleRoot entrySource =
  map Text.pack (filter (`notElem` ["", "."]) (splitDirectories (dropExtension (makeRelative moduleRoot entrySource))))

findPackageRoot :: (FilePath -> IO FilePath) -> IO (Either ProgramCorpusViolation FilePath)
findPackageRoot canonicalizeRoot = do
  currentDirectory <- getCurrentDirectory
  search (candidateRoots currentDirectory)
  where
    search candidates =
      case candidates of
        [] -> pure (Left (MissingCorpusManifest "could not locate jazz-next.cabal"))
        candidate : remaining -> do
          markerExists <- doesFileExist (candidate </> "jazz-next.cabal")
          if markerExists
            then canonicalizeCorpusRoot canonicalizeRoot candidate
            else search remaining

canonicalizeCorpusRoot ::
  (FilePath -> IO FilePath) ->
  FilePath ->
  IO (Either ProgramCorpusViolation FilePath)
canonicalizeCorpusRoot canonicalizeRoot requestedRoot = do
  canonicalResult <- try (canonicalizeRoot requestedRoot) :: IO (Either IOException FilePath)
  pure $
    case canonicalResult of
      Left exception ->
        Left
          ( UnreadableCorpusRoot
              requestedRoot
              (Text.pack (show exception))
          )
      Right canonicalRoot -> Right canonicalRoot

candidateRoots :: FilePath -> [FilePath]
candidateRoots currentDirectory =
  concatMap (\ancestor -> [ancestor, ancestor </> "jazz-next"]) (ancestors currentDirectory)

ancestors :: FilePath -> [FilePath]
ancestors directory =
  let parent = takeDirectory directory
   in directory : if parent == directory then [] else ancestors parent

renderProgramCorpusViolation :: ProgramCorpusViolation -> Text
renderProgramCorpusViolation violation =
  case violation of
    MissingCorpusManifest path -> "missing program corpus manifest: " <> Text.pack path
    UnreadableCorpusManifest path message ->
      "could not read program corpus manifest " <> Text.pack path <> ": " <> message
    UnreadableCorpusRoot path message ->
      "could not canonicalize program corpus root " <> Text.pack path <> ": " <> message
    ManifestDecodeFailure message -> "could not decode program corpus manifest: " <> message
    UnsupportedSchemaVersion version -> "unsupported program corpus schema version: " <> Text.pack (show version)
    DuplicateCaseIdentifier identifier -> "duplicate program case identifier: " <> identifier
    DuplicateCaseDirectory directory -> "duplicate program case directory: " <> Text.pack directory
    UnknownTerminationCategory identifier category -> casePrefix identifier <> "unknown termination category: " <> category
    UnknownWorkloadClass identifier workload -> casePrefix identifier <> "unknown workload class: " <> workload
    UnknownFeatureTag identifier feature -> casePrefix identifier <> "unknown feature tag: " <> feature
    UnknownBenchmarkGroup identifier groupName -> casePrefix identifier <> "unknown benchmark group: " <> groupName
    AbsoluteCorpusPath identifier field path -> casePathPrefix identifier field <> "must be relative: " <> Text.pack path
    EscapingCorpusPath identifier field path -> casePathPrefix identifier field <> "escapes the corpus root: " <> Text.pack path
    MissingCorpusPath identifier field path -> casePathPrefix identifier field <> "does not exist: " <> Text.pack path
    UnreadableCorpusPath identifier field path message -> casePathPrefix identifier field <> "could not read " <> Text.pack path <> ": " <> message
    EntrySourceOutsideModuleRoot identifier moduleRoot entrySource ->
      casePrefix identifier
        <> "entry source "
        <> Text.pack entrySource
        <> " is outside module root "
        <> Text.pack moduleRoot
    InvalidEntrySourceExtension identifier entrySource -> casePrefix identifier <> "entry source must use .jz: " <> Text.pack entrySource

casePrefix :: Text -> Text
casePrefix identifier = "program case " <> identifier <> ": "

casePathPrefix :: Text -> ProgramPathField -> Text
casePathPrefix identifier field = casePrefix identifier <> renderPathField field <> " "

renderPathField :: ProgramPathField -> Text
renderPathField field =
  case field of
    CaseDirectoryPath -> "directory"
    EntrySourcePath -> "entry source"
    ModuleRootPath -> "module root"
    ExpectedStdoutPath -> "expected stdout"
