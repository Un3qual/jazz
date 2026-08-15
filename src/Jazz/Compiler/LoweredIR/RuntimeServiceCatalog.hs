{-# LANGUAGE OverloadedStrings #-}

-- | Stable backend-neutral runtime-support identities used by Lowered IR.
module Jazz.Compiler.LoweredIR.RuntimeServiceCatalog
  ( RuntimeServiceKey (..),
    textLayoutId,
    textLayout,
    textRepresentation,
    textOperationService,
    runtimeServiceContract,
    orderedRuntimeServices,
  )
where

import qualified Data.Set as Set
import Jazz.Compiler.BuiltinCatalog
  ( BuiltinSymbol (BuiltinTextAppend, BuiltinTextAppendChar, BuiltinTextLength),
  )
import Jazz.Compiler.LoweredIR

data RuntimeServiceKey
  = TextEqualService
  | TextLengthService
  | TextAppendService
  | TextAppendCharService
  deriving (Eq, Ord, Show)

textLayoutId :: LoweredLayoutId
textLayoutId = LoweredLayoutId "jazz.layout.text.v1"

textLayout :: LoweredLayout
textLayout = LoweredLayout textLayoutId LoweredTextLayout

textRepresentation :: LoweredRepresentation
textRepresentation = LoweredManagedReferenceRepresentation textLayoutId

textOperationService :: BuiltinSymbol -> Maybe RuntimeServiceKey
textOperationService symbol =
  case symbol of
    BuiltinTextLength -> Just TextLengthService
    BuiltinTextAppend -> Just TextAppendService
    BuiltinTextAppendChar -> Just TextAppendCharService
    _ -> Nothing

runtimeServiceContract :: RuntimeServiceKey -> LoweredRuntimeService
runtimeServiceContract key =
  case key of
    TextEqualService ->
      service
        "jazz.runtime.text.equal.v1"
        [textRepresentation, textRepresentation]
        LoweredBoolRepresentation
    TextLengthService ->
      service
        "jazz.runtime.text.length.v1"
        [textRepresentation]
        int64Representation
    TextAppendService ->
      service
        "jazz.runtime.text.append.v1"
        [textRepresentation, textRepresentation]
        textRepresentation
    TextAppendCharService ->
      service
        "jazz.runtime.text.append-char.v1"
        [textRepresentation, LoweredCharRepresentation]
        textRepresentation
  where
    service identifier arguments result =
      LoweredRuntimeService
        (LoweredRuntimeServiceId identifier)
        (LoweredCallSignature arguments result)
    int64Representation =
      LoweredSignedIntegerRepresentation LoweredIntegerWidth64

orderedRuntimeServices :: Set.Set RuntimeServiceKey -> [LoweredRuntimeService]
orderedRuntimeServices required =
  [ runtimeServiceContract key
  | key <-
      [ TextEqualService,
        TextLengthService,
        TextAppendService,
        TextAppendCharService
      ],
    Set.member key required
  ]
