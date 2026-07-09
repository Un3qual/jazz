-- | Shared binder semantics for lowered core patterns.
module JazzNext.Compiler.Pattern
  ( commonPatternBinderNames,
    extendBoundWithPattern,
    patternBinderNames
  ) where

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import JazzNext.Compiler.AST (Pattern (..))
import JazzNext.Compiler.Identifier (identifierText)

extendBoundWithPattern :: Pattern -> Set Text -> Set Text
extendBoundWithPattern patternValue bound =
  Set.union bound (patternBinderNames patternValue)

commonPatternBinderNames :: [Pattern] -> Set Text
commonPatternBinderNames alternatives =
  case alternatives of
    [] -> Set.empty
    firstAlternative : rest ->
      foldl'
        Set.intersection
        (patternBinderNames firstAlternative)
        (map patternBinderNames rest)

patternBinderNames :: Pattern -> Set Text
patternBinderNames patternValue =
  case patternValue of
    PVariable name -> Set.singleton (identifierText name)
    PWildcard -> Set.empty
    PLiteral {} -> Set.empty
    PConstructor _ patterns ->
      Set.unions (map patternBinderNames patterns)
    PList patterns ->
      Set.unions (map patternBinderNames patterns)
    PConsList headPattern tailPattern ->
      Set.union (patternBinderNames headPattern) (patternBinderNames tailPattern)
    PTuple patterns ->
      Set.unions (map patternBinderNames patterns)
    PAs name nestedPattern ->
      Set.insert (identifierText name) (patternBinderNames nestedPattern)
    POr alternatives ->
      commonPatternBinderNames alternatives
