module Jazz.AST.Declaration where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))

data Module = Module {
  modulePath :: FilePath
, moduleName :: Text
-- , moduleBindings :: [NonEmpty Binding]
-- , moduleTypeDeclarations :: [TypeDeclaration]
-- , moduleExterns :: [Extern]
} deriving (Show, Eq, Ord)

-- data Binding = Binding {
--   bindingName :: !(Located IdentName)
-- , bindingType :: !Type
-- , bindingArgs :: ![Typed (Located IdentName)]
-- -- , bindingReturnType :: Type
-- , bindingBody :: Expr
-- } deriving (Show, Eq)

-- data Extern = Extern {
--   externName :: Text
-- } deriving (Show, Eq)