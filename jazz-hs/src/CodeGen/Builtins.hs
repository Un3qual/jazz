module CodeGen.Builtins where

import Language.QBE
import Data.List.NonEmpty (NonEmpty( (:|) ))

-- builtin functions
-- [Linkage] (Maybe AbiTy) (Ident 'Global) (Maybe (Ident 'Temporary)) [Param] Variadic (NonEmpty Block)	 
-- jzAdd :: FuncDef
-- jzAdd = FuncDef 