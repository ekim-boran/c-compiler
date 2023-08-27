module Irgen.Lib where

import Control.Monad.State
import Data.Map qualified as M
import Data.Tuple.Extra (second)
import Ir.Types
import Irgen.Gen
import Irgen.GenUtil
import Irgen.Structs
import Language.C
import Simplify.Gen qualified as T
import Simplify.Types qualified as T

genTranslUnit :: CTranslUnit -> Either String TranslationUnit
genTranslUnit t@(CTranslUnit defs _) = do
  (T.TranslationUnit decls structs) <- T.checkTu t
  (TranslationUnit decls structs) <- evalStateT (TranslationUnit <$> (concat <$> traverse genExtDecl decls) <*> pure structs) emptyFunContext
  return $ TranslationUnit (fmap processStruct decls) structs
