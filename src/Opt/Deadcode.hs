module Opt.Deadcode where

import Data.Bifunctor (second)
import Data.Either (isRight)
import Data.List
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Ir.Types
import Ir.Utils (foldOperands, removeInstructions, removeAllocs)

deadcode :: FunctionDefinition -> FunctionDefinition
deadcode def@(FunctionDefinition allocs blocks init) = newdef
  where
    used = usedOperands blocks
    blocks' = removeInstructions f blocks
    newdef = removeAllocs falloc (FunctionDefinition allocs blocks' init)
      where
        falloc alloc = S.member alloc used

    f _ _ x@Store {} = True
    f _ _ x@Call {} = True
    f _ _ x@Nop {} = False
    f bid index instr
      | S.member (Temp bid index) used = True
      | otherwise = False

usedOperands :: M.Map k Block -> S.Set RegisterId
usedOperands = foldOperands f S.empty
  where
    f :: S.Set RegisterId -> Operand -> S.Set RegisterId
    f set (Constant c) = set
    f set (Register rid _) = S.insert rid set
