module AsmGen.Ir where

import Control.Arrow (first)
import Data.Map qualified as M
import Ir.Types qualified as I
import Ir.Utils

--  converts temp = f() to temp = f(); temp1 = temp; for register allocation
--  TODO: refactor
convert oldIndex@(I.Temp bid index) newIndex@(I.Temp _ nindex) x@(I.Call op args dtype) =
  moveArgs ++ [(Nothing, I.Call op args' dtype)] ++ [(Just oldIndex, I.TypeCast (I.Register (I.Temp bid (nindex + length args)) dtype) dtype)]
  where
    (moveArgs, args') = unzip [((Nothing, I.TypeCast a (I.getDtype a)), I.Register (I.Temp bid i) (I.getDtype a)) | (i, a) <- zip [nindex ..] args]
convert temp _ x = [(Just temp, x)]

preProcess :: M.Map I.BlockId I.Block -> M.Map I.BlockId (I.Block)
preProcess blocks = blocks''
  where
    (replaceMap, blocks') = M.mapAccumWithKey (\accum bid block -> first (accum `M.union`) (passInstructions1 bid block)) M.empty blocks
    (_, blocks'') = M.mapAccumWithKey (\accum bid block -> first (accum `M.union`) (passInstructions1 bid block)) M.empty (replaceOperand replaceMap blocks)
    passInstructions1 bid (I.Block phis instrs exit) = (replaceMap', I.Block phis newinstrs exit)
      where
        (is, _) = foldl go ([], 0) (zip [0 ..] instrs)
        go (xs, counter) (i, line) = let xs' = convert (I.Temp bid i) (I.Temp bid counter) line in (xs ++ xs', counter + length xs')
        newinstrs = snd <$> is
        replaceMap' = M.fromList $
          concat $ do
            (newindex, (maybeOldIndex, instr)) <- zip [0 ..] is
            case maybeOldIndex of
              Nothing -> return []
              (Just t) -> return [(t, I.Temp bid newindex)]