module AsmGen.Ir where

import Control.Arrow (first)
import Data.Map qualified as M
import Ir.Types qualified as I
import Ir.Utils

processInstructions f bs = blocks'''
  where
    (replaceMap, blocks') = M.mapAccumWithKey (\accum bid block -> first (accum `M.union`) (passInstructions1 f bid block)) M.empty bs
    blocks'' = replaceOperand replaceMap bs
    (_, blocks''') = M.mapAccumWithKey (\accum bid block -> first (accum `M.union`) (passInstructions1 f bid block)) M.empty blocks''

passInstructions1 f bid (I.Block phis instrs exit) = (replaceMap', I.Block phis newinstrs exit)
  where
    (is, _) = foldl go ([], 0) (zip [0 ..] instrs)
    go (xs, counter) (i, line) = let xs' = f (I.Temp bid i) (I.Temp bid counter) line in (xs ++ xs', counter + length xs')
    newinstrs = snd <$> is
    replaceMap' = M.fromList $
      concat $ do
        (newindex, (maybeOldIndex, instr)) <- zip [0 ..] is
        case maybeOldIndex of
          Nothing -> return []
          (Just t) -> return [(t, I.Temp bid newindex)]

preProcess blocks = (processInstructions f blocks)
  where
    f oldIndex@(I.Temp bid index) newIndex@(I.Temp _ nindex) x@(I.Call op args dtype) =
      moveArgs ++ [(Nothing, I.Call op args' dtype)] ++ [(Just oldIndex, I.TypeCast (I.Register (I.Temp bid (nindex + length args)) dtype) dtype)]
      where
        (moveArgs, args') = unzip [((Nothing, I.TypeCast a (I.getDtype a)), I.Register (I.Temp bid i) (I.getDtype a)) | (i, a) <- zip [nindex ..] args]
    f temp _ x = [(Just temp, x)]
