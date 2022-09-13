module AsmGen.DataFlow where

import Control.Monad.Trans.Accum qualified as M
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Graph
import Ir.Types
import Ir.Utils (makeCfg, neigbours)
import Util (runUntilNoChange)

data Flow = I Instruction | E BlockExit | PreE BlockExit | P Int Dtype

initial cfg map bid = mconcat $ [s | bid' <- edges cfg bid, (Just map') <- [M.lookup bid' map], (Just (s, _)) <- [M.minView map']]

reverseFlow :: (Monoid m, Eq m) => M.Map BlockId Block -> (BlockId -> Int -> Flow -> m -> m) -> M.Map BlockId (M.Map Int m)
reverseFlow blocks f = runUntilNoChange go M.empty
  where
    cfg = makeCfg blocks
    go map = M.foldlWithKey (\map bid block -> M.insert bid (flow f bid block $ initial cfg map bid) map) map blocks

flow :: (Monoid m) => (BlockId -> Int -> Flow -> m -> m) -> BlockId -> Block -> m -> M.Map Int m
flow f bid (Block phi instr exit) initial = M.fromList [(i, x) | ((i, _), x) <- reverse (zip ys xs)]
  where
    xs = tail $ scanl (\acc (index, item) -> f bid index item acc) initial ys
    ys = reverse $ zip [(-length phi) ..] ([P i d | (i, d) <- zip [0 ..] (item <$> phi)] ++ ((I <$> instr) ++ [PreE exit, E exit]))