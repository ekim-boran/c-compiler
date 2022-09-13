module AsmGen.Allocator where

import Asm.Types (reg)
import Asm.Types qualified as A
import AsmGen.ConstraintSatisfaction
import AsmGen.DataFlow
import Data.Foldable
import Data.Graph (Graph, edges)
import Data.List (nub, sortOn)
import Data.Map qualified as M
import Data.Set qualified as S
import Graph
import Ir.Types
import Ir.Utils (foldInstructions, getJumpArgs)
import Util
import Prelude hiding ((!!))

data LivenessKey
  = Rid RegisterId
  | TempPhi BlockId Int
  | TempConstant BlockId Int Constant
  | TempLocal BlockId Int Int -- bid line localId
  deriving (Show, Ord, Eq)

mkAllocator x = (solveCSP (mkGraph l) (initialAssignment (allElems l) x), l)
  where
    l = liveness x

mkGraph livenessMap =
  (M.map S.fromList . createGraph xs . groupWith . nub . concat)
    [[(a, b), (b, a)] | m <- M.elems livenessMap, s <- M.elems m, (a, _) <- S.toList s, (b, _) <- S.toList s, a /= b]
  where
    xs = fst <$> allElems livenessMap

allElems livenessMap = nub $ [a | m <- M.elems livenessMap, s <- M.elems m, a <- S.toList s]

-- is (TempConstant {}, _) = False
-- is (TempLocal {}, _) = False
-- is _ = True

rids bid i (Register rid d) = [(Rid rid, d)]
rids _ _ _ = []

-- rids bid i (Register (Local lid) d) = [(TempLocal bid i lid, d)]
-- rids bid i x@(Constant c) = [(TempConstant bid i c, getDtype x)]

liveness (FunctionDefinition {..}) = reverseFlow blocks (go) -- (\a b c -> go a b c . S.filter (is))
  where
    go bid index (I x) set = S.delete ((Rid (Temp bid index), (getDtype x))) $ foldl (\s op -> S.insert op s) set $ concatMap (rids bid index) x
    go bid index (P pindex d) set = S.insert ((TempPhi bid pindex, (getDtype d))) $ S.delete ((Rid (Arg bid pindex), (getDtype d))) set
    go bid index (E x) set = foldl (\s op -> S.insert op s) set $ concatMap (rids bid index) x
    go bid index (PreE x) set = foldl (\s op -> S.delete op s) set $ phis x
    phis e = concatMap go $ getJumpArgs e
      where
        go (JumpArg bid xs) = [(TempPhi bid i, (getDtype d)) | (i, d) <- zip [0 ..] xs]

initialAssignment edges fd@(FunctionDefinition allocations blocks initBid) = M.assocs $ foldl (\m (k, v) -> M.adjust (\_ -> v) k m) all functionCalls
  where
    all = M.fromList (go <$> edges)
    go (rid@(TempPhi b i), d) | b == initBid = (rid, S.singleton $ reg i d)
    go (rid, d) = (rid, regs d)

    functionCalls = foldInstructions f [] blocks
    f bid i (Call calee args dtype) mapping =
      [((Rid $ Temp bid i), S.singleton (reg 0 dtype))]
        ++ [((Rid rid), S.singleton $ reg i d) | (i, Register rid d) <- zip [0 ..] args]
        ++ mapping
    f bid i _ mapping = mapping

regs DFloat {} = S.fromList $ (A.Arg A.RFloatingPoint <$> [0 .. 6]) ++ (A.Temp A.RFloatingPoint <$> [0 .. 11]) ++ (A.Saved A.RFloatingPoint <$> [0 .. 11])
regs _ = S.fromList $ (A.Arg A.RInteger <$> [0 .. 5]) ++ (A.Temp A.RInteger <$> [0 .. 6]) ++ (A.Saved A.RInteger <$> [1 .. 11])
