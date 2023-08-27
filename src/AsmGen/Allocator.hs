module AsmGen.Allocator where

import Asm.Types (reg)
import Asm.Types qualified as A
import Control.Monad
import Control.Monad.Trans.Accum qualified as M
import Data.Foldable
import Data.Graph (Graph)
import Data.IntMap.Strict qualified as IM
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.Tuple.Extra (first)
import GHC.Arr (array, listArray)
import GHC.Arr qualified as A
import Graph
import Ir.Types
import Ir.Utils (foldInstructions, getJumpArgs, makeCfg, neigbours)
import Util (runUntilNoChange, (!!!))
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

rids bid i (Register rid d) = [(Rid rid, d)]
rids _ _ _ = []

liveness (FunctionDefinition {..}) = reverseFlow blocks go -- (\a b c -> go a b c . S.filter (is))
  where
    go bid index (I x) set = S.delete (Rid (Temp bid index), getDtype x) $ foldl (flip S.insert) set $ concatMap (rids bid index) x
    go bid index (P pindex d) set = S.insert (TempPhi bid pindex, getDtype d) $ S.delete (Rid (Arg bid pindex), getDtype d) set
    go bid index (E x) set = foldl (flip S.insert) set $ concatMap (rids bid index) x
    go bid index (PreE x) set = foldl (flip S.delete) set $ phis x
    phis e = concatMap go $ getJumpArgs e
      where
        go (JumpArg bid xs) = [(TempPhi bid i, getDtype d) | (i, d) <- zip [0 ..] xs]

initialAssignment edges fd@(FunctionDefinition allocations blocks initBid) = M.assocs $ foldl (\m (k, v) -> M.adjust (const v) k m) all functionCalls
  where
    all = M.fromList (go <$> edges)
    go (rid@(TempPhi b i), d) | b == initBid = (rid, S.singleton $ reg i d)
    go (rid, d) = (rid, regs d)

    functionCalls = foldInstructions f [] blocks
    f bid i (Call calee args dtype) mapping =
      [(Rid $ Temp bid i, S.singleton (reg 0 dtype))]
        ++ [(Rid rid, S.singleton $ reg i d) | (i, Register rid d) <- zip [0 ..] args]
        ++ mapping
    f bid i _ mapping = mapping

regs DFloat {} = S.fromList $ (A.Arg A.RFloatingPoint <$> [0 .. 6]) ++ (A.Temp A.RFloatingPoint <$> [0 .. 11]) ++ (A.Saved A.RFloatingPoint <$> [0 .. 11])
regs _ = S.fromList $ (A.Arg A.RInteger <$> [0 .. 5]) ++ (A.Temp A.RInteger <$> [0 .. 6]) ++ (A.Saved A.RInteger <$> [1 .. 11])

--

solveCSP map initAssignment = M.map (head . S.toList) $ M.fromList $ it [initAssignment]
  where
    it [] = error "x"
    it (x : xs)
      | finished x = x
      | otherwise =
          let (cur, rest) = expand x
           in case process graph (to cur) of
                Nothing -> it (rest : xs)
                (Just x) -> it (from x : (rest : xs))
    (graph, to, from) = mapToArray map
    mapToArray map = (IM.fromList [(reverseMap M.! k, S.toList $ S.map (reverseMap M.!) s) | (k, s) <- M.assocs map], listToArray, arrayToList)
      where
        listToArray = IM.fromList . fmap (first (reverseMap M.!))
        arrayToList = fmap (first (lookupMap IM.!)) . IM.assocs
        size = M.size map
        lookupMap = IM.fromList $ zip [0 ..] (M.keys map)
        reverseMap = M.fromList $ zip (M.keys map) [0 ..]
    finished = all ((== 1) . S.size . snd)
    invalid = any (S.null . snd)
    expand sol = ((k, S.singleton a) : l ++ vs, l ++ [(k, rest)] ++ vs)
      where
        (l, (k, v) : vs) = span ((<= 1) . S.size . snd) $ sortOn (S.size . snd) sol
        (a, rest) = S.deleteFindMin v
    process :: Ord a => IM.IntMap [IM.Key] -> IM.IntMap (S.Set a) -> Maybe (IM.IntMap (S.Set a))
    process graph = helper (\sol' -> foldM f sol' keys)
      where
        keys = IM.keys graph
        f sol' n = if S.size new == 0 then Nothing else Just $ IM.insert n (S.difference set nbrs) sol'
          where
            set = sol' IM.! n
            new = S.difference set $ nbrs
            nbrs = S.unions $ filter ((== 1) . S.size) $ (sol' IM.!) <$> (graph IM.! n)
        helper f d = case f d of
          Nothing -> Nothing
          (Just result) -> if IM.map S.size d /= IM.map S.size result then helper f result else Just result

data Flow = I Instruction | E BlockExit | PreE BlockExit | P Int Dtype

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
    initial cfg map bid = mconcat $ [s | bid' <- edges cfg bid, (Just map') <- [M.lookup bid' map], (Just (s, _)) <- [M.minView map']]
