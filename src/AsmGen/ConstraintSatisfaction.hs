module AsmGen.ConstraintSatisfaction where

import Control.Monad
import Data.IntMap.Strict qualified as IM
import Data.List (sortOn)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Tuple.Extra (first)
import Debug.Trace
import GHC.Arr (array, listArray)
import GHC.Arr qualified as A
import Util (runUntilNoChange, (!!!))

finished = all ((== 1) . S.size . snd)

invalid = any (S.null . snd)

expand sol = ((k, S.singleton a) : l ++ vs, l ++ [(k, rest)] ++ vs)
  where
    (l, (k, v) : vs) = span ((<= 1) . S.size . snd) $ sortOn (S.size . snd) sol
    (a, rest) = S.deleteFindMin v

r f d = case f d of
  Nothing -> Nothing
  (Just result) -> if IM.map (S.size) d /= IM.map (S.size) result then r f result else Just result

process graph = r (\sol' -> foldM f sol' keys)
  where
    keys = (IM.keys graph)
    f sol' n = if S.size new == 0 then Nothing else Just $ IM.insert n (S.difference set $ nbrs) sol'
      where
        set = sol' IM.! n
        new = (S.difference set $ nbrs)
        nbrs = S.unions $ filter ((== 1) . S.size) $ (sol' IM.!) <$> (graph IM.! n)

solveCSP map initAssignment = M.map (head . S.toList) $ M.fromList $ it [initAssignment]
  where
    it [] = error "x"
    it (x : xs)
      | finished x = x
      | otherwise =
          let (cur, rest) = expand x
           in case process graph (to cur) of
                Nothing -> it (rest : xs)
                (Just x) -> it ((from x) : (rest : xs))
    (graph, to, from) = mapToArray map

mapToArray map = (IM.fromList [(reverseMap M.! k, S.toList $ S.map (reverseMap M.!) s) | (k, s) <- M.assocs map], listToArray, arrayToList)
  where
    listToArray = IM.fromList . fmap (first (reverseMap M.!))
    arrayToList = fmap (first (lookupMap IM.!)) . IM.assocs
    size = M.size map
    lookupMap = IM.fromList $ zip [0 ..] (M.keys map)
    reverseMap = M.fromList $ zip (M.keys map) [0 ..]