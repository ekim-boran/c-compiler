{-# LANGUAGE FunctionalDependencies #-}

module Graph where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (foldl', foldl1', groupBy, sort)
import Data.Map qualified as M
import Data.Maybe
import Data.Ord
import Data.Set qualified as S
import Util (runUntilNoChange)

type Cfg a = M.Map a [a]

createGraph nodes edges = fromTuples $ ((,[]) <$> nodes) ++ edges

fromEdges edges = fromTuples $ ((,[]) <$> nodes) ++ edges
  where
    nodes = S.toList $ foldl (\s (x, xs) -> s `S.union` S.fromList (x : xs)) S.empty edges

fromTuples = M.fromListWith (flip (++)) . sort

groupWith = M.toList . fromTuples . fmap (second pure)

nodes = M.keys

edges :: (Ord a, Monoid m) => M.Map a m -> a -> m
edges cfg bid = fromMaybe mempty $ M.lookup bid cfg

-- why first part? include edges where tos is empty like return blocks
reverseCfg cfg = fromTuples $ [(n, []) | n <- nodes cfg] ++ [(to, [from]) | (from, tos) <- M.toList cfg, to <- tos]

reachable :: (Ord a) => Cfg a -> a -> S.Set a
reachable cfg bid = dfs (edges cfg) [bid] S.empty

wierddfs :: (Eq a, Ord a) => (a -> [a]) -> a -> S.Set a -> S.Set a
wierddfs edges bid s = go s [bid]
  where
    go visited [] = visited
    go visited (x : xs) = go (foldr S.insert visited (edges x)) (filter (`S.notMember` visited) (edges x) ++ xs)

dfs :: (Eq a, Ord a) => (a -> [a]) -> [a] -> S.Set a -> S.Set a
dfs edges bid s = go s bid
  where
    go visited [] = visited
    go visited (x : xs)
      | S.member x visited = go visited xs
      | otherwise = go (S.insert x visited) (filter (not . flip S.member visited) (edges x) ++ xs)

postOrder' graph = repeat ([], S.empty)
  where
    nextNode visited = head $ filter (`S.notMember` visited) $ nodes graph
    repeat (xs, visited) = if M.size graph == length xs then xs else repeat (go (xs, visited) [(nextNode visited, False)])
    go acc [] = acc
    go (ret, visited) ((x, completed) : xs)
      | completed = go (ret ++ [(x, edges graph x)], visited) xs
      | S.member x visited = go (ret, visited) xs
      | otherwise =
          let visited' = S.insert x visited
           in go (ret, visited') (((,False) <$> filter (not . flip S.member visited') (edges graph x)) ++ [(x, True)] ++ xs)

postOrder = fmap fst . postOrder'

-------------------------------------------------------------------------------------------------
-- domination

data DomTree a = DomTree
  { domBy :: M.Map a (S.Set a),
    immDomBy :: M.Map a [a],
    frontierMap :: M.Map a [a]
  }
  deriving (Show)

frontiers :: Ord a => DomTree a -> a -> [a]
frontiers (DomTree {..}) node = fromMaybe [] $ M.lookup node frontierMap

idoms :: Ord a => DomTree a -> a -> [a]
idoms (DomTree {..}) node = fromMaybe [] $ M.lookup node immDomBy

idomsRec :: (Ord a) => DomTree a -> a -> [a]
idomsRec d bid = bid : [y | x <- idoms d bid, y <- idomsRec d x]

makeDomTree :: (Ord a, Show a) => M.Map a [a] -> DomTree a
makeDomTree cfg = DomTree domainatedByMap immDomainatedByMap frontierMap
  where
    rCfg = reverseCfg cfg

    domainatedByMap = M.mapWithKey S.delete $ runUntilNoChange (flip (foldl' go) rpo) init
      where
        rpo = postOrder' rCfg
        init = M.fromList [(n, S.fromList $ if null prevs then [n] else nodes cfg) | (n, prevs) <- rpo]
        go doms (nodeId, prevIds) =
          M.insert nodeId (S.insert nodeId $ foldl1' S.intersection $ mapMaybe (`M.lookup` doms) (nodeId : prevIds)) doms

    immDomainatedByMap = M.mapWithKey (\k v -> take 1 $ filter (`S.member` v) order) domainatedByMap
      where
        order = postOrder cfg

    frontierMap = reverseCfg $ M.mapWithKey (concatMap . go) rCfg
      where
        dominates x y = maybe False (S.member x) (M.lookup y domainatedByMap)
        go node prev
          | dominates prev node = []
          | otherwise = prev : [y | x <- fromMaybe [] (M.lookup prev immDomainatedByMap), y <- go node x]
