module Opt.Loop where

import Control.Monad.State
import Data.Bifunctor (Bifunctor (second))
import Data.Either
import Data.Foldable (foldl', traverse_)
import qualified Data.Map as M
import qualified Data.Set as S
import Graph
import Ir.Types
import Ir.Utils (addBlockAfter, foldInstructions, makeCfg, removeInstructions, replaceOperand)
import Opt.Util
import Util

-- https://www.cs.cmu.edu/~aplatzer/course/Compilers11/17-loopinv.pdf

-- 1) find natural loops
-- add an if block - not execute the instruction if it is not neccessary
-- dominate loop exits? -- what if an if exists
-- it cannot throw exceptions -- division is not ok
-- speculative execution
--

naturalLoops cfg = (\(head, exit) -> (head, body (head, exit))) <$> xs
  where
    rCfg = reverseCfg cfg
    dom = makeDomTree cfg
    xs = groupWith [(to, from) | (from, tos) <- M.toList cfg, to <- tos, dominates dom to from]
    body (header, footers) = dfs (edges rCfg) footers (S.singleton header)
    dominates d x y
      | x == y = True
      | otherwise = maybe False (S.member x) (M.lookup y (domBy d))

licm :: FunctionDefinition -> FunctionDefinition
licm a@(FunctionDefinition allocs blocks initbid) = FunctionDefinition allocs blocks' initbid
  where
    cfg = makeCfg blocks
    loops = naturalLoops cfg
    blocks' = foldl' processLoop blocks loops

processLoop blocks (headerBid, loopBodyIds) =
  if null hoistCandidates
    then blocks
    else carryInstruction newBid blocks' hoistCandidates -- replaceOperand map blocks''
  where
    hoistCandidates = willHoist (M.filterWithKey (\k _ -> S.member k loopBodyIds) blocks)
    (newBid, blocks') = addBlockAfter headerBid blocks loopBodyIds

-- ((map, newinstrs), blocks'') = M.mapAccumWithKey (passInstructions newBid hoistCandidates) (M.empty, []) blocks'

willHoist loopBody = reverse $ foldInstructions go [] loopBody
  where
    go bid index Store {} set = set
    go bid index Load {} set = set
    go bid index Call {} set = set
    go bid index Nop {} set = set
    go bid index instr set = if and (fmap (mark set) instr) then (bid, index) : set else set

    mark set (Constant (GlobalVariable _ _)) = False
    mark set (Constant _) = True
    mark set (Register (Arg bid _) _) = M.notMember bid loopBody
    mark set (Register (Temp bid index) _) = M.notMember bid loopBody || (bid, index) `elem` set
    mark _ _ = False

carryInstruction targetBid blocks instrs =
  removeInstructions remove $ add $ replaceOperand map blocks
  where
    map = M.fromList $ [(Temp sid sindex, Temp targetBid i) | (i, (sid, sindex)) <- zip [pos ..] instrs]
      where
        pos = length $ instructions $ blocks M.! targetBid

    add blocks = M.adjust (\(Block p i e) -> Block p (i ++ xs) e) targetBid blocks
      where
        xs = [instructions (blocks M.! sid) !! sindex | (sid, sindex) <- instrs]

    remove bid index instr | (bid, index) `elem` instrs = False
    remove _ _ x = True

passInstructions ::
  BlockId ->
  S.Set (BlockId, Int) ->
  (M.Map RegisterId RegisterId, [Instruction]) ->
  BlockId ->
  Block ->
  ((M.Map RegisterId RegisterId, [Instruction]), Block)
passInstructions newbid set (replaceMap, xs) bid (Block phis instrs exit) =
  ((map'', xs ++ newbinstrs), Block phis insrs' exit)
  where
    markinstr = [if S.member (bid, index) set then Right (index, instr) else Left (index, instr) | (index, instr) <- zip [0 ..] instrs]

    (map', newbinstrs) = foldl' f (replaceMap, []) (zip [(length xs) ..] (rights markinstr))
    f (map, is) (newpos, (oldpos, i)) = (M.insert (Temp bid oldpos) (Temp newbid newpos) map, is ++ [i])

    (map'', insrs') = replaceInstrs map' bid $ lefts markinstr

replaceInstrs :: M.Map RegisterId RegisterId -> BlockId -> [(Int, b)] -> (M.Map RegisterId RegisterId, [b])
replaceInstrs replaceMap bid instrs = (M.union replaceMap $ M.fromList replaceList, instrs')
  where
    (replaceList, instrs') = unzip [((Temp bid oldIndex, Temp bid index), inst) | (index, (oldIndex, inst)) <- zip [0 ..] instrs]
