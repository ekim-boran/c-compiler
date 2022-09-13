{-# LANGUAGE LambdaCase #-}

module Opt.SimplifyCfg where

import Control.Lens
import Control.Monad (join)
import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Graph
import Ir.Types
import Ir.Utils
import Opt.Util
import Util

simplifyCfg :: FunctionDefinition -> FunctionDefinition
simplifyCfg = runUntilNoChange emptyBlocks . runUntilNoChange mergeBlocks . runUntilNoChange removeUnreachable . runUntilNoChange constantProp

constantProp :: FunctionDefinition -> FunctionDefinition
constantProp = over lblocks (M.map (over lexit constantProp'))
  where
    constantProp' :: BlockExit -> BlockExit
    constantProp' (ConditionalJump o l r) | l == r = Jump l
    constantProp' (ConditionalJump (Constant (Int value _ _)) l r)
      | value == 0 = Jump r
      | value == 1 = Jump l
    constantProp' (Switch o def xs)
      | and $ fmap ((def ==) . snd) xs = Jump def
    constantProp' (Switch (Constant c) def xs) = case lookup c xs of
      Nothing -> Jump def
      (Just t) -> Jump t
    constantProp' x = x

emptyBlocks :: FunctionDefinition -> FunctionDefinition
emptyBlocks f@(FunctionDefinition {..}) = over lblocks (M.map (over lexit go)) f
  where
    empties = M.map exit $ M.filter (\b -> null (instructions b) && null (phinodes b)) blocks

    isEmptyJump arg@(JumpArg bid xs) = case M.lookup bid empties of
      (Just (Jump targetArg)) -> targetArg
      _ -> arg

    go arg@(Jump (JumpArg bid xs)) = fromMaybe arg (M.lookup bid empties)
    go (ConditionalJump o arg1 arg2) = ConditionalJump o (isEmptyJump arg1) (isEmptyJump arg2)
    go (Switch o def xs) = Switch o (isEmptyJump def) (second isEmptyJump <$> xs)
    go x = x

removeUnreachable :: FunctionDefinition -> FunctionDefinition
removeUnreachable (FunctionDefinition allocs blocks bid) = FunctionDefinition allocs newblocks bid
  where
    nodes = reachable (makeCfg blocks) bid
    newblocks = M.filterWithKey (\bid block -> S.member bid nodes) blocks

-- why postorder ?
-- if we merge (2,3 ) and (3, 4) in this order it will be error we must first process (3, 4) then (2, 3)
mergeBlocks :: FunctionDefinition -> FunctionDefinition
mergeBlocks (FunctionDefinition allocs blocks bid) = FunctionDefinition allocs newblocks bid
  where
    cfg = makeCfg blocks
    reversedCfg = reverseCfg cfg
    mergeList = [(s, [t]) | (s, [t]) <- M.toList cfg, (rs, [rt]) <- M.toList reversedCfg, s == rt && t == rs]
    order = [(x, t) | (x, [t]) <- postOrder' (fromEdges mergeList)] -- process from last
    newblocks = foldl' merge blocks (order)

merge blocks' (from, to) = M.map (fmap replacer) $ (M.delete to . M.insert from newblock) blocks'
  where
    (newblock, replacer) = merge' from (blocks' M.! from) (blocks' M.! to)

    merge' sourceBid (Block phis1 instrs1 (Jump (JumpArg targetBid xs))) (Block phis2 instrs2 exit2) =
      (Block phis1 (instrs1 ++ instrs2) exit2, replacer)
      where
        phis = zipWith3 (\i (Named _ dtype) arg -> (Register (Arg targetBid i) dtype, arg)) [0 ..] phis2 xs
        local = [(Register (Temp targetBid i) (getDtype ins), Register (Temp sourceBid (i + length instrs1)) (getDtype ins)) | (i, ins) <- zip [0 ..] instrs2]
        replacer o = fromMaybe o (lookup o (phis ++ local))
    merge' _ _ _ = error "cannot merge"