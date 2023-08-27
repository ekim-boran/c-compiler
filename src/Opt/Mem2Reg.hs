module Opt.Mem2Reg where

import Control.Monad (join, mfilter)
import Data.List as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Graph
import Ir.Types
import Ir.Utils
import Opt.Util
import Util

mem2Reg :: FunctionDefinition -> FunctionDefinition
mem2Reg a@(FunctionDefinition allocs blocks initbid) = FunctionDefinition allocs blocks' initbid
  where
    fold f acc bid block = foldl (f bid) acc $ zip [0 ..] $ instructions block
    mem2Reg = mkMem2Reg blocks initbid
    mem2Reg1 = M.foldlWithKey' (fold localPass) mem2Reg blocks
    mem2Reg2 = M.foldlWithKey' (fold globalPass) mem2Reg1 blocks
    phiNodes = lastPass mem2Reg2
    typedPhiNodes = groupWith $ sort $ [(bid, (aid, allocs  !! aid, xs)) | (bid, (aid, xs)) <- phiNodes]
    blocks' = M.map noops $ addPhiNodes typedPhiNodes (replaces mem2Reg2) blocks
    noops :: Block -> Block
    noops (Block a inst b) = Block a ((\i -> fromMaybe i $ pass mem2Reg2 i (const Nop) (const $ const Nop)) <$> inst) b

data OperandValue
  = Value Operand
  | Phi Int BlockId
  | UndefVal
  deriving (Show, Eq, Ord)

data Mem2Reg = Mem2Reg
  { rCfg :: Cfg BlockId,
    domTree :: DomTree BlockId,
    isPromotable :: LocalId -> Bool,
    isPhiCandidate :: LocalId -> BlockId -> Bool,
    lastValues :: M.Map (Int, BlockId) OperandValue,
    replaces :: M.Map RegisterId OperandValue,
    phiSet :: S.Set (Int, BlockId)
  }

mkMem2Reg :: M.Map BlockId Block -> BlockId -> Mem2Reg
mkMem2Reg blocks initbid =
  Mem2Reg rCfg domTree isPromotable isPhiCandidate M.empty M.empty S.empty --
  where
    rCfg = reverseCfg (makeCfg blocks)
    domTree = makeDomTree (makeCfg blocks)
    impromotable = markImpromotable blocks
    isPromotable loc = S.notMember loc impromotable

    promotableStores = M.filterWithKey (\k a -> S.notMember k impromotable) (markStores blocks)
    iteratedDomFrontier = M.map (foldr (wierddfs (frontiers domTree)) S.empty) promotableStores
    isPhiCandidate aid bid = maybe False (S.member bid) (M.lookup aid iteratedDomFrontier)

markImpromotable :: M.Map BlockId Block -> S.Set LocalId
markImpromotable blocks = S.fromList [cs | block <- M.elems blocks, inst <- instructions block, cs <- mark inst]
  where
    mark (Store ptr value) = maybeToList $ getLocal value
    mark (Load a) = []
    mark ins = concatMap (maybeToList . getLocal) ins

markStores :: M.Map BlockId Block -> M.Map LocalId [BlockId]
markStores = rev . M.map (nub . mapMaybe mark . instructions)
  where
    mark (Store ptr value) = getLocal ptr
    mark x = Nothing
    rev xs = fromTuples $ [(to, [from]) | (from, tos) <- M.toList xs, to <- tos]

addLastValue :: (Int, BlockId) -> OperandValue -> Mem2Reg -> Mem2Reg
addLastValue loc op m = m {lastValues = M.insert loc op (lastValues m)}

addReplaceEntry :: RegisterId -> OperandValue -> Mem2Reg -> Mem2Reg
addReplaceEntry rid op m = m {replaces = M.insert rid op (replaces m)}

insertPhiNode :: Int -> BlockId -> Mem2Reg -> Mem2Reg
insertPhiNode aid rid m = m {phiSet = S.insert (aid, rid) (phiSet m)}

getLastValue :: Int -> BlockId -> Mem2Reg -> Maybe OperandValue
getLastValue aid bid m = M.lookup (aid, bid) (lastValues m)

valueToOperand' :: [((BlockId, Int), Int)] -> OperandValue -> Dtype -> Operand
valueToOperand' indexes (Value o) = const o
valueToOperand' indexes (Phi aid bid) = Register (Arg bid (fromMaybe undefined $ lookup (bid, aid) indexes))
valueToOperand' indexes UndefVal = Constant . Undef

replace replaceMap x@(Register rid d) = case M.lookup rid replaceMap of
  Nothing -> x
  (Just f) -> replace replaceMap $ f d
replace _ x = x

addArguments typedPhiFinal indexes blocks = foldl' f blocks xs
  where
    xs = [(bid, sourceid, valueToOperand' indexes val (item dtype)) | (bid, other) <- typedPhiFinal, (aid, dtype, xs) <- other, (sourceid, val) <- xs]

    f map (bid, sid, val) = M.adjust (\(Block x y exit) -> Block x y (applyOp' g exit)) sid map
      where
        g (JumpArg bid1 ops) | bid == bid1 = JumpArg bid1 (ops ++ [val])
        g x = x

addParameters typedPhiFinal indexes bid (Block phis a b) =
  let newBlock = Block (phis ++ tys) a b
      indexes' = [((bid, aid), index) | (aid, index) <- zip aids [length phis ..]]
   in (indexes' ++ indexes, newBlock)
  where
    (aids, tys, _) = unzip3 (fromMaybe [] $ lookup bid typedPhiFinal)

addPhiNodes phiFinal replaceMap blocks =
  let (indexes, blocks1) = M.mapAccumWithKey (addParameters phiFinal) [] blocks
      blocks2 = addArguments phiFinal indexes blocks1
      newReplaces = M.map (valueToOperand' indexes) replaceMap
   in M.map (fmap (replace newReplaces)) blocks2

pass :: Mem2Reg -> Instruction -> (LocalId -> a) -> (Operand -> Int -> a) -> Maybe a
pass mem2Reg instr processLoad processStore =
  case instr of
    Store ((Register (Local i) d)) value | isPromotable mem2Reg i -> Just $ processStore value i
    Load ((Register (Local i) d)) | isPromotable mem2Reg i -> Just $ processLoad i
    _ -> Nothing

localPass :: BlockId -> Mem2Reg -> (Int, Instruction) -> Mem2Reg
localPass bid m (index, instr) = fromMaybe m $ pass m instr load store
  where
    load aid = case go m aid bid of
      Nothing -> m
      (Just (Right val)) -> addReplaceEntry (Temp bid index) val m -- local value
      (Just (Left bid)) -> addLastValue (aid, bid) (Phi aid bid) $ insertPhiNode aid bid $ addReplaceEntry (Temp bid index) (Phi aid bid) m -- it is a join point
    store value aid = addLastValue (aid, bid) (Value value) m

go m aid bid =
  case M.lookup (aid, bid) (lastValues m) of
    Nothing -> if isPhiCandidate m aid bid then Just $ Left bid else Nothing
    (Just x) -> Just $ Right x

domValues aid bid m = fmap (go m aid) (idomsRec (domTree m) bid)

globalPass :: BlockId -> Mem2Reg -> (Int, Instruction) -> Mem2Reg
globalPass bid m (index, i) = fromMaybe m $ pass m i handleLoad (\_ _ -> m)
  where
    handleLoad aid
      | M.member (Temp bid index) (replaces m) = m -- already replaced with phinode or local
      | otherwise =
          case join $ find isJust $ drop 1 $ domValues aid bid m of -- skip current block
            Nothing -> addReplaceEntry (Temp bid index) UndefVal m
            (Just (Right val)) -> addReplaceEntry (Temp bid index) val m
            (Just (Left domBid)) -> addLastValue (aid, domBid) (Phi aid domBid) $ addReplaceEntry (Temp bid index) (Phi aid domBid) $ insertPhiNode aid domBid m

lastPass m = go m (phiSet m) (S.toList $ phiSet m)
  where
    go x _ [] = []
    go m visited ((aid, bid) : xs) =
      let (new, visited', as, m') = foldl' (f aid) ([], visited, [], m) (edges (rCfg m) bid)
       in (bid, (aid, as)) : go m' visited' (xs ++ new)

    f aid (new, visited, as, m) prevBlockId =
      case join $ find isJust $ domValues aid prevBlockId m of
        Nothing -> (new, visited, (prevBlockId, UndefVal) : as, m)
        (Just (Right val)) -> (new, visited, (prevBlockId, val) : as, m)
        (Just (Left jp)) ->
          let newstack = if S.member (aid, jp) visited then new else (aid, jp) : new
           in (newstack, S.insert (aid, jp) visited, (prevBlockId, Phi aid jp) : as, addLastValue (aid, jp) (Phi aid jp) m)
