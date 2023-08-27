module Ir.Utils where

import Control.Lens
import Data.Bifunctor
import Data.Bits
import Data.List as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Ir.Types
import Language.C.Syntax.Ops

lexit :: Lens' Block BlockExit
lexit = lens exit (\b e -> b {exit = e})

lblocks :: Lens' FunctionDefinition (Blocks Operand)
lblocks = lens blocks (\b e -> b {blocks = e})

makeCfg :: M.Map BlockId Block -> M.Map BlockId [BlockId]
makeCfg = M.map (neigbours . exit)

neigbours :: BlockExit -> [BlockId]
neigbours (Jump j) = [jbid j]
neigbours (ConditionalJump o l r) = [jbid l, jbid r]
neigbours (Switch o def xs) = jbid def : fmap (jbid . snd) xs
neigbours (Return o) = []
neigbours Unreachable = []

foldOperands f = M.foldl' (foldl f)

foldInstructions :: (BlockId -> Int -> Instruction -> a -> a) -> a -> M.Map BlockId Block -> a
foldInstructions f acc blocks = fst $ foldMapInstructions (\bid index i acc -> (i, f bid index i acc)) acc blocks

mapInstructions :: (BlockId -> Int -> Instruction -> Instruction) -> M.Map BlockId Block -> M.Map BlockId Block
mapInstructions f blocks = snd $ foldMapInstructions (\bid index i acc -> (f bid index i, acc)) [] blocks

foldMapInstructions :: (BlockId -> Int -> Instruction -> a -> (Instruction, a)) -> a -> M.Map BlockId Block -> (a, M.Map BlockId Block)
foldMapInstructions f acc blocks =
  M.mapAccumWithKey go acc blocks
  where
    go acc bid (Block {..}) =
      let (is, a) = foldl (\(xs, acc) (i, instr) -> first (: xs) (f bid i instr acc)) ([], acc) $ zip [0 ..] instructions
       in (a, Block phinodes (reverse is) exit)

applyOp' f x = runIdentity (applyOp (pure . f) x)

getJumpArgs :: BlockExit -> [JumpArg]
getJumpArgs (Jump j) = [j]
getJumpArgs (ConditionalJump o x y) = [x, y]
getJumpArgs (Switch o def xs) = def : (snd <$> xs)
getJumpArgs Unreachable = []
getJumpArgs (Return o) = []

applyOp :: Applicative f => (JumpArg -> f JumpArg) -> BlockExit -> f BlockExit
applyOp f (Jump j) = Jump <$> f j
applyOp f (ConditionalJump o a b) = ConditionalJump o <$> f a <*> f b
applyOp f (Switch o def xs) = Switch o <$> f def <*> traverse (\(a, b) -> (a,) <$> f b) xs
applyOp f Unreachable = pure Unreachable
applyOp f (Return o) = pure $ Return o

addBlockAfter afterId blocks except = (newBid,) $ M.insert newBid newBlock $ changeExit newBid afterId except blocks
  where
    newBid = newBlockId blocks
    (Block phis _ _) = blocks M.! afterId
    newBlock = Block phis [] (Jump (JumpArg afterId jumpArgs))
    jumpArgs = [Register (Arg newBid index) d | (index, Named _ d) <- zip [0 ..] phis]

newBlockId blocks = BlockId $ head $ filter ((`notElem` M.keysSet blocks) . BlockId) [0 ..]

changeExit newId oldId except = M.mapWithKey (\bid block -> if S.member bid except then block else block {exit = applyOp' change (exit block)})
  where
    change (JumpArg bid xs)
      | bid == oldId = JumpArg newId xs
      | otherwise = JumpArg bid xs

replaceOperand :: Functor f => M.Map RegisterId RegisterId -> M.Map k (f Operand) -> M.Map k (f Operand)
replaceOperand map = M.map (fmap (replace map))
  where
    replace replaceMap r@(Register Arg {} dtype) = r
    replace replaceMap r@(Register rid dtype) = case M.lookup rid replaceMap of
      Nothing -> r
      (Just rid') -> Register rid' dtype
    replace replaceMap c = c

replaceOperand' map = M.map (fmap (\r -> fromMaybe r (M.lookup r map)))

removeAllocs f (FunctionDefinition {..}) = FunctionDefinition allocs' (replaceOperand replaceMap' blocks) bid_init
  where
    (replaceMap', allocs') = replaceAllocs f allocations
    replaceAllocs f allocs = (M.fromList replaceMap, allocs')
      where
        xs = [(oldindex, alloc) | (oldindex, alloc) <- zip [0 ..] allocs, f (Local oldindex)]
        (replaceMap, allocs') = unzip [((Local oldIndex, Local index), alloc) | (index, (oldIndex, alloc)) <- zip [0 ..] xs]

removeInstructions f bs = replaceOperand (M.fromList replaceMap) blocks'
  where
    (replaceMap, blocks') = M.mapAccumWithKey (\accum bid block -> first (accum ++) (go bid block)) [] bs
    go bid (Block phis instrs exit) = (replaceMap, Block phis (snd <$> xs) exit)
      where
        xs = [(oldIndex, instr) | (oldIndex, instr) <- zip [0 ..] instrs, f bid oldIndex instr]
        replaceMap = [(Temp bid oldIndex, Temp bid newIndex) | (newIndex, (oldIndex, instr)) <- zip [0 ..] xs]

class ReplaceDtype a where
  replaceDType' :: Int -> (Dtype -> Dtype) -> a -> a
  replaceDType :: (Dtype -> Dtype) -> a -> a
  replaceDType' d = replaceDType
  replaceDType = replaceDType' 1 -- structs are recursive so we specify depth

instance ReplaceDtype Declaration where
  replaceDType f (Variable dtype init) = Variable (replaceDType f dtype) init
  replaceDType f (Function s (FunctionDefinition {..})) =
    let (DFunction s') = replaceDType f (DFunction s)
     in Function s' (FunctionDefinition (fmap (replaceDType f) <$> allocations) (M.map (replaceDType f) blocks) bid_init)

instance ReplaceDtype Dtype where
  replaceDType' n f a@(DStruct _) | n == 0 = f a
  replaceDType' n f a@(DStruct (TyStruct {..})) =
    f (DStruct (TyStruct sname (fmap (replaceDType' (n - 1) f) <$> fields) is_sconst size_align_offsets))
  replaceDType' n f (DPointer d x) = f $ DPointer (replaceDType' n f d) x
  replaceDType' n f (DArray d x) = f $ DArray (replaceDType' n f d) x
  replaceDType' n f (DFunction (FunctionSignature ret xs)) = f $ DFunction (FunctionSignature (replaceDType f ret) (replaceDType f <$> xs))
  replaceDType' n f d = d

instance ReplaceDtype Operand where
  replaceDType f (Register rid d) = Register rid $ replaceDType f d
  replaceDType f (Constant (GlobalVariable a d)) = Constant (GlobalVariable a (replaceDType f d))
  replaceDType f x = x

instance ReplaceDtype Instruction where
  replaceDType f Nop = Nop
  replaceDType f b@BinOp {..} = (fmap (replaceDType f) b) {idtype = replaceDType f idtype}
  replaceDType f b@UnaryOp {..} = fmap (replaceDType f) b {idtype = replaceDType f idtype}
  replaceDType f b@Store {..} = fmap (replaceDType f) b
  replaceDType f b@Load {..} = fmap (replaceDType f) b
  replaceDType f b@Call {..} =
    let calee' = replaceDType f callee
        args' = fmap (replaceDType f) args
     in case getDtype calee' of
          (DPointer (DFunction FunctionSignature {..}) _) -> Call calee' args' ret
          _ -> error "cannot happen"
  replaceDType f b@TypeCast {..} = fmap (replaceDType f) b {target_dtype = replaceDType f target_dtype}
  replaceDType f b@GetElementPtr {..} = fmap (replaceDType f) b {idtype = replaceDType f idtype}

instance ReplaceDtype Block where
  replaceDType f a@(Block {..}) = Block (fmap (replaceDType f) <$> phinodes) (replaceDType f <$> instructions) (replaceDType f exit)

instance ReplaceDtype BlockExit where
  replaceDType f = fmap (replaceDType f)
