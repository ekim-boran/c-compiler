module AsmGen.Gen where

import Asm.Types
import AsmGen.Allocator
import AsmGen.Ir (preProcess)
import AsmGen.Util
import Control.Arrow
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (maybeToList)
import Debug.Trace
import Ir.Types (FunctionSignature (FunctionSignature))
import Ir.Types qualified as I
import Ir.Utils (foldInstructions)
import Language.C
import Numeric (showHex)

p :: I.TranslationUnit -> I.TranslationUnit
p (I.TranslationUnit decls xs) = I.TranslationUnit ((fmap (second go)) decls) xs
  where
    go (I.Function s (I.FunctionDefinition allocs blocks bid)) = I.Function s (I.FunctionDefinition allocs (preProcess blocks) bid)
    go x = x

asmGen i = asmGen' (p i)

asmGen' :: I.TranslationUnit -> TranslationUnit
asmGen' (I.TranslationUnit decls structs) = flip evalState emptyState $ do
  defs <- concat <$> traverse fgen decls
  vars <- concat <$> traverse vgen decls
  ds <- gets ds
  return $ TranslationUnit defs $ vars ++ [Section [DGlobl name, DSection SData] (Variable name [dir]) | (name, dir) <- ds]
  where
    fgen (name, I.Function sig def) = do
      let (a, b) = mkAllocator def
      modify (\s -> s {fName = name, top = 16, locations = M.empty, registers = a, livenessMap = b})
      s <- funGen name sig def
      return [s]
    fgen (name, I.Variable ty init) = return []
    vgen (name, I.Variable ty init) = return [(varGen name ty init)]
    vgen _ = return []

varGen :: Label -> I.Dtype -> Maybe (I.Initializer) -> Section Variable
varGen name ty init = Section [DGlobl name, DSection SData] (Variable name (initGen ty init))
  where
    initGen ty (Just init) = go ty init
    initGen ty Nothing = [DZero (fromIntegral $ I.width ty)]
    go (I.DArray inner size) (I.InitList xs) =
      [inside | i <- xs, inside <- go inner i] ++ [DZero (fromIntegral $ I.width inner * (size - length xs)) | size - length xs /= 0]
    go (I.DFloat w _) (I.InitConst (I.Float value _)) = [genGlobalDirective w (getFloatRepr w value)]
    go (I.DInt w _ _) (I.InitConst (I.Int value _ _)) = [genGlobalDirective w (fromIntegral value)]
    go (I.DStruct (I.TyStruct name _ _ xs)) _ = undefined
    go a b = error (show a ++ show b) undefined

-- allocate spaces for function arguments - allocations - phinodes
funGen :: (MonadState State m) => String -> I.FunctionSignature -> I.FunctionDefinition -> m (Section Function)
funGen name (FunctionSignature {..}) funDef@(I.FunctionDefinition {..}) = do
  zipWithM_ (storeLoc . I.Local) [0 ..] (I.item <$> allocations) -- reserve allocations
  let funLocs = foldInstructions f [] blocks
  xs <- traverse (uncurry getUsed) funLocs
  let maxLoc = maximum $ (0 : (fmap length xs))
  modify (\s -> s {top = top s + maxLoc * 8})

  init' <- traverse blockGen (M.toList initialBlock)
  rest' <- traverse blockGen (M.toList rest)
  top' <- gets top
  return $ Section (funHeader name) $ Function $ addfooter top' <$> Block (Just name) (preamble top') : init' ++ rest'
  where
    (initialBlock, rest) = M.partitionWithKey (\k _ -> k == bid_init) blocks
    f bid index (I.Call {}) = ((bid, index) :)
    f bid index _ = id

blockGen :: (MonadState State m) => (I.BlockId, I.Block) -> m Block
blockGen (bid, I.Block phis instrs exit) = do
  name <- getLabel bid
  phiInstrs <- movePhis bid phis
  instrs' <- concat <$> traverse (instrGen bid) (zip [0 ..] instrs)
  exits' <- test <$> (execute $ exitGen bid (length instrs) exit)
  return $ Block (Just name) (phiInstrs ++ instrs' ++ exits')

movePhis bid phis = concat <$> traverse f (zip [0 ..] phis)
  where
    f (index, arg) = execute $ do
      source <- getRegisterTemp bid index
      target <- getRegister (I.Arg bid index)
      return $ move target source (I.item arg)

test (Pseudo (Li regx value) : Pseudo (Mv target regy) : xs) | regx == regy = Pseudo (Li target value) : test xs
test (Pseudo (Li regx value) : (RType (Add datasize') out reg1 (Just regy)) : xs)
  | regx == regy = [IType (Addi Nothing) out reg1 (Value (value))] ++ test xs
test (x : xs) = x : test xs
test x = x

exitGen :: I.BlockId -> Int -> (MonadState State m) => I.BlockExit -> MaybeT m [Instruction]
exitGen bid index (I.Jump arg) = do
  (label, instrs) <- argGen bid index arg
  return $ instrs ++ [Pseudo (J label)]
exitGen bid index (I.ConditionalJump a arg1 arg2) = do
  (label1, instrs1) <- argGen bid index arg1
  (label2, instrs2) <- argGen bid index arg2
  (l3, reg) <- load bid index a
  return $ instrs2 ++ l3 ++ [BType Beq reg Zero label2] ++ instrs1 ++ [Pseudo (J label1)]
exitGen bid index (I.Switch op def args) = do
  (l1, r) <- load bid index op
  is <- forM args $ \((I.Int v _ _), arg) -> do
    let a = li (Arg RInteger 6) v
    (l, i) <- argGen bid index arg
    return $ i ++ a ++ [BType Beq (Arg RInteger 6) r l]
  (l, i) <- argGen bid index def
  return $ l1 ++ (concat is) ++ i ++ [Pseudo (J l)]
exitGen bid index (I.Return op) = do
  (l1, r) <- load bid index op
  let l2 = move (reg 0 (I.getDtype op)) r (I.getDtype op)
  return $ l1 ++ l2 ++ [Pseudo Ret]
exitGen bid index I.Unreachable = return []

argGen bid1 index1 (I.JumpArg bid arg) = do
  name <- getLabel bid
  xs <- concat <$> traverse f (zip [0 ..] arg)
  return (name, xs)
  where
    f (index, arg) = execute $ do
      (l1, reg) <- load bid1 index1 arg
      target <- getRegisterTemp bid index
      let l2 = move target reg (I.getDtype arg)
      return $ l1 ++ l2

saveRestore bid index regs = do
  ret <- maybeToList <$> (runMaybeT $ getRegister (I.Temp bid index))
  regs' <- filter ((`notElem` (ret ++ regs)) . fst) <$> getUsed bid index
  top' <- gets top
  let save = (\(i, (r, d)) -> storeInstr r (top' - 8 * i) d) <$> (zip [0 ..] regs')
  let restore = (\(i, (r, d)) -> loadInstr r (top' - 8 * i) d) <$> (zip [0 ..] regs')
  return (save, restore)

instrGen :: (MonadState State m) => I.BlockId -> (Int, I.Instruction) -> m [Instruction]
instrGen bid (index, instr) = test <$> execute (go instr)
  where
    getReg = getRegister $ I.Temp bid index
    loadReg = load bid index
    go I.Nop = return []
    go i@(I.BinOp op lhs rhs dtype) = do
      reg0 <- getReg
      (l1, reg1) <- loadReg lhs
      (l2, reg2) <- loadReg rhs
      let l3 = createBinOp op reg1 reg2 (I.getDtype lhs) reg0
      return $ l1 ++ l2 ++ l3
    go (I.UnaryOp op operand dtype) = do
      reg0 <- getReg
      (l1, reg1) <- loadReg operand
      let l3 = createUnaryOp op reg1 (I.getDtype operand) reg0
      return $ l1 ++ l3
    go (I.Store ptr value) = do
      (l0, reg1) <- loadReg ptr
      (l1, reg2) <- loadReg value
      let (size, _) = size_signed (I.getDtype value)
      return $ l0 ++ l1 ++ [SType (Store size) reg1 reg2 (Value 0)]
    go x@(I.Load ptr) = do
      let dtype = I.getInner $ I.getDtype ptr
      (l1, reg1) <- loadReg ptr
      reg0 <- getReg
      let (size, signed) = size_signed dtype
      return $ l1 ++ [IType (Load size signed) reg1 reg0 (Value 0)]
    go (I.Call (I.Constant (I.GlobalVariable name d)) args return_type) = do
      reg0 <- getReg
      xs <- traverse (loadReg) $ args
      (l1, l2) <- saveRestore bid index (snd <$> xs)
      return $ l1 ++ concat (fst <$> xs) ++ [Pseudo (Call name)] ++ l2
    go (I.Call r args return_type) = do
      xs <- traverse (loadReg) $ args
      (l3, reg1) <- loadReg r
      (l1, l2) <- saveRestore bid index (reg1 : (snd <$> xs))
      return $ l1 ++ concat (fst <$> xs) ++ l3 ++ [Pseudo (Jalr reg1)] ++ l2
    go (I.GetElementPtr ptr offset dtype) = do
      reg0 <- getReg
      (l1, reg1) <- loadReg ptr
      (l2, reg2) <- loadReg offset
      let l3 = createBinOp CAddOp reg1 reg2 (I.getDtype ptr) reg0
      return $ l1 ++ l2 ++ l3
    go i@(I.TypeCast value dtype) = do
      reg0 <- getReg
      (l1, source) <- loadReg value
      let inst = typecast reg0 source dtype (I.getDtype value)
      return $ l1 ++ inst

load :: MonadState State m => I.BlockId -> Int -> I.Operand -> MaybeT m ([Instruction], Register)
load bid index (I.Constant (I.Int value width signed)) = return $ (li (Arg RInteger 6) value, (Arg RInteger 6))
load bid index (I.Constant (I.Float value width)) = do
  name <- addDecl (genGlobalDirective width (getFloatRepr width value))
  let reg = (Arg RFloatingPoint 7)
  let temp = (Arg RInteger 7)
  let instrs = la temp name ++ loadValue temp reg 0 (I.DFloat width False)
  return (instrs, reg)
load bid index (I.Constant (I.String s)) = do
  name <- addDecl (DAscii s)
  let instrs = la (Arg RInteger 7) name
  return (instrs, (Arg RInteger 7))
load bid index (I.Constant (I.Unit)) = return $ (li (Arg RInteger 6) 0, (Arg RInteger 6))
load bid index (I.Constant (I.Undef _)) = return $ (li (Arg RInteger 6) 0, (Arg RInteger 6))
load bid index (I.Constant (I.GlobalVariable label d)) = return $ (la (Arg RInteger 7) label, (Arg RInteger 7))
load bid index i@(I.Register rid@(I.Local {}) dtype) = do
  (addr, dtype') <- getLoc rid dtype
  let x
        | dtype == dtype' = (loadValue s0 (reg 7 dtype) addr dtype, (reg 7 dtype))
        | I.getInner dtype == dtype' = (loadAddr s0 ((Arg RInteger 7)) addr, (Arg RInteger 7))
  return x
load bid index i@(I.Register rid dtype) = ([],) <$> getRegister rid

store sourceReg rid dtype = do
  (addr, dtype) <- storeLoc rid dtype
  return [SType (Store datasize) s0 sourceReg (Value (-addr))]
  where
    (datasize, signed) = size_signed dtype
