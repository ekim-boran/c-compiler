{-# LANGUAGE MonoLocalBinds #-}

module NaiveAsmGen.Gen where

import Asm.Types
import Control.Monad.State.Strict hiding (State)
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Ir.Types (FunctionSignature (FunctionSignature))
import Ir.Types qualified as I
import Language.C
import NaiveAsmGen.Util
import Numeric (showHex)

asmGen :: I.TranslationUnit -> TranslationUnit
asmGen (I.TranslationUnit decls structs) = flip evalState emptyState $ do
  defs <- concat <$> traverse fgen decls
  vars <- concat <$> traverse vgen decls
  ds <- gets ds
  return $ TranslationUnit defs $ vars ++ [Section [DGlobl name, DSection SData] (Variable name [dir]) | (name, dir) <- ds]
  where
    fgen (name, I.Function sig def) = do
      modify (\s -> s {fName = name, top = 16, locations = M.empty})
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
  params' <- concat <$> zipWithM (\i d -> store (reg i d) (I.Arg bid_init i) d) [0 ..] params -- save function arguments
  zipWithM_ (storeLoc . I.Local) [0 ..] (I.item <$> allocations) -- reserve allocations
  sequence_ [storeLoc (I.Arg bid index) (I.item phi) | (bid, block) <- M.toList blocks, (index, phi) <- zip [0 ..] (I.phinodes block)] -- reserve locations for block phinodes
  init' <- traverse blockGen (M.toList initialBlock)
  rest' <- traverse blockGen (M.toList rest)
  top' <- gets top
  return $ Section (funHeader name) $ Function $ addfooter top' <$> Block (Just name) (preamble top' ++ params') : init' ++ rest'
  where
    (initialBlock, rest) = M.partitionWithKey (\k _ -> k == bid_init) blocks

blockGen :: (MonadState State m) => (I.BlockId, I.Block) -> m Block
blockGen (bid, I.Block phis instrs exit) = do
  name <- getLabel bid
  instrs' <- concat <$> traverse (instrGen bid) (zip [0 ..] instrs)
  exits' <- exitGen exit
  return $ Block (Just name) (instrs' ++ exits')

exitGen :: (MonadState State m) => I.BlockExit -> m [Instruction]
exitGen (I.Jump arg) = do
  (label, instrs) <- argGen arg
  return $ instrs ++ [Pseudo (J label)]
exitGen (I.ConditionalJump a arg1 arg2) = do
  (label1, instrs1) <- argGen arg1
  (label2, instrs2) <- argGen arg2
  (l3, reg) <- load 0 a
  return $ instrs1 ++ instrs2 ++ l3 ++ [BType Beq reg Zero label2, Pseudo (J label1)]
exitGen (I.Switch op arg args) = undefined
exitGen (I.Return op) = load 0 op >>= \(i, _) -> return $ i ++ [Pseudo Ret]
exitGen I.Unreachable = return []

argGen (I.JumpArg bid arg) = do
  name <- getLabel bid
  xs <- concat <$> traverse f (zip [0 ..] arg)
  return (name, xs)
  where
    f (index, arg) = do
      (l1, reg) <- load 0 arg
      l2 <- store reg (I.Arg bid index) (I.getDtype arg)
      return (l1 ++ l2)

instrGen :: (MonadState State m) => I.BlockId -> (Int, I.Instruction) -> m [Instruction]
instrGen bid (index, instr) = go instr
  where
    temp = I.Temp bid index
    go I.Nop = return []
    go i@(I.BinOp op lhs rhs dtype) = do
      let reg0 = reg 0 dtype
      (l1, reg1) <- load 1 lhs
      (l2, reg2) <- load 2 rhs
      let l3 = createBinOp op reg1 reg2 (I.getDtype lhs) reg0
      l4 <- store reg0 temp dtype
      return $ l1 ++ l2 ++ l3 ++ l4
    go (I.UnaryOp op operand dtype) = do
      let reg0 = reg 0 dtype
      (l1, reg1) <- load 1 operand
      let l3 = createUnaryOp op reg1 (I.getDtype operand) reg0
      l4 <- store reg0 temp dtype
      return $ l1 ++ l3 ++ l4
    go (I.Store ptr value) = do
      (l0, reg1) <- load 0 ptr
      (l1, reg2) <- load 1 value
      let (size, _) = size_signed (I.getDtype value)
      return $ l0 ++ l1 ++ [SType (Store size) reg1 reg2 (Value 0)]
    go x@(I.Load ptr) = do
      let dtype = I.getInner $ I.getDtype ptr
      (l1, reg1) <- load 1 ptr
      let reg0 = reg 0 dtype
      let (size, signed) = size_signed dtype
      l2 <- store reg0 temp (I.getDtype x)
      return $ l1 ++ [IType (Load size signed) reg1 reg0 (Value 0)] ++ l2
    go (I.Call (I.Constant (I.GlobalVariable name d)) args return_type) = do
      let reg0 = reg 0 return_type
      xs <- traverse (uncurry load) $ zip [0 ..] args
      l2 <- store reg0 temp return_type
      return $ concat (fst <$> xs) ++ [Pseudo (Call name)] ++ l2
    go (I.Call r args return_type) = do
      let reg0 = reg 0 return_type
      xs <- traverse (uncurry load) $ zip [0 ..] args
      l2 <- store reg0 temp return_type
      (l3, reg1) <- load 7 r
      return $ concat (fst <$> xs) ++ l3 ++ [Pseudo (Jalr reg1)] ++ l2
    go (I.GetElementPtr ptr offset dtype) = do
      let reg0 = reg 0 dtype
      (l1, reg1) <- load 1 ptr
      (l2, reg2) <- load 2 offset
      let l3 = createBinOp CAddOp reg1 reg2 (I.getDtype ptr) reg0
      l4 <- store reg0 temp dtype
      return $ l1 ++ l2 ++ l3 ++ l4
    go i@(I.TypeCast value dtype) = do
      let reg0 = reg 3 (I.getDtype i)
      (l1, source) <- load 0 value
      let inst = typecast reg0 source dtype (I.getDtype value)
      l2 <- store reg0 temp dtype
      return $ l1 ++ inst ++ l2

load :: MonadState State m => Int -> I.Operand -> m ([Instruction], Register)
load number (I.Constant (I.Int value width signed)) = return $ (\r -> (li r value, r)) $ (intReg number)
load number (I.Constant (I.Float value width)) = do
  name <- addDecl (genGlobalDirective width (getFloatRepr width value))
  let reg = (floatReg number)
  let temp = Saved RInteger 4
  let instrs = la temp name ++ loadValue temp reg 0 (I.DFloat width False)
  return (instrs, reg)
load number (I.Constant (I.String s)) = do
  name <- addDecl (DAscii s)
  let reg = (intReg number)
  let instrs = la reg name
  return (instrs, reg)
load number (I.Constant (I.Unit)) = return $ (\r -> (li r 0, r)) $ (intReg number)
load number (I.Constant (I.Undef _)) = return $ (\r -> (li r 0, r)) $ (intReg number)
load number (I.Constant (I.GlobalVariable label d)) = return $ (\r -> (la r label, r)) $ (intReg number)
load number i@(I.Register rid dtype) = do
  (addr, dtype') <- getLoc rid dtype
  let register = anyReg number dtype
  let instr
        | dtype == dtype' = loadValue s0 register addr dtype
        | I.getInner dtype == dtype' = loadAddr s0 register addr
        | otherwise = error (show dtype ++ show dtype')
  return (instr, register)

store sourceReg rid dtype = do
  (addr, dtype) <- storeLoc rid dtype
  return [SType (Store datasize) s0 sourceReg (Value (-addr))]
  where
    (datasize, signed) = size_signed dtype
