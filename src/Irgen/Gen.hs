module Irgen.Gen where

import Control.Arrow (second)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Ir.Types
import Irgen.GenUtil
import Irgen.Util
import Language.C
import Simplify.Gen qualified as T
import Simplify.Types qualified as T

addInstruction :: (Compiler m) => Instruction -> m Operand
addInstruction instr = do
  FunContext {..} <- get
  index <- case M.lookup _bid _uncommited of
    Nothing -> modify (over uncommited $ M.insert _bid ([], [instr])) >> return 0
    (Just (phis, instrs)) -> modify (over uncommited (M.insert _bid (phis, instrs ++ [instr]))) >> return (length instrs)
  return (Register (Temp _bid index) (getDtype instr))

store o1 o2 = addInstruction (Store o1 o2) >> return o2

tempTo :: T.Initializer -> Initializer
tempTo (T.InitExpr (T.CastExpr e d)) = tempTo $ T.InitExpr e
tempTo (T.InitExpr (T.UnaryExpr CMinOp (T.ConstExpr (Int a w b)))) = InitConst (Int (-a) w b)
tempTo (T.InitExpr (T.UnaryExpr CMinOp (T.ConstExpr (Float a w)))) = InitConst (Float (-a) w)
tempTo (T.InitExpr (T.ConstExpr (Int a w b))) = InitConst (Int a w b)
tempTo (T.InitExpr (T.ConstExpr (Float a w))) = InitConst (Float a w)
tempTo (T.InitArray d xs) = InitList (tempTo . snd <$> xs)
tempTo (T.InitStruct xs) = InitList (tempTo . trd <$> xs)
  where
    trd (a, b, c) = c
tempTo x = error (show x)

genExtDecl :: (Compiler m) => T.Decl -> m [(String, Declaration)]
genExtDecl (T.Variable (T.VarDecl name dtype init)) = do
  insertSym name (Constant (GlobalVariable name dtype))
  if isFn dtype then return [] else return [(name, Variable dtype (tempTo <$> init))]
genExtDecl f@(T.Function name ret params body) = do
  insertSym name (Constant (GlobalVariable name (DFunction (FunctionSignature ret (item <$> params)))))
  newFunctionScope $ do
    operands <- fmap toPointer <$> traverse add params
    modify (over uncommited $ M.insert (BlockId 0) (params, []))
    traverse_ (\(i, d, s) -> store s (Register (Arg (BlockId 0) i) d)) $ zip3 [0 ..] (item <$> params) operands
    body' <- traverse_ genStat body >> commitFunction ret
    return [(name, Function (FunctionSignature ret (item <$> params)) body')]

logicOp op expr1 expr2 = do
  [t, f, cont] <- newBlock 3
  t1 <- add (Named Nothing boolTy)
  let tempVar = toPointer t1
  e' <- genExpr expr1
  case op of
    CLndOp -> do
      commitBlock (ConditionalJump e' (JumpArg t []) (JumpArg f []))
      withBlock t $ genBranch tempVar cont expr2
      withBlock f $ genBranch' tempVar cont 0
      withBlock cont $ addInstruction (Load tempVar)
    CLorOp -> do
      commitBlock (ConditionalJump e' (JumpArg f []) (JumpArg t []))
      withBlock t $ genBranch tempVar cont expr2
      withBlock f $ genBranch' tempVar cont 1
      withBlock cont $ addInstruction (Load tempVar)
  where
    genBranch tempVar bidCont i = do
      op <- genExpr i
      store tempVar op
      commitBlock (Jump (JumpArg bidCont []))
    genBranch' tempVar bidCont i = do
      store tempVar (Constant (Int i 1 False))
      commitBlock (Jump (JumpArg bidCont []))

genExpr (T.CommaExpr xs d) = do
  xs' <- traverse genExpr xs
  return (last xs')
genExpr (T.VarExpr name dtype) = lookupSym' name
genExpr (T.AddressOfExpr e) = do
  op' <- genExpr e
  return (toPointer op')
genExpr (T.IndirectionExpr e) = do
  op' <- genExpr e
  addInstruction (Load op')
genExpr (T.ConstExpr c) = return (Constant c)
genExpr e@(T.AssignExpr expr1 expr2) = do
  e1 <- genExpr expr1
  e2 <- genExpr expr2
  addInstruction (Store e1 e2) >> return e2
genExpr e@(T.CallExpr fun args) = do
  e1 <- genExpr fun
  e2s <- traverse genExpr args
  addInstruction (Call e1 e2s (getDtype e))
genExpr e@(T.BinaryExpr op expr1 expr2 d)
  | op `elem` [CLndOp, CLorOp] = logicOp op expr1 expr2
  | otherwise = do
      e1 <- genExpr expr1
      e2 <- genExpr expr2
      addInstruction (BinOp op e1 e2 (getDtype e))
genExpr (T.CastExpr e d) = do
  e' <- genExpr e
  addInstruction (TypeCast e' d)
genExpr (T.GetElementPtr left right d) = do
  left' <- genExpr left
  right' <- genExpr right
  addInstruction (GetElementPtr left' right' d)
genExpr (T.UnaryExpr op expr) = do
  expr' <- genExpr expr
  addInstruction (UnaryOp op expr' (getDtype expr'))
genExpr (T.CondExpr c a b) = do
  [t, f, cont] <- newBlock 3
  t1 <- add (Named Nothing (getDtype a))
  let tempVar = toPointer t1
  e' <- genExpr c
  commitBlock (ConditionalJump e' (JumpArg t []) (JumpArg f []))
  withBlock t $ genBranch tempVar cont a
  withBlock f $ genBranch tempVar cont b
  withBlock cont $ addInstruction (Load tempVar)
  where
    genBranch tempVar bidCont i = newBlockScope $ do
      op <- genExpr i
      store tempVar op
      commitBlock (Jump (JumpArg bidCont []))
genExpr T.EmptyExpr = return (Constant Unit)
genExpr (T.PostOpExpr expr1 expr2) = do
  f <- genExpr expr1
  s <- genExpr expr2
  return f

genInit (T.InitExpr e) operand = do
  e' <- genExpr e
  void $ addInstruction (Store operand e')
genInit (T.InitArray d xs) operand = do
  operand' <- addInstruction (GetElementPtr operand (Constant $ Int 0 64 True) (pointer d))
  forM_ xs $ \(offset, init) -> do
    operand'' <- addInstruction (GetElementPtr operand' (Constant $ Int offset 64 True) (pointer d))
    genInit init operand''
genInit (T.InitStruct xs) operand = forM_ xs $ \(offset, d, init) -> do
  operand' <- addInstruction (GetElementPtr operand (Constant $ Int offset 64 True) (pointer d))
  genInit init operand'

genStat (T.DeclStmt (T.VarDecl name dtype init)) = do
  alloc <- add' name dtype
  case init of
    Nothing -> return ()
    (Just init) -> genInit init (toPointer alloc)
genStat (T.RetStmt expr) = do
  operand <- genExpr expr
  commitBlock (Return operand)
genStat (T.ExprStmt expr) = void (genExpr expr)
genStat (T.IfStmt cond ifStmts elseStmts) = do
  [t, f, cont] <- newBlock 3
  e' <- genExpr cond
  commitBlock (ConditionalJump e' (JumpArg t []) (JumpArg f []))
  withBlock t $ genBranch cont ifStmts
  withBlock f $ genBranch cont elseStmts
  withBlock cont $ return ()
  where
    genBranch bidCont is = newBlockScope $ traverse_ genStat is >> commitBlock (Jump (JumpArg bidCont []))
genStat T.ContStmt = do
  pid <- gets _postid
  commitBlock (Jump (JumpArg pid []))
genStat T.BreakStmt = do
  cid <- gets _contid
  commitBlock (Jump (JumpArg cid []))
genStat (T.ForStmt init cond post body) = do
  [bidInit, bidCond, bidBody, bidPost, bidCont] <- newBlock 5
  commitBlock (Jump (JumpArg bidInit []))
  withBlock bidInit $ do
    traverse_ genStat init >> commitBlock (Jump (JumpArg bidCond []))
    withBlock bidCond $ do
      e' <- genExpr cond
      commitBlock (ConditionalJump e' (JumpArg bidBody []) (JumpArg bidCont []))
    withBlock bidBody $ do
      modify (set postid bidPost)
      modify (set contid bidCont)
      traverse_ genStat body >> commitBlock (Jump (JumpArg bidPost []))
    withBlock bidPost $ do
      e' <- genExpr post
      commitBlock (Jump (JumpArg bidCond []))
  withBlock bidCont $ return ()
genStat (T.SwitchStmt e def cases) = do
  [cont] <- newBlock 1
  modify (set contid cont)
  curbid <- gets _bid
  e' <- genExpr e
  [defId] <- newBlock 1
  withBlock defId $
    case def of
      Nothing -> commitBlock $ Return $ Constant Unit
      (Just (T.Case _ stats)) -> traverse_ genStat stats
  xs <- forM cases $ \(T.Case (Just constant) stats) -> do
    [block] <- newBlock 1
    withBlock block $ traverse genStat stats
    return (constant, block)
  withBlock curbid $ commitBlock (Switch e' (JumpArg defId []) ((\(c, bid) -> (c, JumpArg bid [])) <$> xs))
  withBlock cont $ return ()
genStat _ = undefined