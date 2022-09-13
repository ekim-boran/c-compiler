{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Simplify.Gen where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Foldable (traverse_)
import Data.Functor
import Data.Map.Lazy qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as S
import Ir.Types
  ( Constant (Float, Int, Undef, Unit),
    Dtype (..),
    HasDtype (..),
    Named (Named, item),
    TyStruct (..),
    boolTy,
    floatTy,
    getField,
    getInner,
    intTy,
    unit,
    unsignedLongTy,
    width,
  )
import Irgen.Util hiding (get)
import Language.C
import Language.C.Data.Ident
import Simplify.Types
import Simplify.Util
import Util

checkTu (CTranslUnit defs _) = mkTranslUnit <$> runStateT (traverse checkExt defs) emptyEnv
  where
    mkTranslUnit (declss, env) = TranslationUnit (concat declss) (_structDecls env)
    emptyEnv = Env M.empty M.empty M.empty "" 0

checkExt :: (Checker m, Show a) => CExternalDeclaration a -> m [Decl]
checkExt (CDeclExt decl) = fmap Variable <$> checkDecl decl
checkExt (CFDefExt (CFunDef retSpecs declarator decls stmt a)) = do
  (fName, ret, params) <- addFunction retSpecs declarator
  newFunctionScope fName $ do
    traverse_ (\(Named (Just name) ty) -> insertType name ty) params
    stmt' <- statement stmt
    return [Function fName ret params stmt']
checkExt (CAsmExt csl a) = error "not implemented"

checkInit d (CInitExpr expr a) = do
  expr' <- expression expr
  return $ InitExpr (typecast expr' d)
checkInit d'@(DStruct (TyStruct {fields = xs, size_align_offsets = (_, _, offsets), ..})) (CInitList as a) = do
  InitStruct <$> traverse (\(d, offset, list) -> (offset,d,) <$> checkInit d list) (zip3 (fmap item xs) offsets (snd <$> as))
checkInit d'@(DArray d _) (CInitList as a) = do
  let size = width d
  InitArray d <$> traverse (\(i, list) -> (size * i,) <$> checkInit d list) (zip [0 ..] (snd <$> as))
checkInit d (CInitList x1 a) = error "not implemented"

checkDecl (CDecl specs xs a) = do
  typesAndInits <- parseDeclType specs xs
  forM typesAndInits $ \(nd@(Named (Just name) d), initializer) -> do
    init <- traverse (checkInit d) initializer
    insertType name d
    return (VarDecl name d init)
checkDecl (CStaticAssert ce csl a) = error "not implemented static assert"

checkComp :: (Checker m, Show a) => CCompoundBlockItem a -> m [Statement]
checkComp (CBlockStmt stat) = statement stat
checkComp (CBlockDecl decl) = fmap DeclStmt <$> checkDecl decl
checkComp (CNestedFunDef cfd) = error "not implemented nested fun def"

size d = ConstExpr (Int (width d) 64 False)

addressOf = concatIndirection . AddressOfExpr
  where
    concatIndirection (AddressOfExpr (IndirectionExpr x)) = concatIndirection x
    concatIndirection (IndirectionExpr x) = IndirectionExpr $ concatIndirection x
    concatIndirection x = x

expression :: (Checker m, Show a) => CExpression a -> m Expr
expression (CVar i@(Ident name _ _) a) = do
  d <- getType name
  case (d) of
    d@(DFunction _) -> return $ addressOf $ VarExpr name d
    _ -> return $ IndirectionExpr $ addressOf $ VarExpr name d
expression (CConst cons) = return $ ConstExpr $ genConstant' cons
expression (CComma xs a) = do
  xs' <- traverse expression xs
  pure $ CommaExpr xs' (maybe unit getDtype $ lastMay xs')
expression (CAssign op left right a) = do
  right' <- case op of
    CAssignOp -> expression right
    op' -> expression (CBinary (assignOp op') left right a)
  left' <- expression left
  return $ AssignExpr (addressOf left') (typecast right' (getDtype left'))
expression (CCond c (Just ok) false a) = do
  c' <- expression c
  ok' <- expression ok
  false' <- expression false
  case numericCastRules ok' false' of
    Nothing -> throwError "type error"
    (Just sharedType) -> return $ CondExpr (typecast c' boolTy) (typecast ok' sharedType) (typecast false' sharedType)
expression (CBinary op e1 e2 a) = checkBinary op e1 e2 a
expression (CCast decl expr a) = do
  expr' <- expression expr
  d <- parseSingleDecl decl
  return $ typecast expr' (d)
expression (CIndex left right a) = do
  left' <- expression left
  right' <- expression right
  case pointerArithmeticCast CAddOp left' right' of
    Just x -> return $ IndirectionExpr x
    Nothing -> throwError "type error"
expression (CUnary op expr a) = do
  case op of
    CAdrOp -> addressOf <$> expression expr
    CIndOp -> IndirectionExpr <$> expression expr
    CPreIncOp -> expression $ CAssign CAddAssOp expr (CConst $ CIntConst (cInteger 1) a) a
    CPreDecOp -> expression $ CAssign CSubAssOp expr (CConst $ CIntConst (cInteger 1) a) a
    CPostIncOp -> PostOpExpr <$> expression expr <*> expression (CAssign CAddAssOp expr (CConst $ CIntConst (cInteger 1) a) a)
    CPostDecOp -> PostOpExpr <$> expression expr <*> expression (CAssign CSubAssOp expr (CConst $ CIntConst (cInteger 1) a) a)
    _ -> UnaryExpr op <$> expression expr
expression (CCall name args a) = do
  name' <- expression name
  let (ret, argsTy) = func $ getDtype name'
  args' <- traverse expression args
  let args'' = zipWith (\a b -> uncurry typecast $ checkStruct a b) args' argsTy
  return $ CallExpr name' args''
  where
    checkStruct arg x =
      case (getDtype arg, x) of
        (DStruct {}, DPointer (DStruct {}) _) -> (addressOf arg, x)
        (a, b) -> (arg, b)
expression (CMember left ident isPointer a) = do
  left' <- expression left
  let name = identName ident
      xs = getField (getDtype left') name
      l = if isPointer then left' else addressOf left'
      x = foldl (\l (d, offset) -> GetElementPtr l (ConstExpr (Int offset 64 True)) (pointer d)) l xs
  return $ IndirectionExpr x
expression (CAlignofExpr expr a) = fmap (size . getDtype) (expression expr)
expression (CAlignofType decl a) = size <$> parseSingleDecl decl
expression (CSizeofExpr expr a) = fmap (size . getDtype) (expression expr)
expression (CSizeofType decl a) = size <$> parseSingleDecl decl
expression x = error $ "not implemented" ++ show x

r ret = case ret of
  DUnit _ -> Unit
  _ -> Undef ret

statement :: (Checker m, Show a) => CStatement a -> m [Statement]
statement (CCompound _ items _) = concat <$> traverse checkComp items
statement (CReturn mexpr a) = do
  rtype <- retTy
  expr' <- case mexpr of
    (Just e) -> expression e
    Nothing -> return $ ConstExpr (r rtype)
  return [RetStmt (typecast expr' rtype)]
statement (CFor init cond post body a) = do
  init' <- case init of
    Left e -> case e of
      Nothing -> return []
      (Just e) -> pure . ExprStmt <$> expression e
    Right r -> fmap DeclStmt <$> checkDecl r
  cond' <- case cond of
    Nothing -> return $ ConstExpr (Int 1 1 False)
    (Just e) -> expression e
  post' <- case post of
    Nothing -> return EmptyExpr
    (Just e) -> expression e
  body' <- statement body
  return [ForStmt init' (typecast cond' boolTy) post' body']
statement (CExpr mexpr a) =
  case mexpr of
    Nothing -> return []
    (Just expr) -> pure . ExprStmt <$> expression expr
statement (CIf cond i melse a) = do
  cond' <- expression cond
  i' <- statement i
  e' <- case melse of
    Nothing -> return []
    (Just elseExpr) -> statement elseExpr
  return [IfStmt (typecast cond' boolTy) i' e']
statement (CWhile cond body b a) = do
  cond' <- expression cond
  body' <- statement body
  return [ForStmt [] (typecast cond' boolTy) EmptyExpr body']
statement (CCont a) = return [ContStmt]
statement (CBreak a) = return [BreakStmt]
statement (CCase expr stat a) = do
  e <- expression expr
  e' <- case e of
    ConstExpr e' -> return e'
    _ -> throwError "must be constant"
  pure . CaseStmt . Case (Just e') <$> statement stat
statement (CDefault stat a) = pure . CaseStmt . Case Nothing <$> statement stat
statement (CSwitch expr stats a) = do
  cases <- statement stats
  e <- expression expr
  let def = headMay $ concatMap (\case (CaseStmt c@(Case Nothing _)) -> [c]; _ -> []) cases
      cases' = concatMap (\case (CaseStmt c@(Case (Just x) _)) -> [c]; _ -> []) cases
  return [SwitchStmt e def cases']
statement _ = error "not implemented"

numericCastRules op1 op2 = case (getDtype op1, getDtype op2) of
  (a@(DFloat w c), b@(DFloat w' c')) -> Just $ DFloat (max w w') False
  (a@(DFloat _ _), DInt {}) -> Just a
  (DInt {}, b@(DFloat _ _)) -> Just b
  (a@(DInt w s c), b@(DInt w' s' c')) -> Just st
    where
      st
        | a == b = a
        | w < 32 || w' < 32 = DInt 32 True False
        | w > w' = DInt w s False
        | w' > w = DInt w' s' False
        | otherwise = DInt (max w w') (s && s') False
  (_, _) -> Nothing

rT shared op | op `elem` [CLeOp, CLeqOp, CGrOp, CGeqOp, CEqOp, CNeqOp] = boolTy
rT shared _ = shared

floatCast op op1 op2 | op `elem` [CAddOp, CSubOp, CMulOp, CDivOp, CLeOp, CLeqOp, CGrOp, CGeqOp, CEqOp, CNeqOp] = do
  sharedType <- case (getDtype op1, getDtype op2) of
    (a@(DFloat w c), b@(DFloat w' c')) -> Just $ DFloat (max w w') False
    (a@(DFloat _ _), DInt {}) -> Just a
    (DInt {}, b@(DFloat _ _)) -> Just b
    (_, _) -> Nothing
  Just (BinaryExpr op (typecast op1 sharedType) (typecast op2 sharedType) (rT sharedType op))
floatCast op _ _ = Nothing

integerCast op op1 op2 | op `notElem` [CLndOp, CLorOp] = case (getDtype op1, getDtype op2) of
  (a@(DInt w s c), b@(DInt w' s' c')) -> Just (BinaryExpr op (typecast op1 sharedType) (typecast op2 sharedType) (rT sharedType op))
    where
      sharedType
        | a == b = a
        | w < 32 || w' < 32 = DInt 32 True False
        | w > w' = DInt w s False
        | w' > w = DInt w' s' False
        | otherwise = DInt (max w w') (s && s') False
  (_, _) -> Nothing
integerCast op op1 op2 = Nothing

-- fix it in order to avoid looping in recursive structs
myeq (DPointer x _) (DPointer y _) = x `myeq` y
myeq x y = x == y

typecast operand dtype | dtype `myeq` getDtype operand = operand
typecast (ConstExpr (Int v w s)) (DInt a b c) = ConstExpr $ Int v a b
typecast (ConstExpr (Float v w)) (DFloat a b) = ConstExpr $ Float v a
typecast operand dtype = case (getDtype operand, dtype) of
  (DInt w s _, DInt 1 False _) -> BinaryExpr CNeqOp operand (ConstExpr $ Int 0 w s) dtype
  (DArray inner2 _, DPointer inner1 a) -> GetElementPtr (addressOf operand) (ConstExpr $ Int 0 32 True) dtype
  (_, _) -> CastExpr operand dtype

arithHelper op2 d neg = fromMaybe undefined $ integerCast CMulOp op2 (ConstExpr (Int (if neg then -1 * width d else width d) 64 True))

pointerArithmeticCast CSubOp op1 op2 = case (getDtype op1, getDtype op2) of
  (a@DInt {}, b@DPointer {}) -> Nothing
  (a@DInt {}, b@DArray {}) -> Nothing
  (a@(DPointer i _), b@(DInt w' s' c')) -> Just (GetElementPtr op1 (arithHelper op2 i True) a)
  (a@(DArray i _), b@(DInt w' s' c')) -> Just (GetElementPtr (typecast op1 (arrayToPointer a)) (arithHelper op2 i True) (arrayToPointer a))
  (_, _) -> Nothing
pointerArithmeticCast CAddOp op1 op2 = case (getDtype op1, getDtype op2) of
  (a@DInt {}, b@DPointer {}) -> pointerArithmeticCast CAddOp op2 op1
  (a@DInt {}, b@DArray {}) -> pointerArithmeticCast CAddOp op2 op1
  (a@(DPointer i _), b@(DInt w' s' c')) -> pure (GetElementPtr op1 (arithHelper op2 i False) a)
  (a@(DArray i _), b@(DInt w' s' c')) -> pure (GetElementPtr (typecast op1 (arrayToPointer a)) (arithHelper op2 i False) (arrayToPointer a))
  (_, _) -> Nothing
pointerArithmeticCast _ _ _ = Nothing

pointerComparisonCast op e1 e2
  | op `elem` [CLeOp, CLeqOp, CGrOp, CGeqOp, CEqOp, CNeqOp] =
      Just (BinaryExpr op (typecast e1 unsignedLongTy) (typecast e2 unsignedLongTy) boolTy)
  | otherwise = Nothing

logicCast op op1 op2
  | op `elem` [CLndOp, CLorOp] = Just (BinaryExpr op (typecast op1 boolTy) (typecast op2 boolTy) boolTy)
  | otherwise = Nothing

checkBinary op e1 e2 a = do
  e1' <- expression e1
  e2' <- expression e2
  let p1 = pointerArithmeticCast op e1' e2'
  let p2 = integerCast op e1' e2'
  let p3 = floatCast op e1' e2'
  let p4 = logicCast op e1' e2'
  let p5 = pointerComparisonCast op e1' e2'
  case p1 <|> p2 <|> p3 <|> p4 <|> p5 of
    Just e -> return e
    Nothing -> throwError "type checking Error"