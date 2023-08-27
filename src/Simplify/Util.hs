{-# LANGUAGE ConstraintKinds #-}

module Simplify.Util where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens
import Control.Monad qualified
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Foldable (traverse_)
import Data.Functor
import Data.Map.Lazy qualified as M
import Data.Maybe (isJust)
import Data.Set qualified as S
import Ir.Types
import Irgen.Util hiding (get)
import Language.C
import Language.C.Data.Ident

data Env = Env
  { _env :: M.Map String Dtype,
    _typedefs :: M.Map String Dtype,
    _structDecls :: M.Map String Dtype,
    _funName :: String,
    _counter :: Int
  }

$(makeLenses ''Env)

type Checker m = (MonadState Env m, MonadError String m, MonadFail m, MonadFix m)

genStructType :: (Checker m) => m String
genStructType = do
  c <- use counter
  counter .= c + 1
  return $ "%t" ++ show c

newFunctionScope name action = do
  s <- get
  modify (set funName name)
  a <- action
  put s
  return a

retTy = do
  x <- gets _funName
  (DFunction (FunctionSignature ret _)) <- lookupDict x _env
  return ret

lookupDict key map = gets (M.lookup key . map) >>= go
  where
    go = maybe (throwError $ "type checker error, lookup failed" ++ show key) return

insertDict key value map = modify (over map (M.insert key value))

getType :: (Checker m) => String -> m Dtype
getType name = lookupDict name _env

insertType :: (Checker m) => String -> Dtype -> m ()
insertType name ty = insertDict name ty env

insertTypedef :: (Checker m) => String -> Dtype -> m ()
insertTypedef name dtype = insertDict name dtype typedefs

processParameters name = traverse (go)
  where
    go (Named n ((DArray d _))) = return $ Named n $ DPointer d False
    go x = return x

addFunction retSpecs decl = do
  retd <- mkDtype retSpecs
  (Named (Just name) (DFunction (FunctionSignature ret args))) <- fromDeclarator retd decl
  ps <- funparams decl
  let params = [Named n d | i@(Named n d) <- ps, isJust n]
  params' <- processParameters name params
  insertType name (DFunction (FunctionSignature ret (item <$> params')))
  return (name, ret, params')
  where
    funparams a@(CDeclr _ xs _ _ _) = concat <$> traverse go xs
      where
        go ((CFunDeclr (Right (decls, bool)) a b)) = fmap fst . concat <$> traverse fromDeclaration decls
        go x = pure []

parseDeclType specs xs =
  concat <$> do
    let baseTy = mkBaseType specs
    dt <- toDtype baseTy
    let isTyDefDecl = is_typedef baseTy
    case xs of
      [] -> return []
      _ ->
        forM xs $ \(Just mdeclerator, init, _) -> do
          ty@(Named (Just n) t) <- fromDeclarator dt mdeclerator

          if isTyDefDecl
            then insertTypedef n t >> return []
            else return [(Named (Just n) t, init)]

parseSingleDecl declaration = do
  d <- fromDeclaration declaration
  case d of
    [] -> error "unable"
    ((Named _ d, _) : xs) -> return d

fromDeclarator :: (Checker m, Show a) => Dtype -> CDeclarator a -> m (Named Dtype)
fromDeclarator d a@(CDeclr i xs _ _ _) = Named (identName <$> i) <$> foldM go d (reverse xs)
  where
    go d x@((CPtrDeclr qualifier _)) = pure $ DPointer d (is_const $ mkBaseType qualifier)
    go d ((CArrDeclr qualifier (CNoArrSize _) _)) = pure $ DPointer d (is_const $ mkBaseType qualifier)
    go d ((CArrDeclr qualifier (CArrSize _ (CConst (CIntConst ((CInteger i c3 fc)) _))) _)) = pure $ DArray d (fromInteger i)
    go d ((CFunDeclr (Right (decls, bool)) a b)) = do
      decls' <- traverse fromDeclaration decls
      let xs = item . fst <$> concat decls'
      pure $ DFunction (FunctionSignature d xs)
    go d (CArrDeclr {}) = error ""
    go d (CFunDeclr {}) = error ""

fromDeclaration :: (Checker m, Show a) => CDeclaration a -> m [(Named Dtype, Maybe (CInitializer a))]
fromDeclaration (CDecl xs [] a) = do
  xs' <- mkDtype xs
  return [(Named Nothing xs', Nothing)]
fromDeclaration ((CDecl xs inits a)) = do
  d <- mkDtype xs
  traverse (go d) inits
  where
    go d (Nothing, b, _) = pure (Named Nothing d, b)
    go d (Just a, b, _) = (,b) <$> fromDeclarator d a
fromDeclaration (CStaticAssert {}) = error "invalid"

toStruct (CStruct _ name fields attr _) = do
  name <- maybe genStructType (pure . identName) name
  case fields of
    Nothing -> lookupDict name _structDecls
    (Just fields) -> do
      mfix $ \s -> do
        insertDict name s structDecls
        xs <- fmap fst . concat <$> traverse fromDeclaration fields
        pure $ DStruct (TyStruct name xs False (calculateOffsets1 xs))

toDtype :: (Checker m, Show a) => BaseDtype a -> m Dtype
toDtype BaseDtype {scalar = Nothing, typedef_name = (Just name), ..} = mkConst is_const <$> lookupDict name _typedefs
toDtype BaseDtype {scalar = Nothing, struct_type = (Just ty), ..} = mkConst is_const <$> toStruct ty
toDtype BaseDtype {scalar = (Just SVoid), ..} = return $ DUnit is_const
toDtype BaseDtype {scalar = (Just (SInt w)), ..} = return $ DInt w (not is_unsigned) is_const
toDtype BaseDtype {scalar = (Just (SFloat w)), ..} = return $ DFloat w is_const
toDtype _ = undefined

mkDtype :: (Checker m, Show a) => [CDeclarationSpecifier a] -> m Dtype
mkDtype xs = do
  let baseTy = mkBaseType xs
  toDtype baseTy

class Get a where
  make :: a b -> BaseDtype b -> BaseDtype b

instance Get CDeclarationSpecifier where
  make (CStorageSpec (CTypedef a)) d = d {is_typedef = True}
  make (CTypeSpec a) d = make a d
  make (CTypeQual a) d = make a d
  make a d = d

instance Get CTypeQualifier where
  make (CConstQual n) d = d {is_const = True}
  make (CVolatQual n) d = d
  make (CRestrQual n) d = d
  make (CAtomicQual n) d = d
  make (CAttrQual cn) d = d
  make (CNullableQual n) d = d
  make (CNonnullQual n) d = d
  make (CClRdOnlyQual n) d = d
  make (CClWrOnlyQual n) d = d

instance Get CTypeSpecifier where
  make (CShortType a) d = d {scalar = Just (SInt 16)}
  make (CLongType a) d = d {scalar = Just (SInt 64)}
  make (CUnsigType a) d = d {is_unsigned = True}
  make (CVoidType a) d = d {scalar = Just SVoid}
  make (CCharType a) d = d {scalar = Just (SInt 8)}
  make (CIntType a) d = d {scalar = Just (SInt 32)}
  make (CFloatType a) d = d {scalar = Just (SFloat 32)}
  make (CDoubleType a) d = d {scalar = Just (SFloat 64)}
  make (CBoolType a) d = d {scalar = Just (SInt 1)}
  make (CSUType s a) d = d {struct_type = Just s}
  make (CTypeDef ((Ident s _ _)) a) d = d {typedef_name = Just s}
  make _ d = d

data Scalar = SVoid | SInt Int | SFloat Int deriving (Show)

data BaseDtype a = BaseDtype
  { scalar :: Maybe Scalar,
    struct_type :: Maybe (CStructureUnion a),
    typedef_name :: Maybe String,
    is_typedef :: Bool,
    is_unsigned :: Bool,
    is_const :: Bool
  }
  deriving (Show)

mkBaseType = foldr make (BaseDtype Nothing Nothing Nothing False False False)

genConstant' (CIntConst (CInteger i c fc) _) = Int (fromInteger i) 32 True
genConstant' (CFloatConst (CFloat val) _) = case last val of
  'f' -> Float (realToFrac ((read (init val)) :: Float)) 32
  _ -> Float (read val) 64
genConstant' (CStrConst (CString s _) _) = String (s)
genConstant' x = error (show x)

assignOp :: CAssignOp -> CBinaryOp
assignOp CDivAssOp = CDivOp
assignOp CRmdAssOp = CRmdOp
assignOp CAddAssOp = CAddOp
assignOp CSubAssOp = CSubOp
assignOp CShlAssOp = CShlOp
assignOp CShrAssOp = CShrOp
assignOp CAndAssOp = CAndOp
assignOp CXorAssOp = CXorOp
assignOp COrAssOp = COrOp

identName (Ident x _ _) = x
