module Simplify.Types where

import Control.Applicative ((<|>))
import Control.Arrow (second)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Ir.Types (Constant (Float, Int, Undef, Unit), Dtype (DArray, DFloat, DFunction, DInt, DPointer, DStruct, DUnit), FunctionSignature (FunctionSignature), HasDtype (getDtype), Named (Named, item), Operand (Constant), getInner, mkConst, width)
import Irgen.GenUtil (newFunctionScope)
import Irgen.Util hiding (get)
import Language.C
import Simplify.Util

data Initializer = InitExpr Expr | InitStruct [(Int, Dtype, Initializer)] | InitArray Dtype [(Int, Initializer)]
  deriving (Eq, Show)

data TranslationUnit = TranslationUnit
  { decls :: [Decl],
    structs :: M.Map String Dtype
  }
  deriving (Eq, Show)

data Decl = Function String Dtype [Named Dtype] [Statement] | Variable VarDecl deriving (Eq, Show)

data VarDecl = VarDecl String Dtype (Maybe Initializer) deriving (Eq, Show)

data Statement
  = DeclStmt VarDecl
  | RetStmt Expr
  | ForStmt [Statement] Expr Expr [Statement]
  | ContStmt
  | BreakStmt
  | IfStmt Expr [Statement] [Statement]
  | ExprStmt Expr
  | SwitchStmt Expr (Maybe Case) [Case]
  | CaseStmt Case
  deriving (Eq, Show)

data Case = Case (Maybe Constant) [Statement] deriving (Eq, Show)

data Expr
  = CastExpr Expr Dtype
  | ConstExpr Constant
  | EmptyExpr
  | VarExpr String Dtype
  | CommaExpr [Expr] Dtype
  | AssignExpr Expr Expr
  | BinaryExpr CBinaryOp Expr Expr Dtype
  | UnaryExpr CUnaryOp Expr
  | CondExpr Expr Expr Expr
  | IndirectionExpr Expr
  | AddressOfExpr Expr
  | CallExpr Expr [Expr]
  | GetElementPtr Expr Expr Dtype
  | PostOpExpr Expr Expr
  deriving (Eq, Show)

instance HasDtype Decl where
  getDtype (Variable (VarDecl _ d _)) = mkConst False d
  getDtype (Function _ ret params _) = DFunction (FunctionSignature ret (item <$> params))

instance HasDtype Expr where
  getDtype x = mkConst False $ case x of
    (CastExpr _ d) -> d
    (ConstExpr c) -> getDtype c
    EmptyExpr -> DUnit False
    (VarExpr name d) -> d
    (CommaExpr xs d) -> d
    (AssignExpr a b) -> getDtype b
    (BinaryExpr op a b d) -> d
    (UnaryExpr op a) -> getDtype a
    (IndirectionExpr a) -> getInner (getDtype a)
    (AddressOfExpr a) -> pointer $ getDtype a
    (CallExpr f _) -> let (DFunction (FunctionSignature ret _)) = getInner $ getDtype f in ret
    (GetElementPtr ptr a dtype) -> dtype
    (CondExpr c a b) -> getDtype a
    (PostOpExpr a b) -> getDtype a
