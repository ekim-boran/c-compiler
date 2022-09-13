module Ir.Types where

import Data.List (intercalate)
import Data.Map qualified as M
import Data.Maybe
import Language.C
  ( CBinaryOp,
    CInitializer,
    CUnaryOp,
    NodeInfo,
  )

data TranslationUnit = TranslationUnit
  { decls :: [(String, Declaration)],
    structs :: M.Map String Dtype
  }
  deriving (Eq, Show)

data Initializer = InitConst Constant | InitList [Initializer] deriving (Show, Eq)

data Declaration
  = Variable {vdtype :: Dtype, initializer :: Maybe (Initializer)}
  | Function {signature :: FunctionSignature, definition :: FunctionDefinition}
  deriving (Show, Eq)

data FunctionSignature = FunctionSignature
  { ret :: Dtype,
    params :: [Dtype]
  }
  deriving (Eq, Show, Ord)

data FunctionDefinition = FunctionDefinition
  { allocations :: [Named Dtype],
    blocks :: M.Map BlockId (GBlock Operand),
    bid_init :: BlockId
  }
  deriving (Eq, Show)

newtype BlockId = BlockId Int deriving (Eq, Show, Ord)

type Blocks a = (M.Map BlockId (GBlock a))

type Block = GBlock Operand

data GBlock a = Block
  { phinodes :: [Named Dtype],
    instructions :: [GInstruction a],
    exit :: GBlockExit a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Operand
  = Constant Constant
  | Register {rid :: RegisterId, odtype :: Dtype}
  deriving (Eq, Show, Ord)

data Constant
  = Unit
  | Undef Dtype
  | Int Int Int Bool -- val width is_signed
  | Float Double Int -- val width
  | GlobalVariable String Dtype
  | String String
  deriving (Eq, Show, Ord)

type LocalId = Int

data RegisterId
  = Local LocalId
  | Arg BlockId Int
  | Temp BlockId Int
  deriving (Eq, Show, Ord)

type Instruction = GInstruction Operand

type BlockExit = GBlockExit Operand

type JumpArg = GJumpArg Operand

type IsConst = Bool

type Width = Int

type IsSigned = Bool

data TyStruct = TyStruct
  { sname :: String,
    fields :: [Named Dtype],
    is_sconst :: Bool,
    size_align_offsets :: (Int, Int, [Int])
  }

instance Eq TyStruct where
  (TyStruct nm1 _ _ _) == (TyStruct nm2 _ _ _) = nm1 == nm2

instance Ord TyStruct where
  (TyStruct nm1 _ _ _) <= (TyStruct nm2 _ _ _) = nm1 <= nm2

data Dtype
  = DUnit IsConst
  | DInt Width IsSigned IsConst
  | DFloat Width IsConst
  | DPointer Dtype IsConst
  | DArray Dtype Int -- type size
  | DStruct TyStruct
  | DFunction FunctionSignature
  deriving (Eq, Ord)

instance Show Dtype where
  show (DUnit a) = "()"
  show (DInt w isSigned c) = if isSigned then "i" <> (show w) else "u" <> (show w)
  show (DFloat a b) = "f" <> (show a)
  show (DPointer a b) = "*" <> show a
  show (DArray a b) = "[" ++ show a ++ ";" ++ show b ++ "]"
  show (DStruct a) = "struct " ++ show (sname a) ++ (show $ size_align_offsets a)
  show (DFunction (FunctionSignature ret args)) = if null args then "()->" ++ show ret else (intercalate "->" (show <$> args)) ++ "->" ++ show ret

mkConst :: IsConst -> Dtype -> Dtype
mkConst x (DUnit b) = DUnit x
mkConst x (DInt w s _) = DInt w s x
mkConst x (DFloat w _) = DFloat w x
mkConst x (DPointer d b) = DPointer d x
mkConst x (DStruct t) = DStruct (t {is_sconst = x})
mkConst x d = d

getInner (DPointer inner _) = inner
getInner (DArray inner _) = inner

class HasDtype a where
  getDtype :: a -> Dtype

instance HasDtype Dtype where
  getDtype i = i

instance HasDtype Declaration where
  getDtype (Variable d mi) = d
  getDtype (Function x mf) = DFunction x

instance HasDtype Instruction where
  getDtype Nop = DUnit False
  getDtype (BinOp c1 o o3 d) = d
  getDtype (UnaryOp c1 o d) = d
  getDtype (Store o o2) = DUnit False
  getDtype (Load o) = getInner (getDtype o)
  getDtype (Call o l_o d) = d
  getDtype (TypeCast o d) = d
  getDtype (GetElementPtr o o2 d) = d

instance HasDtype Operand where
  getDtype (Constant c) = getDtype c
  getDtype (Register (Local i) d) = d
  getDtype (Register (Arg i i3) d) = d
  getDtype (Register (Temp i i3) d) = d

instance HasDtype Constant where
  getDtype Unit = DUnit False
  getDtype ((Undef d)) = DUnit False
  getDtype ((Int i i2 b)) = DInt i2 b False
  getDtype ((Float d i)) = DFloat i False
  getDtype ((GlobalVariable l_c d)) = d
  getDtype (String _) = stringTy

stringTy :: Dtype
stringTy = DPointer (DInt 8 True False) False

data GInstruction a
  = Nop
  | BinOp {bop :: CBinaryOp, lhs :: a, rhs :: a, idtype :: Dtype}
  | UnaryOp {uop :: CUnaryOp, operand :: a, idtype :: Dtype}
  | Store {ptr :: a, value :: a}
  | Load {ptr :: a}
  | Call {callee :: a, args :: [a], return_type :: Dtype}
  | TypeCast {value :: a, target_dtype :: Dtype}
  | GetElementPtr {ptr :: a, offset :: a, idtype :: Dtype}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data GBlockExit a
  = Jump (GJumpArg a)
  | ConditionalJump a (GJumpArg a) (GJumpArg a)
  | Switch a (GJumpArg a) [(Constant, GJumpArg a)]
  | Return a
  | Unreachable
  deriving (Eq, Show, Functor, Foldable, Traversable)

data GJumpArg a = JumpArg
  { jbid :: BlockId,
    jargs :: [a]
  }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Named a = Named
  { name :: Maybe String,
    item :: a
  }
  deriving (Eq, Show, Ord, Functor)

byte d = (d + 7) `div` 8

width :: Dtype -> Int
width (DUnit b) = 1
width (DInt i b b3) = byte i
width (DFloat i b) = byte i
width (DPointer d b) = 8
width (DArray d i) = width d * i
width (DStruct (TyStruct _ (xs) _ ((w, a, offsets)))) = w
width (DFunction _) = 8

align :: Dtype -> Int
align (DUnit b) = 1
align (DInt i b b3) = byte i
align (DFloat i b) = byte i
align (DPointer d b) = 8
align (DArray d i) = align d
align (DStruct (TyStruct _ xs _ (w, a, offsets))) = a
align (DFunction _) = 8

calculateOffsets1 xs = (size, alignofStruct, ls)
  where
    helper y a =
      let (d, m) = divMod y a
       in if m == 0 then d * a else (d + 1) * a
    offsets = scanl f (0, 0) as
    ls = fst <$> tail offsets
    size = snd $ last offsets
    alignofStruct = maximum $ snd <$> as
    as = [(width d, align d) | (Named _ d) <- xs]
    f (x, y) (w, a) = (helper y a, helper (y + w) a)

boolTy = DInt 1 False False

unsignedLongTy = DInt 64 False False

intTy = DInt 32 True False

floatTy = DFloat 32 False

true = Int 1 1 False

false = Int 0 1 False

unit = DUnit False

getField :: Dtype -> String -> [(Dtype, Int)]
getField (DStruct (TyStruct _ xs _ (_, _, offsets))) name = do
  case xs' of
    [] -> head noname
    _ -> xs'
  where
    xs' = [(d, i) | (Named n d, i) <- zip xs offsets, Just name == n]
    noname = [(d, i) : getField d name | (Named n d, i) <- zip xs offsets, not $ isJust n, not $ null $ getField d name]
getField (DPointer d _) name = getField d name
getField _ _ = []
