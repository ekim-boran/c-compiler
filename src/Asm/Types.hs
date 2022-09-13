{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Asm.Types where

import Ir.Types (Dtype (DFloat))
import Ir.Types qualified as I
import Language.C
import Util

data TranslationUnit = TranslationUnit
  { functions :: [Section Function],
    variables :: [Section Variable]
  }
  deriving (Show, Eq, Ord)

data Section a = Section
  { header :: [Directive],
    body :: a
  }
  deriving (Show, Eq, Ord)

newtype Function = Function [Block]
  deriving (Show, Eq, Ord)

data Variable = Variable Label [Directive]
  deriving (Show, Eq, Ord)

data Block = Block (Maybe Label) [Instruction]
  deriving (Show, Eq, Ord)

data Directive
  = DAlign Int
  | DGlobl Label
  | DSection SectionType
  | DType Label SymbolType
  | DByte Integer
  | DHalf Integer
  | DWord Integer
  | DQuad Integer
  | DZero Integer
  | DAscii String
  deriving (Show, Eq, Ord)

data SectionType
  = SText
  | SData
  | SRodata
  | SBss
  deriving (Show, Eq, Ord)

data SymbolType = SFunction | SObject
  deriving (Show, Eq, Ord)

data Instruction
  = RType RType Register Register (Maybe Register) -- rd rs1 rs2
  | IType IType Register Register Immediate -- rd rs1 imm
  | SType SType Register Register Immediate -- rd rs1 imm
  | BType BType Register Register Label
  | UType UType Register Immediate
  | Pseudo Pseudo
  deriving (Show, Eq, Ord)

data RType
  = Add (Maybe DataSize)
  | Sub (Maybe DataSize)
  | Sll (Maybe DataSize)
  | Srl (Maybe DataSize)
  | Sra (Maybe DataSize)
  | Mul (Maybe DataSize)
  | Div (Maybe DataSize) ISSigned
  | Rem (Maybe DataSize) ISSigned
  | Slt ISSigned -- rd rs1 rs2
  | Xor
  | Or
  | And
  | Fadd DataSize
  | Fsub DataSize
  | Fmul DataSize
  | Fdiv DataSize
  | Feq DataSize
  | Flt DataSize
  | FmvIntToFloat DataSize
  | FmvFloatToInt DataSize
  | FcvtIntToFloat (Maybe DataSize) DataSize ISSigned -- int_data_size float_data_size is_signed
  | FcvtFloatToInt DataSize (Maybe DataSize) ISSigned -- float_data_size int_data_size is_signed
  | FcvtFloatToFloat DataSize DataSize -- from to
  deriving (Show, Eq, Ord)

data IType
  = Load DataSize ISSigned
  | Addi (Maybe DataSize)
  | Xori
  | Ori
  | Andi
  | Slli (Maybe DataSize)
  | Srli (Maybe DataSize)
  | Srai (Maybe DataSize)
  | Slti ISSigned
  deriving (Show, Eq, Ord)

newtype SType = Store DataSize
  deriving (Show, Eq, Ord)

type ISSigned = Bool

data BType
  = Beq
  | Bne
  | Blt ISSigned
  | Bge ISSigned
  deriving (Show, Eq, Ord)

data UType = Lui
  deriving (Show, Eq, Ord)

data Pseudo
  = La Register Label
  | Li Register Int
  | Mv Register Register -- rd rs
  | Fmv DataSize Register Register
  | Neg (Maybe DataSize) Register Register
  | SextW Register Register
  | Seqz Register Register
  | Snez Register Register
  | Fneg DataSize Register Register
  | J Label
  | Jr Register
  | Jalr Register
  | Ret
  | Call Label
  deriving (Show, Eq, Ord)

data Immediate
  = Value Int
  | Relocation RelocationFunction Label -- relocation symbol
  deriving (Show, Eq, Ord)

data RelocationFunction = Hi20 | Lo12
  deriving (Show, Eq, Ord)

type Label = String

data DataSize
  = Byte
  | Half
  | Word
  | Double
  | SinglePrecision
  | DoublePrecision
  deriving (Show, Eq, Ord)

data Register
  = Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | Arg RegisterType Int
  | Temp RegisterType Int
  | Saved RegisterType Int
  deriving (Show, Eq, Ord)

data RegisterType = RInteger | RFloatingPoint deriving (Show, Eq, Ord)

move target source d | target /= source = case d of
  (I.DFloat {}) -> [Pseudo (Fmv datasize' target source)]
  _ -> [Pseudo (Mv target source)]
  where
    (datasize', signed) = size_signed d
move target source d = []

mask n
  | n == 1 = 1
  | n == 8 = 255
  | n == 16 = 65535
  | n == 32 = 4294967295

typecast target source targetD sourceD =
  case (sourceD, targetD) of
    (I.DInt {}, I.DFloat {}) ->
      [RType (FcvtIntToFloat (Just size1) size2 signed2) target source Nothing]
    (I.DFloat {}, I.DInt {}) ->
      [RType (FcvtFloatToInt size1 (Just size2) signed2) target source Nothing]
    (I.DFloat w1 _, I.DFloat w2 _)
      | w1 /= w2 ->
          [RType (FcvtFloatToFloat size1 size2) target source Nothing]
    (I.DInt w1 _ _, I.DInt w2 _ _)
      | w1 > w2 && w2 == 8 ->
          [IType Andi target source (Value (mask w2))]
      | w1 > w2 ->
          IType (Slli Nothing) target source (Value (64 - w2)) : [IType (Srli Nothing) target target (Value (64 - w2))]
    (_, _) -> move target source targetD
  where
    (size1, signed1) = size_signed sourceD
    (size2, signed2) = size_signed targetD

rType (Arg i _) = i
rType (Temp i _) = i
rType (Saved i _) = i
rType _ = RInteger

tempInt = Temp RInteger <$> [0 .. 6]

savedInt = Saved RInteger <$> [1 .. 11]

argInt = Arg RInteger <$> [1 .. 7]

tempFloat = Temp RFloatingPoint <$> [0 .. 11]

savedFloat = Saved RFloatingPoint <$> [0 .. 11]

argFloat = Arg RFloatingPoint <$> [1 .. 7]

s0 = Saved RInteger 0

a0 = Arg RInteger 0

a1 = Arg RInteger 1

reg number (I.DFloat {}) = Arg RFloatingPoint number
reg number _ = Arg RInteger number

createBinOp op reg1 reg2 d@(DFloat {}) out = case op of
  CMulOp -> [RType (Fmul datasize') out reg1 (Just reg2)]
  CDivOp -> [RType (Fdiv datasize') out reg1 (Just reg2)]
  CAddOp -> [RType (Fadd datasize') out reg1 (Just reg2)]
  CSubOp -> [RType (Fsub datasize') out reg1 (Just reg2)]
  CEqOp -> [RType (Feq datasize') out reg1 (Just reg2)]
  CNeqOp -> [RType (Feq datasize') out reg1 (Just reg2), Pseudo (Snez out out)]
  CLeOp -> [RType (Flt datasize') out reg1 (Just reg2)]
  CLeqOp -> [RType (Flt datasize') out reg2 (Just reg1), IType Xori out out (Value 1)]
  CGrOp -> [RType (Flt datasize') out reg2 (Just reg1)]
  CGeqOp -> [RType (Flt datasize') out reg1 (Just reg2), IType Xori out out (Value 1)]
  _ -> undefined
  -- CShlOp -> [RType (Sll datasize') out reg1 (Just reg2)]
  -- CShrOp -> if signed then [RType (Sra datasize') out reg1 (Just reg2)] else [RType (Srl datasize') out reg1 (Just reg2)]
  -- CXorOp -> [RType Xor out reg1 (Just reg2)]
  -- COrOp -> [RType Or out reg1 (Just reg2)]
  -- CAndOp -> [RType And out reg1 (Just reg2)]
  where
    (datasize', signed) = size_signed d
createBinOp op reg1 reg2 d out =
  case op of
    CMulOp -> [RType (Mul datasize') out reg1 (Just reg2)]
    CDivOp -> [RType (Div datasize' signed) out reg1 (Just reg2)]
    CRmdOp -> [RType (Rem datasize' signed) out reg1 (Just reg2)]
    CAddOp -> [RType (Add datasize') out reg1 (Just reg2)]
    CSubOp -> [RType (Sub datasize') out reg1 (Just reg2)]
    CEqOp -> [RType Xor out reg1 (Just reg2), Pseudo (Seqz out out)]
    CNeqOp -> [RType Xor out reg1 (Just reg2), Pseudo (Snez out out)]
    CLeOp -> [RType (Slt signed) out reg1 (Just reg2)]
    CLeqOp -> [RType (Slt signed) out reg2 (Just reg1), IType Xori out out (Value 1)]
    CGrOp -> [RType (Slt signed) out reg2 (Just reg1)]
    CGeqOp -> [RType (Slt signed) out reg1 (Just reg2), IType Xori out out (Value 1)]
    CShlOp -> [RType (Sll datasize') out reg1 (Just reg2)]
    CShrOp -> if signed then [RType (Sra datasize') out reg1 (Just reg2)] else [RType (Srl datasize') out reg1 (Just reg2)]
    CXorOp -> [RType Xor out reg1 (Just reg2)]
    COrOp -> [RType Or out reg1 (Just reg2)]
    CAndOp -> [RType And out reg1 (Just reg2)]
    _ -> undefined
  where
    (datasize1, signed) = size_signed d
    datasize' = Nothing

createUnaryOp op reg1 d out = case op of
  CPlusOp -> []
  CMinOp -> instr
  CNegOp -> instr1
  _ -> undefined
  where
    (dsize', signed) = size_signed d
    instr = case d of
      DFloat {} -> [Pseudo $ Fneg dsize' out reg1]
      _ -> [Pseudo $ Neg (if dsize' == Double then Nothing else Just dsize') out reg1]
    instr1 = case d of
      DFloat {} -> [Pseudo $ Seqz out reg1]
      _ -> [Pseudo $ Seqz out reg1]

floor' n a = if a `mod` n == 0 then a else floor' n (a + 1)

size_signed :: Dtype -> (DataSize, Bool)
size_signed f@(I.DFloat w _) = if floor' 8 w == 32 then (SinglePrecision, True) else (DoublePrecision, True)
size_signed (I.DInt w s _)
  | f == 8 = (Byte, s)
  | f == 16 = (Half, s)
  | f == 32 = (Word, s)
  | f == 64 = (Double, s)
  where
    f = floor' 8 w
size_signed (I.DPointer _ _) = (Double, True)
size_signed (I.DUnit _) = (Byte, True)
size_signed x = (Word, True)

li reg value = [Pseudo (Li reg value)]

la reg label = [Pseudo (La reg label)]

loadValue addrReg toReg offset dtype = [IType (uncurry Load (size_signed dtype)) addrReg toReg (Value (-offset))]

loadAddr addrReg toReg offset = [IType (Addi Nothing) toReg addrReg (Value (-offset))]

genGlobalDirective :: Int -> Integer -> Directive
genGlobalDirective w
  | w <= 8 = DByte
  | w <= 16 = DHalf
  | w <= 32 = DWord
  | w <= 64 = DQuad
  | otherwise = undefined

getFloatRepr width num = if width == 32 then fromIntegral $ float2WordBitwise (realToFrac num :: Float) else fromIntegral $ double2WordBitwise num

funHeader name =
  [ DGlobl name,
    DSection SText,
    DType name SFunction
  ]

preamble len =
  [ IType (Addi Nothing) Sp Sp (Value (-len)),
    SType (Store Double) Sp Ra (Value (len - 8)),
    SType (Store Double) Sp s0 (Value (len - 16)),
    IType (Addi Nothing) s0 Sp (Value len)
  ]

addfooter len (Block l instr) = Block l $ concatMap (\i -> if is_ret i then footer ++ [i] else [i]) instr
  where
    footer =
      [ IType (Load Double True) Sp s0 (Value (len - 16)),
        IType (Load Double True) Sp Ra (Value (len - 8)),
        IType (Addi Nothing) Sp Sp (Value len)
      ]
    is_ret (Pseudo Ret) = True
    is_ret _ = False

intReg number = Arg RInteger number

floatReg number = Arg RFloatingPoint number

anyReg number (I.DFloat {}) = floatReg number
anyReg number _ = intReg number

storeInstr r addr d = SType (Store size) s0 r (Value (-addr))
  where
    (size, signed) = size_signed d

loadInstr r addr d = IType (Load size signed) s0 r (Value (-addr))
  where
    (size, signed) = size_signed d
