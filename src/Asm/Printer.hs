module Asm.Printer where

import Asm.Types
import Language.C.Analysis (TypeName (TyFloating))
import Numeric (showHex)
import Text.PrettyPrint
import Text.Printf
import Util
import Prelude hiding ((<>))

instance Print Directive where
  write (DAlign value) = text ".align" <+> int value
  write (DGlobl label) = text $ ".globl " ++ label
  write (DType symbol symbol_type) = text ".type" <+> write symbol <+> write symbol_type
  write (DSection section_type) = text ".section" <+> write section_type
  write (DByte value) = text ".byte  " <> text (printf "0x%hhx" value)
  write (DHalf value) = text ".half  " <> text (printf "0x%hx" value)
  write (DWord value) = text ".word  " <> text (printf "0x%lx" value)
  write (DQuad value) = text ".quad  " <> text (printf "0x%llx" value)
  write (DZero bytes) = text ".zero " <+> integer bytes
  write (DAscii str) = text ".asciz " <+> (doubleQuotes $ text str)

instance Print SectionType where
  write SText = text ".text"
  write SData = text ".data"
  write SRodata = text ".rodata"
  write SBss = text ".bss"

instance Print SymbolType where
  write SFunction = text "@function"
  write SObject = text "@object"

instance Print TranslationUnit where
  write (TranslationUnit fs vs) = vcat (map write fs) $$ vcat (map write vs)

instance (Print a) => Print (Section a) where
  write (Section directives body) = nest 4 (vcat (map write directives)) $$ write body

instance Print Function where
  write (Function blocks) = vcat (map write blocks)

instance Print Variable where
  write (Variable label directives) = write label <> text ":" $$ nest 4 (vcat (map (write) directives))

instance Print Label where
  write s = text s

instance Print a => Print (Maybe a) where
  write Nothing = empty
  write (Just a) = write a

instance Print Block where
  write (Block mlabel instrs) = write mlabel <> text ":" $$ (nest 4 $ vcat ((map write instrs)))

instance Print Instruction where
  write (RType i rd rs1 rs2) = write i <+> write rd <> text "," <> write rs1 <> maybe empty (\a -> text "," <> write a) rs2 <+> rounding_mode i
    where
      rounding_mode FcvtFloatToInt {} = text ",rtz"
      rounding_mode _ = empty
  write (IType i@(Load _ _) reg1 reg2 imm) = write i <+> write reg2 <> text "," <> write imm <> parens (write reg1)
  write (IType i rd rs1 imm) = write i <+> write rd <> text "," <> write rs1 <> text "," <> write imm
  write (SType i reg1 reg2 imm) = write i <+> write reg2 <> text "," <> write imm <> parens (write reg1)
  write (BType b reg1 reg2 label) = write b <+> write reg1 <> text "," <> write reg2 <> text "," <+> write label
  write (UType u rd imm) = write u <+> write rd <+> write imm
  write (Pseudo p) = write p

sign False = text "u"
sign True = empty

is_integer o = o `elem` [Byte, Half, Word, Double]

instance Print RType where
  write (Add msize) = text "add" <> write msize
  write (Sub msize) = text "sub" <> write msize
  write (Sll msize) = text "sll" <> write msize
  write (Srl msize) = text "srl" <> write msize
  write (Sra msize) = text "sra" <> write msize
  write (Mul msize) = text "mul" <> write msize
  write (Div msize signed) = text "div" <> sign signed <> write msize
  write (Rem msize signed) = text "rem" <> sign signed <> write msize
  write (Slt signed) = text "slt" <> sign signed
  write Xor = text "xor"
  write Or = text "or"
  write And = text "and"
  write (Fadd size) = text "fadd." <> write size
  write (Fsub size) = text "fsub." <> write size
  write (Fmul size) = text "fmul." <> write size
  write (Fdiv size) = text "fdiv." <> write size
  write (Feq size) = text "feq." <> write size
  write (Flt size) = text "flt." <> write size
  write (FmvIntToFloat size) = text "fmv.x." <> if size == SinglePrecision then text "w" else text "d"
  write (FmvFloatToInt size) = text "fmv." <> if size == SinglePrecision then text "w" else text "d" <> text ".x"
  write (FcvtFloatToInt float_size int_size signed) = text "fcvt." <> maybe (text "l") f int_size <> sign signed <> text "." <> write float_size
    where
      f s = if s == Word then text "w" else text "l"
  write (FcvtIntToFloat int_size float_size signed) = text "fcvt." <> write float_size <> text "." <> maybe (text "l") f int_size <> sign signed
    where
      f s = if s == Word then text "w" else text "l"
  write (FcvtFloatToFloat from to) = text "fcvt." <> write to <> text "." <> write from

instance Print SType where
  write (Store size)
    | is_integer size = text "s" <> write size
    | size == SinglePrecision = text "fsw"
    | otherwise = text "fsd"

instance Print BType where
  write Beq = text "beq"
  write Bne = text "bne"
  write (Blt signed) = text "blt" <> sign signed
  write (Bge signed) = text "bge" <> sign signed

instance Print UType where
  write x = text "lui"

instance Print Pseudo where
  write (La rd label) = text "la" <+> write rd <> text "," <> text label
  write (Li rd imm) = text "li" <+> write rd <> text "," <> int imm
  write (Mv rd rs) = text "mv" <+> write rd <> text "," <> write rs
  write (Fmv size rd rs) = text "fmv." <> write size <+> write rd <> text "," <> write rs
  write (Neg msize rd rs) = text "neg" <> write msize <+> write rd <> text "," <> write rs
  write (SextW rs rd) = text "sext.w" <+> write rd <> text "," <> write rs
  write (Seqz rd rs) = text "seqz" <+> write rd <> text "," <> write rs
  write (Snez rd rs) = text "snez" <+> write rd <> text "," <> write rs
  write (Fneg size rd rs) = text "fneg." <> write size <+> write rd <> text "," <> write rs
  write (J offset) = text "j" <+> text offset
  write (Jr rs) = text "jr" <+> write rs
  write (Jalr rs) = text "jalr" <+> write rs
  write Ret = text "ret"
  write (Call offset) = text "call" <+> text offset

instance Print IType where
  write (Load size signed)
    | is_integer size =
        if size == Double
          then text "l" <> write size
          else text "l" <> write size <> sign signed
    | size == SinglePrecision = text "flw"
    | size == DoublePrecision = text "fld"
  write (Addi msize) = text "addi" <> write msize
  write Xori = text "xori"
  write Ori = text "ori"
  write Andi = text "andi"
  write (Slli msize) = text "slli" <> write msize
  write (Srli msize) = text "srli" <> write msize
  write (Srai msize) = text "srai" <> write msize
  write (Slti signed) = text "slti" <> sign signed

instance Print Register where
  write Zero = text "zero"
  write Ra = text "ra"
  write Sp = text "sp"
  write Gp = text "gp"
  write Tp = text "tp"
  write (Temp ty i) = write ty <> text "t" <> int i
  write (Saved ty i) = write ty <> text "s" <> int i
  write (Arg ty i) = write ty <> text "a" <> int i

instance Print RegisterType where
  write RFloatingPoint = text "f"
  write _ = empty

instance Print DataSize where
  write Byte = text "b"
  write Half = text "h"
  write Word = text "w"
  write Double = text "d"
  write SinglePrecision = text "s"
  write DoublePrecision = text "d"

instance Print Immediate where
  write (Value v) = int v
  write (Relocation f label) = write f <> parens (text label)

instance Print RelocationFunction where
  write Hi20 = text "%hi"
  write Lo12 = text "%lo"
