{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Ir.Printer where

import Data.List
import Data.Map qualified as M
import Ir.Types
import Language.C
import Numeric
import Text.PrettyPrint
import Util
import Prelude hiding ((<>))

instance Print TranslationUnit where
  write (TranslationUnit decs structs) = vcat (map writeStruct (M.toList structs)) $$ vcat (write <$> sortBy f decs)
    where
      f (name, Variable {}) (name1, Variable {}) = EQ
      f (name, Variable {}) (name1, _) = LT
      f (name, _) (name1, Variable {}) = GT
      f (name, _) (name1, _) = EQ
      writeStruct (name, DStruct (TyStruct n xs _ _)) =
        text "struct" <+> text n <+> text ":" <+> text "{"
          <+> hsep (punctuate (text ",") $ map writeField xs)
          <+> text "}"
      writeStruct _ = undefined
      writeField (Named n d) = maybe (text "%anon") text n <> text ":" <> write d

instance Print (String, Declaration) where
  write (name, Function sig mdef) =
    write (name, sig) <+> text "{" $+$ write mdef
      $+$ text "}"
  write (name, Variable dtype x) = text "var" <+> write dtype <+> text "@" <> text name <+> maybe empty (\w -> text "=" <+> write w) x

instance Print Initializer where
  write (InitConst Unit) = text "unit"
  write (InitConst (Undef d)) = text "undef"
  write (InitConst (Int i i2 b)) = if i < 0 then text "-" <> parens (int (-i)) else int i
  write (InitConst (Float d i)) = if d < 0 then text "-" <> parens (text (showFFloat Nothing (-d) "")) else text (showFFloat Nothing d "")
  write (InitConst (GlobalVariable name d)) = text "@" <> text name
  write (InitConst (String s)) = doubleQuotes (text s)
  write (InitList xs) = text "{" <> hsep (punctuate (text ",") (fmap (write) xs)) <> text "}"

enumerate prefix xs = [(text "%" <> prefix <> int i <> text ":") <> p | (i, p) <- zip [0 ..] xs]

instance Print FunctionDefinition where
  write (FunctionDefinition allocs blocks initbid) =
    text "init:"
      $$ nest 2 (text "bid:" <+> write initbid)
      $$ nest 2 (text "allocations:" $$ nest 2 (vcat (enumerate (text "l") (fmap write allocs))))
      $$ write ((initbid,) $ blocks M.! initbid)
      $$ vcat (fmap write (filter ((initbid /=) . fst) $ M.toList blocks))

instance Print a => Print (Maybe a) where
  write Nothing = empty
  write (Just a) = write a

instance Print (BlockId, Block) where
  write (id, Block phis insts exit) =
    text "block" <+> write id <> text ":"
      $$ nest 2 (phis' $$ instrs $$ write exit)
    where
      instrs = vcat (enumerate (write id <> text ":i") (fmap (\w -> prepend w <+> write w) insts))
      prepend instr = write (getDtype instr) <+> text "="
      phis' = vcat (enumerate (write id <> text ":p") (fmap write phis))

instance Print BlockExit where
  write (Jump j) = text "j" <+> write j
  write (ConditionalJump o j1 j2) = text "br" <+> write o <> text "," <+> write j1 <> text "," <+> write j2
  write (Return o) = text "ret" <+> write o
  write (Switch o def xs) = text "switch" <+> write o <+> text "default" <+> write def <+> text "[" $$ nest 2 (vcat (fmap f xs)) $$ text "]"
    where
      f (c, o) = write c <+> write o
  write Unreachable = text "unreachable"

instance Print JumpArg where
  write (JumpArg bid args) = write bid <> parens (hsep $ punctuate (text ",") $ map write args)

instance Print Instruction where
  write Nop = text "nop"
  write (TypeCast o d) = text "typecast" <+> write o <+> text "to" <+> write d
  write (BinOp op l r d) = write op <+> write l <+> write r
  write (UnaryOp op l d) = write op <+> write l
  write (Store l r) = text "store" <+> write r <+> write l
  write (Load o) = text "load" <+> write o
  write (Call calee args d) = text "call" <+> write calee <> parens (hsep $ punctuate (text ",") $ map write args)
  write (GetElementPtr l r d) = text "getelementptr" <+> write l <+> text "offset" <+> write r

instance Print Operand where
  write (Constant x) = write x
  write (Register (Local i) d) = text "%l" <> int i <> text ":" <> write d
  write (Register (Arg bid i) d) = text "%" <> write bid <> text ":" <> text "p" <> int i <> text ":" <> write d
  write (Register (Temp bid i) d) = text "%" <> write bid <> text ":" <> text "i" <> int i <> text ":" <> write d

instance Print Constant where
  write Unit = text "unit:unit"
  write ((Undef d)) = text "undef" <> text ":" <> write d
  write x@((Int i i2 b)) = int i <> text ":" <> write (getDtype x)
  write ((Float d i)) = text (showFFloat Nothing d "") <> text ":" <> write (DFloat i False)
  write ((GlobalVariable name d)) = text "@" <> text name <> text ":" <> write d
  write x@(String s) = doubleQuotes (text s) <> text ":" <> write (getDtype x)

instance Print BlockId where
  write (BlockId i) = text "b" <> int i

instance Print (String, FunctionSignature) where
  write (name, FunctionSignature ret args) =
    text "fun" <+> write ret <+> text "@" <> text name
      <+> parens (hsep $ punctuate (text ",") $ map write args)

instance Print Dtype where
  write (DUnit b) = text "unit"
  write (DInt i True b3) = text "i" <> int i
  write (DInt i False b3) = text "u" <> int i
  write (DFloat i b) = text "f" <> int i
  write (DPointer d b) = write d <> text "*" <> if b then text "const" else empty
  write (DArray d i) = text "[" <> int i <+> text "x" <+> write d <> text "]"
  write (DFunction (FunctionSignature ret args)) = text "[ret:" <> write ret <+> text "params:" <> parens (hsep $ punctuate (text ",") $ map write args) <> text "]"
  write (DStruct (TyStruct maybeName xs is_const mpiil_i)) = (if is_const then text "const" else empty) <+> text "struct" <+> text maybeName

instance Print (Named Dtype) where
  write (Named mname dtype) = write dtype <> maybe empty ((text ":" <>) . text) mname

instance Print CBinaryOp where
  write o = case o of
    CMulOp -> text "mul"
    CDivOp -> text "div"
    CRmdOp -> text "mod"
    CAddOp -> text "add"
    CSubOp -> text "sub"
    CEqOp -> text "cmp eq"
    CNeqOp -> text "cmp ne"
    CLeOp -> text "cmp lt"
    CLeqOp -> text "cmp le"
    CGrOp -> text "cmp gt"
    CGeqOp -> text "cmp ge"
    CShlOp -> text "shl"
    CShrOp -> text "shr"
    CXorOp -> text "xor"
    COrOp -> text "or"
    CAndOp -> text "and"

instance Print CUnaryOp where
  write o = case o of
    CPlusOp -> text "plus"
    CMinOp -> text "minus"
    CNegOp -> text "negate"