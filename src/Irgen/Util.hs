{-# LANGUAGE ConstraintKinds #-}

module Irgen.Util where

import Ir.Types

arrayToPointer :: Dtype -> Dtype
arrayToPointer (DArray d _) = DPointer d False
arrayToPointer x = x

pointer :: Dtype -> Dtype
pointer d = DPointer d False

toPointer :: Operand -> Operand
toPointer (Constant (GlobalVariable a d)) = Constant (GlobalVariable a (DPointer d False))
toPointer (Register r d) = Register r (DPointer d False)

func :: Dtype -> (Dtype, [Dtype])
func (DPointer (DFunction (FunctionSignature ret args)) _) = (ret, args)
func ((DFunction (FunctionSignature ret args))) = (ret, args)

isFn (DPointer (DFunction (FunctionSignature ret args)) _) = True
isFn (DFunction (FunctionSignature ret args)) = True
isFn _ = False