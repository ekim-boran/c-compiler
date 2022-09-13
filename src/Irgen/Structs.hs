module Irgen.Structs where

import Data.Map qualified as M
import Ir.Types
import Ir.Utils (ReplaceDtype (replaceDType), foldInstructions, foldMapInstructions, mapInstructions, removeInstructions, replaceOperand')
import Irgen.Util (pointer)

isStructPointer (DPointer (DStruct _) _) = True
isStructPointer _ = False

fn ty = (Constant (GlobalVariable "memcpy" (DPointer (DFunction (FunctionSignature (DUnit False) [ty, ty, DInt 32 True False])) False)))

processStruct a@(x, (Variable {})) = a
processStruct a@((x, Function sig (def))) =
  if x /= "memcpy"
    then (x, goTypes $ goStructReturn $ Function sig (goCallStructReturn $ firstPass def))
    else a

firstPass def@(FunctionDefinition allocs blocks init) =
  ( FunctionDefinition
      allocs
      (goStructParam init $ goStore $ removeLoad $ goLoad $ blocks)
      init
  )

goLoad blocks = replaceOperand' replacer blocks
  where
    replacer = foldInstructions go M.empty blocks
    go bid i (Load ptr) map = case getDtype ptr of
      (DPointer t@(DStruct _) _) -> M.insert (Register (Temp bid i) t) ptr map
      _ -> map
    go _ _ _ map = map

removeLoad blocks = (removeInstructions f blocks)
  where
    f _ _ (Load ptr) = case getDtype ptr of
      (DPointer t@(DStruct _) _) -> False
      _ -> True
    f _ _ _ = True

goStore blocks = (mapInstructions go blocks)
  where
    go bid i instr@(Store ptr a) = case getDtype ptr of
      ptrType@(DPointer t@(DStruct _) _) -> Call (fn ptrType) [ptr, a, Constant (Int (width t) 32 True)] (DUnit False)
      _ -> instr
    go _ _ i = i

goStructParam bid_init blocks = M.map (\b -> b {phinodes = fmap (fmap go) (phinodes b)}) $ replaceOperand' replacer blocks
  where
    initBlock = blocks M.! bid_init
    replacer =
      M.fromList
        [ (Register (Arg bid_init i) t, Register (Arg bid_init i) (DPointer t False))
          | (i, (Named _ t@DStruct {})) <- zip [0 ..] (phinodes initBlock)
        ]
    go t@(DStruct {}) = pointer t
    go x = x

goStructReturn (Function s@(FunctionSignature t@(DStruct {}) args) (a@(FunctionDefinition {..}))) =
  Function s (a {blocks = M.mapWithKey go blocks})
  where
    go bid (Block phinodes instrs (Return x)) = Block (phinodes ++ [Named Nothing ptrType]) (instrs ++ [i]) (Return (Constant Unit))
      where
        ptrType = DPointer (t) False
        i = Call (fn ptrType) [(Register (Arg bid (length phinodes)) ptrType), x, Constant (Int (width t) 32 True)] (DUnit False)
    go bid x = x
goStructReturn x = x

goCallStructReturn (a@(FunctionDefinition {..})) = FunctionDefinition allocs (replaceOperand' replaceMap blocks') bid_init
  where
    ((allocs, replaceMap), blocks') = foldMapInstructions go (allocations, M.empty) blocks
    go bid index (Call calee args t@(DStruct {})) (allocs, replaceMap) = (Call calee (args ++ [newArg]) t, ((allocs ++ [newAlloc]), insert replaceMap))
      where
        newAlloc = Named (Just $ "t" <> (show $ length allocs)) (t)
        newArg = Register (Local (length allocs)) (DPointer t False)
        insert = M.insert (Register (Temp bid index) (DPointer t False)) (newArg) . M.insert (Register (Temp bid index) t) (newArg)
    go bid index i xs = (i, xs)

goTypes i = replaceDType f i
  where
    f (DFunction (FunctionSignature t@(DStruct _) args)) = DFunction $ FunctionSignature (DUnit False) ((g <$> args) ++ [DPointer t False])
    f (DFunction (FunctionSignature ret args)) = DFunction $ FunctionSignature ret (g <$> args)
    f t = t
    g t@(DStruct {}) = DPointer t False
    g t = t