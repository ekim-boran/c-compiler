module AsmGen.Util where

import Asm.Types (RegisterType, floor')
import Asm.Types qualified as A
import AsmGen.Allocator
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace
import Ir.Types
import Ir.Types qualified as I
import Util (mapMaybe)

data State = State
  { locations :: M.Map I.RegisterId (Int, I.Dtype),
    top :: Int,
    fName :: String,
    ds :: [(String, A.Directive)],
    declCounter :: Int,
    registers :: M.Map LivenessKey A.Register,
    livenessMap :: M.Map BlockId (M.Map Int (S.Set (LivenessKey, Dtype)))
  }

getUsed bid index = do
  livenessMap' <- gets livenessMap
  registers' <- gets registers
  return $
    case M.lookup bid livenessMap' >>= M.lookup index of
      Nothing -> []
      (Just xs) -> mapMaybe (\(k, d) -> (,d) <$> M.lookup k registers') (S.toList xs)

getRegister key = do
  r <- gets registers
  MaybeT $ return $ M.lookup (Rid key) r
getConstant bid index key = do
  r <- gets registers
  MaybeT $ return $ M.lookup (TempConstant bid index key) r

getRegisterTemp bid index = do
  r <- gets registers
  MaybeT $ return $ M.lookup (TempPhi bid index) r

addDecl directive = do
  counter <- gets declCounter
  modify (\s -> s {declCounter = declCounter s + 1})
  let name = ".temp" ++ show counter
  modify (\s -> s {ds = (".temp" ++ show counter, directive) : ds s})
  return name

emptyState = State M.empty 16 "" [] 0 M.empty M.empty

add rid dtype s@(State {..}) =
  s {locations = M.insert rid (start, dtype) locations, top = start}
  where
    start = floor' align (top + I.width dtype)
    align = I.align dtype

storeLoc rid dtype = do
  mindex <- gets (M.lookup rid . locations)
  case mindex of
    Nothing -> do
      modify (add rid dtype)
      top' <- gets top
      return (top', dtype)
    (Just x) -> return x

getLoc rid dtype = do
  mindex <- gets (M.lookup rid . locations)
  case mindex of
    Nothing -> error "not found"
    (Just x) -> return x

getLabel (I.BlockId bid) = do
  name <- gets fName
  return $ "." ++ name ++ "." ++ show bid

execute e = do
  e' <- runMaybeT e
  case e' of
    Nothing -> return []
    (Just x) -> return x
