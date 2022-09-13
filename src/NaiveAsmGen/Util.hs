module NaiveAsmGen.Util where

import Asm.Types
import Control.Monad.State
import Data.Map qualified as M
import Ir.Types qualified as I

data State = State
  { locations :: M.Map I.RegisterId (Int, I.Dtype),
    top :: Int,
    fName :: String,
    ds :: [(String, Directive)],
    declCounter :: Int
  }

addDecl directive = do
  counter <- gets declCounter
  modify (\s -> s {declCounter = declCounter s + 1})
  let name = ".temp" ++ show counter
  modify (\s -> s {ds = (".temp" ++ show counter, directive) : ds s})
  return name

emptyState = State M.empty 16 "" [] 0

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
