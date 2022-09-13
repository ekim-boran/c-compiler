{-# LANGUAGE ConstraintKinds #-}

module Irgen.GenUtil where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.ByteString qualified as B
import Data.Foldable (traverse_)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Ir.Types

type Compiler m = (MonadState FunContext m, MonadError String m, MonadFail m)

data FunContext = FunContext
  { _funDict :: M.Map String Operand,
    _bidCounter :: Int,
    _committed :: M.Map BlockId Block,
    _allocs :: [Named Dtype],
    _uncommited ::
      M.Map
        BlockId
        ( [Named Dtype],
          [Instruction]
        ),
    _bid :: BlockId,
    _postid :: BlockId,
    _contid :: BlockId, -- loop post and cont
    _tempVarCounter :: Int
  }

emptyFunContext :: FunContext
emptyFunContext = FunContext M.empty 0 M.empty [] M.empty (BlockId 0) (BlockId 0) (BlockId 0) 0

$(makeLenses ''FunContext)

insertSym :: (Compiler m) => String -> Operand -> m ()
insertSym name operand = modify $ over funDict (M.insert name operand)

add' :: (Compiler m) => String -> Dtype -> m Operand
add' name d = add (Named (Just name) d)

add :: (Compiler m) => Named Dtype -> m Operand
add (Named name d) = do
  name' <- case name of
    Nothing -> mkName
    (Just x) -> return x
  len <- gets (length . _allocs)
  let op = Register (Local len) d
  insertSym name' op
  modify $ over allocs (++ [Named (Just name') d])
  lookupSym' name'

lookupSym' :: (Compiler m) => String -> m Operand
lookupSym' string = do
  map <- gets _funDict
  maybe (throwError ("x lookup failed" ++ string)) return $ M.lookup string map

-- new block scope -- new bindings are only valid in this scope
newBlockScope :: (Compiler m) => m b -> m b
newBlockScope action = do
  s <- gets _funDict
  a <- action
  modify (set funDict s)
  return a

-- function scope
newFunctionScope :: (MonadState a m) => m b -> m b
newFunctionScope action = do
  s <- get
  a <- action
  put s
  return a

-- create a new block
newBlock :: (Compiler m) => Int -> m [BlockId]
newBlock n =
  replicateM n $ do
    modify (over bidCounter (+ 1))
    bid <- gets (BlockId . _bidCounter)
    modify (over uncommited (M.insertWith (\_ x -> x) bid ([], [])))
    return bid

withBlock :: (Compiler m) => BlockId -> m b -> m b
withBlock bid' f = modify (set bid bid') >> (newBlockScope f)

mkName :: (Compiler m) => m String
mkName = do
  n <- gets _tempVarCounter
  modify (over tempVarCounter (+ 1))
  return $ "t" ++ show n

commitFunction :: (Compiler m) => Dtype -> m FunctionDefinition
commitFunction ret = do
  us <- gets _uncommited
  traverse_ (\(bid, _) -> withBlock bid $ commitBlock (Return returnType)) (M.toList us)
  FunContext {..} <- get
  return $ FunctionDefinition _allocs _committed (BlockId 0)
  where
    returnType = Constant $ case ret of
      DUnit _ -> Unit
      _ -> Undef ret

commitBlock :: (Compiler m) => BlockExit -> m ()
commitBlock exit = do
  FunContext {..} <- get
  case (M.lookup _bid _committed, M.lookup _bid _uncommited) of
    (Just _, _) -> return () -- already committed skip
    (_, Nothing) -> modify (over committed $ M.insert _bid (Block [] [] exit)) -- add and commitFunction
    (_, Just (phis, instrs)) ->
      do
        modify (over committed $ M.insert _bid (Block phis instrs exit)) -- commitFunction block
        modify (over uncommited $ M.delete _bid)