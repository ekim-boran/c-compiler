module Opt.Gvn where

import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.List as L
import Data.Map qualified as M
import Data.Set qualified as S
import Graph
import Ir.Types
import Ir.Utils
import Language.C
import Opt.Util
import Util

type Number = Int

data OperandValue = C Constant | N Number deriving (Show, Eq, Ord)

newtype Expr = Expr {unexpr :: GInstruction OperandValue} deriving (Show)

instance Eq Expr where
  (==) (Expr ((BinOp o1 l1 r1 d1))) (Expr ((BinOp o2 l2 r2 d2))) = d1 == d2 && o1 == o2 && ((l1 == l2 && r1 == r2) || (isAssociative o1 && l1 == r2 && r1 == l2))
  (==) (Expr a) (Expr b) = a == b

isAssociative o = o `elem` [CMulOp, CAddOp, CAndOp, CXorOp, COrOp, CNeqOp, CEqOp]

data Gvn = Gvn
  { _leaderTable :: M.Map BlockId (M.Map Number Operand),
    _registerTable :: M.Map Operand Number,
    _exprTable :: HM.HashMap Expr Number,
    _phiTable :: M.Map BlockId [(Int, Dtype, [(BlockId, Operand)])],
    _counter :: Number,
    _tempPhi :: Int
  }
  deriving (Show)

$(makeLenses ''Gvn)

execute i@(BinOp op (Constant (Int val width b)) (Constant (Int val2 _ _)) _) =
  let (DInt w s _) = getDtype i
   in Just (Constant $ Int (executeOp op val val2) w s)
execute i@(BinOp op (Constant (Float val w)) (Constant (Float val2 _)) _) =
  let (DFloat v w) = getDtype i
   in (\f -> Constant $ Float (f val val2) v) <$> (executeOpFloat op)
execute _ = Nothing

instance Hashable Expr where hashWithSalt salt _d = salt

emptyGvn nodes = Gvn (M.fromList ((,M.empty) <$> nodes)) M.empty HM.empty M.empty 0 0

isGvn BinOp {} = True
isGvn UnaryOp {} = True
isGvn TypeCast {} = True
isGvn GetElementPtr {} = True
isGvn _ = False

insertRegTable :: MonadState Gvn m => Operand -> Number -> m ()
insertRegTable o n = modify (over registerTable (M.insert o n))

insertLeaderTable :: MonadState Gvn m => BlockId -> Number -> Operand -> m ()
insertLeaderTable bid n o = modify (over leaderTable (M.adjust (M.insert n o) bid))

insertExpTable :: MonadState Gvn m => GInstruction OperandValue -> Number -> m ()
insertExpTable o n = modify (over exprTable (HM.insert (Expr o) n))

insertPhiTable :: MonadState Gvn m => BlockId -> Int -> Dtype -> [(BlockId, Operand)] -> m ()
insertPhiTable bid aid dtype xs = modify (over phiTable (M.alter f bid))
  where
    f Nothing = Just [(aid, dtype, xs)]
    f (Just as) = Just (as ++ [(aid, dtype, xs)])

getRegisterTable o = gets (M.lookup o . view registerTable)

getExprTable o = gets (HM.lookup (Expr o) . _exprTable)

getLeaderVal :: MonadState Gvn m => BlockId -> Number -> m (Maybe Operand)
getLeaderVal bid n = gets ((M.lookup n <=< M.lookup bid) . _leaderTable)

insertNewReg key = do
  number <- gets _counter
  modify (over counter (+ 1))
  insertRegTable key number
  return number

insertValue :: MonadState Gvn m => BlockId -> Operand -> Operand -> m Number
insertValue bid key value = do
  number <- insertNewReg key
  insertLeaderTable bid number value
  return number

processPhis prevs bid phis = do
  traverse_ go (zip3 [0 ..] phis prevOperands)
  where
    prevOperands :: [[(BlockId, Operand)]]
    prevOperands = transpose $ [(p,) <$> xs | (p, block) <- prevs, (JumpArg bid' xs) <- getJumpArgs (exit block), bid == bid']
    go (index, phi, xs) = do
      xs' <- traverse (\(bid, op) -> fst <$> resolve bid op) xs
      when (allEq xs') $ insertPhiValue (head xs')
      where
        insertPhiValue (C c) = void $ insertValue bid arg (Constant c)
        insertPhiValue (N n) = insertRegTable arg n
        arg = Register (Arg bid index) (item phi)

initLeader bid [] = return ()
initLeader bid [idom] = do
  xs <- gets ((M.! idom) . _leaderTable)
  modify (over leaderTable (M.insert bid xs))

resolveRegister o = do
  v <- getRegisterTable o
  case v of
    Nothing -> insertNewReg o
    (Just x) -> return x

resolve :: MonadState Gvn m => BlockId -> Operand -> m (OperandValue, Operand)
resolve bid o@(Constant c) = return (C c, o)
resolve bid o = do
  n' <- resolveRegister o
  o' <- resolveLeader n'
  if o' == o then return (N n', o') else resolve bid o'
  where
    resolveRegister o = getRegisterTable o >>= maybe (insertNewReg o) return
    resolveLeader number = getLeaderVal bid number >>= maybe (insertLeaderTable bid number o >> return o) return

replace bid prevs expr tmp | isGvn expr = do
  entry <- getExprTable expr
  case entry of
    Nothing -> void (insertValue bid tmp tmp >>= insertExpTable expr)
    (Just n) -> do
      val <- getLeaderVal bid n
      case val of
        Nothing -> tryAddPhiNodes bid n prevs tmp
        (Just x) -> insertRegTable tmp n
replace _ _ _ i = return ()

tryAddPhiNodes bid number prevs tmp = do
  prevOps <- sequence <$> traverse (`getLeaderVal` number) prevs
  case prevOps of
    Nothing -> insertLeaderTable bid number tmp
    (Just []) -> insertLeaderTable bid number tmp
    (Just xs) -> do
      phi <- gets _tempPhi
      modify (\s -> s {_tempPhi = phi + 1})
      let phiArg = Register (Arg bid phi) (getDtype tmp)
      insertRegTable phiArg number
      insertRegTable tmp number
      insertLeaderTable bid number phiArg
      insertPhiTable bid phi (getDtype tmp) (zip prevs xs)

insertPhis blocks bid as =
  let added = M.adjust (\b -> b {phinodes = phinodes b ++ phis}) bid blocks
   in foldr (\(previd, xs) -> M.adjust (\b -> b {exit = applyOp' (f (snd <$> xs)) (exit b)}) previd) added targets
  where
    phis = (\(a, b, c) -> Named Nothing b) <$> as
    targets = groupWith $ sort $ concatMap (\(aid, _, xs) -> (\(bid, operand) -> (bid, (aid, operand))) <$> xs) as
    f op (JumpArg bid' ops) | bid == bid' = JumpArg bid' (ops ++ op)
    f op x = x

gvn f@(FunctionDefinition allocs blocks initbid) =
  let (blocks, s) = runState (traverse go rpo) (emptyGvn (nodes cfg))
      newblocks = M.foldlWithKey insertPhis (M.fromList blocks) (_phiTable s)
   in FunctionDefinition allocs newblocks initbid
  where
    cfg = makeCfg blocks
    rCfg = reverseCfg cfg
    domTree = makeDomTree cfg
    rpo = [(i, blocks M.! i) | i <- reverse $ postOrder cfg]

    go :: (BlockId, Block) -> StateT Gvn Identity (BlockId, Block)
    go b@(bid, Block phi instrs ex) = do
      initLeader bid (idoms domTree bid)
      let prevs = M.toList (M.filterWithKey (\k _ -> k `elem` edges rCfg bid) blocks)
      processPhis prevs bid phi
      instrs' <- processInstr bid (edges rCfg bid) instrs (length phi)
      exit' <- traverse (resolve bid) ex
      return (bid, Block phi instrs' (fmap snd exit'))

    processInstr bid prevs instrs l = modify (\s -> s {_tempPhi = l}) >> traverse f (zip [0 ..] instrs)
      where
        f (index, inst) = do
          i <- traverse (resolve bid) inst
          let (oexpr, inst) = (fmap fst i, fmap snd i)
          let tmp = Register (Temp bid index) (getDtype inst)
          case execute inst of
            Nothing -> replace bid prevs oexpr tmp
            (Just x) -> void $ insertValue bid tmp x
          return inst