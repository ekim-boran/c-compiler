module Opt.Util where

import Control.Lens
import Data.Bifunctor
import Data.Bits
import Data.List as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Ir.Types
import Ir.Utils
import Language.C.Syntax.Ops

getLocal :: Operand -> Maybe LocalId
getLocal (Register (Local i) d) = Just i
getLocal _ = Nothing

boolToInt op a b = if op a b then 1 else 0

executeOp CMulOp = (*)
executeOp CAddOp = (+)
executeOp CAndOp = (.&.)
executeOp CSubOp = (-)
executeOp CXorOp = xor
executeOp COrOp = (.|.)
executeOp CNeqOp = \a b -> if a /= b then 1 else 0
executeOp CEqOp = \a b -> if a == b then 1 else 0
executeOp CLeOp = boolToInt (<)
executeOp CLeqOp = boolToInt (<=)
executeOp CGrOp = boolToInt (>)
executeOp CGeqOp = boolToInt (>=)
executeOp CDivOp = div
executeOp CShlOp = shiftL
executeOp CShrOp = shiftR
executeOp CRmdOp = mod
executeOp x = error (show x)


executeOpFloat CMulOp = Just (*)
executeOpFloat CAddOp = Just(+)
executeOpFloat CSubOp = Just(-)
executeOpFloat CDivOp = Just (/)
executeOpFloat x = Nothing

