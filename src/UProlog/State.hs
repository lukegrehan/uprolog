module UProlog.State where

import Prelude hiding (and, or)

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

import UProlog.Val

type State' = M.Map Id Val
type Pred = State' -> LogicT (State [Val]) State'


with :: (Val -> Pred) -> Pred
with p = withN 1 (p . head)

withN :: Int -> ([Val] -> Pred) -> Pred
withN n p s = state (splitAt n) >>= flip p s


walk :: Val -> State' -> Val
walk (P (x,y)) s = P(walk x s, walk y s)
walk (Var x) s = maybe (Var x) (flip walk s) $ M.lookup x s
walk x _ = x

unify :: Val -> Val -> Pred
unify v w m =
  case (walk v m, walk w m) of
    (a,b) | a == b -> return m
    (Var a,b) -> return (M.insert a b m)
    (a,Var b) -> return (M.insert b a m)
    (P(x,y), P(x',y')) -> unify x x' m >>= unify y y'
    _ -> mzero

(===) :: Val -> Val -> Pred
(===) = unify


and :: Pred -> Pred -> Pred
p `and` q = \s-> p s >>- q

or :: Pred -> Pred -> Pred
p `or` q = \s-> interleave (p s) (q s)


all :: [Pred] -> Pred
all = foldr and return

any :: [Pred] -> Pred
any = foldr or (const mzero)

naf :: Pred -> Pred
naf p s = do
  lnot (p s)
  return s


run :: (Val -> Pred)  -- ^ A logic program
    -> [String]       -- ^ List of solutions formated with @show@
run = (map show) . runVals

runVals :: (Val -> Pred) -- ^ A logic program
        -> [Val]         -- ^ List of raw solutions
runVals p = map (walk $ Var 0) $ evalState (observeAllT $ with p M.empty) $ map Var [0..]
