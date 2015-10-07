module UProlog where

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

type Id = Integer

data Val = Var Id
         | C Char
         | I Integer
         | B Bool
         | P (Val,Val)
         | Nil
         deriving (Eq)

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

and2 :: Pred -> Pred -> Pred
p `and2` q = \s-> p s >>- q

or2 :: Pred -> Pred -> Pred
p `or2` q = \s-> interleave (p s) (q s)


and' :: [Pred] -> Pred
and' = foldr and2 return

or' :: [Pred] -> Pred
or' = foldr or2 (const mzero)

run :: (Val -> Pred)  -- ^ A logic program
    -> [String]       -- ^ List of solutions formeted with @show@
run = (map show) . runVals

runVals :: (Val -> Pred) -- ^ A logic program
        -> [Val]         -- ^ List of raw solutions
runVals p = map (walk $ Var 0) $ evalState (observeAllT $ with p M.empty) $ map Var [0..]

class LogicVal a where
  quote :: a -> Val

instance LogicVal Char where
  quote = C

instance LogicVal Integer where
  quote = I

instance LogicVal Bool where
  quote = B

instance (LogicVal a, LogicVal b) => LogicVal (a,b) where
  quote (a,b) = P (quote a, quote b)

instance LogicVal Val where
  quote = id

instance (LogicVal a) => LogicVal [a] where
  quote [] = Nil
  quote (x:xs) = quote (x, quote xs)

mkList :: Val -> [Val]
mkList (P(a,b)) = (a: mkList b)
mkList _ = []

instance Show Val where
  show (Var x) = "<" ++ show x ++ ">"
  show (I x) = show x
  show (C c) = show c
  show (B b) = show b
  show (P(a, b)) = "(" ++ show a ++ " " ++ show b ++ ")"
  show Nil = "()"

nandP :: Val -> Val -> Val -> Pred
nandP a b r = or' [
  and' [
    or' [
      a === B False,
      b === B False
      ],
    r === B True
    ],
  and' [
    a === B True,
    b === B True,
    r === B False
    ]
  ]

notP :: Val -> Val -> Pred
notP a r = or' [
  and' [ (a === B True), (r === B False) ],
  and' [ (a === B False), (r === B True) ]
  ]

andP :: Val -> Val -> Val -> Pred
andP a b r = with $ \c -> and' [nandP a b c, notP c r]

orP :: Val -> Val -> Val -> Pred
orP a b r = withN 2 $ \[a', b'] -> and' [notP a a', notP b b', nandP a' b' r]

memberP :: Val -> Val -> Pred
memberP i l = withN 2 $ \[h,t'] -> and' [
  l === quote (h,t'),
  or' [
      h === i,
      memberP i t'
    ]
  ]

notP' :: Pred -> Pred
notP' p = \s -> do
  lnot (p s)
  return s

append :: Val -> Val -> Val -> Pred
append a b r = or'[
  and' [
    a === Nil,
    r === b
  ],
  withN 3 $ \[h, ta, tr] ->
    and' [
      a === quote (h, ta),
      r === quote (h, tr),
      append ta b tr
    ]
  ]

t, f :: Val
[t, f] = B <$> [True, False]

tt :: (Val -> Val -> Val -> Pred) -> Val -> Pred
tt p r = withN 4 $ \[r0, r1, r2, r3] -> and' [
    r === quote [r0, r1, r2, r3],
    p t t r0,
    p t f r1,
    p f t r2,
    p f f r3
  ]

