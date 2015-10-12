-- | Core functions relating to unification and predicates
module UProlog.State where

import Prelude hiding (and, or)

import Control.Monad
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

import UProlog.Val

-- | A 'scope' mapping Identifiers to values
type State' = M.Map Id Val

-- | The core type used in unification, representing a function from a given
-- state to a number of new states in a tree like form, which can be
-- traversed in a backtracking, breadth first fashion to find all possible
-- unifying states. We also provide a supply of bindable names in the form
-- of a 'State [Val]' where 'Val' in this case is simply numbered 'Var' 's.
-- It is assumed (but unproven) that these names can't clash across branches
-- of this tree.
type Pred = State' -> LogicT (State [Val]) State'

-- | Inject number of names into a binding function, giving back a new
-- predicate
withN :: Int               -- ^ Number of names to bind
      -> ([Val] -> Pred)   -- ^ Binding function
      -> Pred              -- ^ The resulting predicate
withN n p s = state (splitAt n) >>= flip p s

-- | A special case of 'withN' for @n = 1@
with :: (Val -> Pred) -> Pred
with p = withN 1 (p . head)


-- | Walk the graph represented by the given state to obtain the terminal value
-- for a given identifier. In the case of a pair we walk both braches and return
-- a pair of the result. Any other value is simply returned unchanged ie.
--
-- >>> walk (I 42) s
-- 42
walk :: Val -> State' -> Val
walk (P (x,y)) s = P(walk x s, walk y s)
walk (Var x) s = maybe (Var x) (flip walk s) $ M.lookup x s
walk x _ = x

-- | Mark two values as equal in a state, giving us a new predicate. If we
-- interpret the search space as a graph this is equivalent to adding a new
-- edge constraint between two nodes.
unify :: Val -> Val -> Pred
unify v w m =
  case (walk v m, walk w m) of
    (a,b) | a == b -> return m
    (Var a,b) -> return (M.insert a b m)
    (a,Var b) -> return (M.insert b a m)
    (P(x,y), P(x',y')) -> unify x x' m >>= unify y y'
    _ -> mzero

-- | Infix version of 'unify'
(===) :: Val -> Val -> Pred
(===) = unify


-- | Given two predicates a and b, return a new predicate in which everything
-- specified by a and b is true. In the graph interpretation this is
-- equivalent to adding all the constraints of a to b.
and :: Pred -> Pred -> Pred
p `and` q = \s-> p s >>- q

-- | Given two predicates a and b, return a new predicate in which everything
-- in a is true or everything in b is true. Both a and b can potentially be
-- true. In the graph interpretatin this is equivalent to taking an initial
-- graph i and returning @/(i+a)/@ and @/(i+b)/@ (where @/(i+x)/@ represents
-- i with all the constraints of x) and looking at both conccurrently in
-- future computation.
or :: Pred -> Pred -> Pred
p `or` q = \s-> interleave (p s) (q s)


-- | Return a predicate specifying that all the input predicates must be true
all :: [Pred] -> Pred
all = foldr and return

-- | Return a predicate that holds if any of its input predicates are true
any :: [Pred] -> Pred
any = foldr or (const mzero)

-- | Negation as failure. Given a predicate return a predicate that prunes
-- any input states that match the input predicate.  __NB:__  This cannot be
-- unified across to get all states that don't match. We only ever prune on
-- negation; never add:
--
-- >>> run $ \v -> naf (v === (quote 42))
-- []
naf :: Pred   -- ^ A predicate
    -> Pred   -- ^ Negation of the given predicate
naf p s = do
  lnot (p s)
  return s


-- | Run a given logic program and return a list of formated solutions.
run :: (Val -> Pred)  -- ^ A logic program
    -> [String]       -- ^ List of solutions formated with @show@
run = (map show) . runVals

-- | Run a given logic program and return the list of raw values that result
-- by walking the first value across the final graph.
runVals :: (Val -> Pred) -- ^ A logic program
        -> [Val]         -- ^ List of raw solutions
runVals p = map (walk $ Var 0)
          $ evalState (observeAllT $ with p M.empty)
          $ map Var [0..]
