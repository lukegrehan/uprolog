-- | Util functions
module UProlog.Util
( t, f, tt, nandP, notP
, andP, orP, memberP, appendP
) where


import Prelude hiding (any, all, and, or)
import UProlog.Val
import UProlog.State

-- | Util values representing lifted bools
t, f :: Val
[t, f] = B <$> [True, False]

-- | Generate truthtables for a given binary boolean operator
tt :: (Val -> Val -> Val -> Pred) -- ^ binary bool operator
   -> (Val -> Pred)               -- ^ logic program generating a truthtable
tt p r = withN 4 $ \[r0, r1, r2, r3] -> all [
    p t t r0,
    p t f r1,
    p f t r2,
    p f f r3,
    r === quote [
      ((t,t),r0),
      ((t,f),r1),
      ((f,t),r2),
      ((f,f),r3)
    ]
  ]

-- | Predicate represnting a lifted nand gate.
nandP :: Val   -- ^ A
      -> Val   -- ^ B
      -> Val   -- ^ A `xor` B
      -> Pred
nandP a b r = any [
  ((a === f) `or` (b === f)) `and` (r === t),
  all [a === t, b === t, r === f]
  ]

-- | Predicate representing a lifted not gate.
notP :: Val   -- ^ A
     -> Val   -- ^ ¬A
     -> Pred
notP a r = any [
    ((a === t) `and` (r === f)),
    ((a === f) `and` (r === t))
  ]

-- | Predicate representing a lifted and gate.
andP :: Val   -- ^ A
     -> Val   -- ^ B
     -> Val   -- ^ A && b
     -> Pred
andP a b r = with $ \c -> (nandP a b c) `and` (notP c r)

-- | Predicate representing a lifted or gate
orP :: Val    -- ^ A
    -> Val    -- ^ B
    -> Val    -- ^ A || B
    -> Pred
orP a b r = withN 2 $ \[a', b'] -> all [notP a a', notP b b', nandP a' b' r]

-- | Predicate representing membership of a given list
memberP :: Val -- ^ item
        -> Val -- ^ list
        -> Pred
memberP i l = withN 2 $ \[h,t'] -> all [
    l === quote (h,t'),
    (h === i) `or` (memberP i t')
  ]

-- | Predicate representing appending two lists together
appendP :: Val  -- ^ L
        -> Val  -- ^ R
        -> Val  -- ^ L ++ R
        -> Pred
appendP a b r = any [
    (a === Nil) `and` (r === b) ,
    withN 3 $ \[h, ta, tr] -> all [
        a === quote (h, ta),
        r === quote (h, tr),
        appendP ta b tr
    ]
  ]


