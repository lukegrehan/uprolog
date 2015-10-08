module UProlog
( module UProlog.Val
, module UProlog.State
, t, f, tt, nandP, notP
, andP, orP, memberP, appendP
) where


import Prelude hiding (any, all, and, or)
import UProlog.Val
import UProlog.State

t, f :: Val
[t, f] = B <$> [True, False]

tt :: (Val -> Val -> Val -> Pred) -> Val -> Pred
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

nandP :: Val -> Val -> Val -> Pred
nandP a b r = any [
  ((a === f) `or` (b === f)) `and` (r === t),
  all [a === t, b === t, r === f]
  ]

notP :: Val -> Val -> Pred
notP a r = any [
    ((a === t) `and` (r === f)),
    ((a === f) `and` (r === t))
  ]

andP :: Val -> Val -> Val -> Pred
andP a b r = with $ \c -> (nandP a b c) `and` (notP c r)

orP :: Val -> Val -> Val -> Pred
orP a b r = withN 2 $ \[a', b'] -> all [notP a a', notP b b', nandP a' b' r]

memberP :: Val -> Val -> Pred
memberP i l = withN 2 $ \[h,t'] -> all [
    l === quote (h,t'),
    (h === i) `or` (memberP i t')
  ]

appendP :: Val -> Val -> Val -> Pred
appendP a b r = any [
    (a === Nil) `and` (r === b) ,
    withN 3 $ \[h, ta, tr] -> all [
        a === quote (h, ta),
        r === quote (h, tr),
        appendP ta b tr
    ]
  ]


