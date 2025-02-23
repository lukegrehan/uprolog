-- | uprolog
module UProlog
( module UProlog.Val
, module UProlog.State
, module UProlog.Util
) where


import Prelude hiding (any, all, and, or)
import UProlog.Val
import UProlog.State
import UProlog.Util

appendP' :: (LogicVal a, LogicVal b, LogicVal c) => a -> b -> c -> Pred
appendP' a b r = appendP (quote a) (quote b) (quote r)

test = runVals $ \v -> withN 3 $ \[a,b,c] -> (P (a,P (b,c)) === v) `and` with (\t -> (appendP' a b t) `and` (appendP' t c [1::Integer,2,3,4,5,6,7,8,9]))

pprint :: Val -> String
pprint (P (a,P (b,c))) = show a ++ "\t\t" ++ show b ++ "\t\t" ++ show c
pprint _ = "<something else>"
