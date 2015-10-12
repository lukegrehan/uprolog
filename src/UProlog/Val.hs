-- | Values used in the unification graph
module UProlog.Val where

-- | Indices used to map var to values
type Id = Integer

-- | The types used to represent given values in the unification graph.
data Val = Var Id       -- ^ Identifiers
         | C Char
         | I Integer
         | B Bool
         | P (Val,Val)  -- ^ Sum types
         | Nil          -- ^ Unit value used to represent the end of lists
         deriving (Eq)


-- | The class of types representing those that can be lifted from standard
-- haskell types
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


instance Show Val where
  show (Var x) = "<" ++ show x ++ ">"
  show (I x) = show x
  show (C c) = show c
  show (B b) = show b
  show (P(a, b)) = "(" ++ show a ++ " " ++ show b ++ ")"
  show Nil = "()"

-- | Util function to convert from lifted lists to standard haskell lists.
-- The effect is left undefined in the case when applied to a value that
-- doesn't represent a list
mkList :: Val    -- ^ A value that can be seen to represent a list
       -> [Val]  -- ^ The given list
mkList (P(a,b)) = (a: mkList b)
mkList _ = []

