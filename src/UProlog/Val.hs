module UProlog.Val where

type Id = Integer

data Val = Var Id
         | C Char
         | I Integer
         | B Bool
         | P (Val,Val)
         | Nil
         deriving (Eq)


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

mkList :: Val -> [Val]
mkList (P(a,b)) = (a: mkList b)
mkList _ = []

