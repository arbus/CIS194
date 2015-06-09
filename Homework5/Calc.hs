{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import           ExprT
import           Parser
import qualified StackVM as S

---------- Exercise 1 -----------

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

---------- Exercise 2 -----------

evalStr :: String -> Maybe Integer
evalStr = (fmap eval) . (parseExp Lit Add Mul)

---------- Exercise 3 -----------

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul :: a -> a -> a

instance Expr ExprT where
	lit n = Lit n
	add x y = Add x y
	mul x y = Mul x y

---------- Exercise 4 -----------

instance Expr Integer where
	lit n = n
	add x y = x + y
	mul x y = x * y

instance Expr Bool where
	lit n = n > 0
	add x y = x || y
	mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
	lit n = MinMax n
	add (MinMax x) (MinMax y) = MinMax $ max x y
	mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
	lit = Mod7 . (mod 7)
	add (Mod7 x) (Mod7 y) = Mod7 . (mod 7) $ x + y
	mul (Mod7 x) (Mod7 y) = Mod7 . (mod 7) $ x * y

---------- Exercise 5 -----------

instance Expr S.Program where
	lit = return . S.PushI
	add x y = x ++ y ++ [S.Add]
	mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
