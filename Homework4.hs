---------- Exercise 1 -----------

--fun1 :: [Integer] -> Integer
--fun1 []     = 1
--fun1 (x:xs)
--  | even x    = (x - 2) * fun1 xs
--  | otherwise = fun1 xs

fun1' = foldl (*) 1 . map (flip (-) 2) . filter even

--fun2 :: Integer -> Integer
--fun2 1 = 0
--fun2 n 
--  | even n    = n + fun2 (n `div` 2)
--  | otherwise = fun2 (3*n + 1)

fun2' = sum . filter even . takeWhile (/= 1) . iterate fun2''
	where
	fun2'' n
		| even n    = n `div` 2
		| otherwise = (3*n) + 1

---------- Exercise 2 -----------

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertItem Leaf
	where
	height :: Tree a -> Integer
	height Leaf           = 0
	height (Node h _ _ _) = h
	insertItem :: a -> Tree a -> Tree a
	insertItem a Leaf = Node 0 Leaf a Leaf
	insertItem a tree@(Node h l y r)
		| height l < height r = iLeft tree a
		| height l > height r = iRight tree a
		| otherwise           = let newTree = iRight tree a
								in if height newTree == h then newTree else iLeft tree a
		where
		iLeft :: Tree a -> a -> Tree a
		iLeft (Node h l y r) a = let newLeft = insertItem a l
								 in Node (max (height newLeft) (height r) + 1) newLeft y r
		iRight :: Tree a -> a -> Tree a
		iRight (Node h l y r) a = let newRight = insertItem a r
								 in Node (max (height l) (height newRight) + 1) l y newRight

---------- Exercise 3 -----------

xor :: [Bool] -> Bool
xor = foldl (==) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc xs = foldr (flip f) acc (reverse xs)

---------- Exercise 4 -----------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let 
					cartesianProduct = [cP | i <- [1..n], j <- [1..i], cP <- [i + j + (2 * i * j)], cP <= n]
					sieve            = filter (not . flip elem cartesianProduct) [1..n]
				  in
				  	2 : map ((+1) . (*2)) sieve
