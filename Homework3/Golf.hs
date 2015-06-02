module Golf where

import Data.Maybe(maybe)
import Data.List(sort, group, replicate, lookup)
import Debug.Trace

---------- Exercise 1 -----------

skips :: [a] -> [[a]]
skips xs = map getSubSequence ns
	where
	ns :: [Int]
	ns = [1 .. length xs]
	getSubSequence n = map snd $ filter (\(x,_) -> (x `rem` n) == 0) $ zip ns xs

---------- Exercise 2 -----------

localMaxima :: [Integer] -> [Integer]
localMaxima = reverse . sort . go
	where
	go :: [Integer] -> [Integer]
	go []        = []
	go (x:y:[])  = []
	go (x:y:z:zs)
		| y > x && y > z = y : go (y:z:zs)
		| otherwise      = go (y:z:zs)

---------- Exercise 3 -----------

histogram :: [Integer] -> String
histogram = unlines . makeLines . (map (\n -> (head n, length n))) . group . sort
	where
	makeLines :: [(Integer, Int)] -> [String]
	makeLines x = go x ++ [(replicate 10 '=')] ++ [(concatMap show [0..9])]
	go :: [(Integer, Int)] -> [String]
	go xs = map (\row -> map placeDot row) $ map (\x -> map (\y -> (y,x)) [0..9]) [9,8..0]
		where
		placeDot :: (Int, Int) -> Char
		placeDot (x,y) = maybe ' ' (\h -> if h > y then '*' else ' ') $ lookup (fromIntegral x) xs
