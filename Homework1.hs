---------- Exercise 1 -----------

toDigits :: Integer -> [Integer]
toDigits n
	| n <= 0    = []
	| otherwise = (toDigits $ n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

---------- Exercise 2 -----------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x * 2]
doubleEveryOther (x:y:ys) = x * 2 : y : doubleEveryOther ys

---------- Exercise 3 -----------

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

---------- Exercise 4 -----------

validate :: Integer -> Bool
validate = (0 ==) . flip mod 10 . sumDigits . doubleEveryOther . toDigits

---------- Exercise 5 -----------

type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

---------- Exercise 6 -----------

-- hanoi for n disks and m pegs
hanoiN :: Integer -> [Peg] -> [Move]
hanoiN 0 _          = []
hanoiN 1 (x:y:ys)   = [(x, y)]
hanoiN n (x:y:z:zs) =
	hanoiN k (x : z : y : zs) ++
	hanoiN (n - k) (x : y : zs) ++
	hanoiN k (z : y : x : zs)
    where 
	k :: Integer
	k = if null zs then n - 1 else n `quot` 2

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 n a b c d = hanoiN n [a,b,c,d]
