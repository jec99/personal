import Data.List
import Data.Char (ord)
import Data.Function

-- sieve of eratosthenes
primesTo m = 2 : sieve [3, 5..m] where
	sieve [] = []
	sieve (p:xs) = p : sieve (xs `minus` [p, p + 2 * p..m])

-- the functions minus and union
minus (x:xs) (y:ys) = case (compare x y ) of
	LT -> x : minus xs (y:ys)
	EQ -> 		minus xs ys
	GT -> 		minus (x:xs) ys
minus xs _ = xs

-- prime factors
factor :: Integer -> [Integer]
factor 0 = []
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2..n]
					 in (prime :) $ factor $ div n prime

-- palindromes
merge :: [a] -> [a] -> [a]
merge xs ys = concat [xs, ys]

palindromes :: Integer -> [Integer]
palindromes n = filter palindrome products where
	products = reverse $ nub [x * y | x <- [1..(10 ^ n)], y <- [1..(10 ^ n)]]
	palindrome p = (digits p 10) == (reverse $ digits p 10)

largestPalindrome :: Integer -> [Integer]
largestPalindrome n = take 1 $ filter palindrome [x * y | x <- [(10 ^ n), (10 ^ n - 1)..1], y <- [(10 ^ n), (10 ^ n - 1)..1]] where
	palindrome p = (digits p 10) == (reverse $ digits p 10)

-- sum of squares vs square of sums
sumsquares :: Integer -> Integer
sumsquares n = (sum [1..n]) ^ 2 - (sum $ map (^ 2) [1..n])

-- triangle numbers
triangle :: Integer -> Integer
triangle k = head $ dropWhile ((<= k) . genericLength . factors) l where
	l = [div (n * n + n) 2 | n <- [1..]]

factors :: Integer -> [Integer]
factors n = filter (< n) $ map product $ nub $ subsequences $ factor n

-- amicable pairs
amicable :: Integer -> Bool
amicable n
	| sum (factors n) == n = False
	| sum (factors (sum (factors n))) == n = True
	| otherwise = False

score :: (Int, [Char]) -> Int
score (x, xs) = x * (sum $ map ((flip (-) 64) . ord) xs)

scores :: [[Char]] -> Int
scores xs = sum $ map score $ zip [1..(length xs)] xs

-- choose n k
factorial :: Integer -> Integer
factorial n = product [1..n]

choose :: Integer -> Integer -> Integer
choose n k
	| k > n = 0
	| otherwise = div (factorial n) (factorial k * factorial (n - k))

chooses :: Integer -> Integer -> Integer
chooses n bound =
	let l = fromIntegral $ length $ dropWhile (<= bound) $ map (choose n) [1..(div (n + 1) 2)]
	in if l == 0 then 0 else if even n then 2 * l - 1 else 2 * l - 2

-- abundant numbers
abundant :: Integer -> Bool
abundant n = sum (factors n) > n	

sumofabundants :: Int -> [Int] -> Bool
sumofabundants n xs = not (any (flip elem (map ((-) n) a)) a) where
	a = filter (< n) xs

-- quadratic formula generating primes
quadprimes :: (Int, Int) -> Int
quadprimes (a, b) = length (takeWhile prime' [n * n + a * n + b | n <- [0..]])

prime' :: Int -> Bool
prime' n
	| n < 2 = False
	| even n = False
	| otherwise = all (/= 0) [mod n k | k <- [2..(div n 2)]]

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- [2..sq]]
	| otherwise 	 = all (/= 0) [mod n k | k <- (merge [5,11..sq] [7,13..sq])]
		where sq 		 = floor (sqrt (fromIntegral n))

largestquadprime :: Int -> (Int, Int)
largestquadprime bound = maximumBy (compare `on` quadprimes) pairs where
	pairs = [(a, b) | b <- [2..bound], a <- [(-b)..bound]]

spiraldiagonals :: Integer -> Integer
spiraldiagonals k = sum ul + sum ur + sum bl + sum br where
	ul = [n ^ 2 | n <- [3,5..k]]
	ur = [n ^ 2 - n + 1 | n <- [3,5..k]]
	bl = [n ^ 2 - 2 * n + 2 | n <- [3,5..k]]
	br = [n ^ 2 - 3 * n + 3 | n <- [3,5..k]]

-- palindromic in bases 10 and 2
palindromic :: Integer -> Integer -> Bool
palindromic n b = digits n b == reverse (digits n b)

digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0

inits' :: [a] -> [[a]]
inits' = tail . inits

tails' :: [a] -> [[a]]
tails' = init . tails

even' :: Integer -> Bool
even' n = even n && n /= 2

truncatable :: Integer -> Bool
truncatable p
	| any even' dgs 	= False
	| length dgs == 1 = False
	| otherwise 	 		= all prime (map readDigits tls) && all prime (map readDigits nts)
		where
			dgs = digits p 10
			tls = tails' dgs
			nts = inits' dgs

spiralprimes :: Integer -> Float
spiralprimes n = l / total
	where
		ur 		= filter prime [(2 * k + 1) ^ 2 - (2 * k + 1) + 1 | k <- [1..n]]
		bl 		= filter prime [(2 * k + 1) ^ 2 - 2 * (2 * k + 1) + 2 | k <- [1..n]]
		br 		= filter prime [(2 * k + 1) ^ 2 - 3 * (2 * k + 1) + 3 | k <- [1..n]]
		l 		= fromIntegral (length ur + length bl + length br) :: Float
		total = fromIntegral (4 * n + 1) :: Float
-- note ul contains no primes
