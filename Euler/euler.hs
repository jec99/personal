import Data.List
import Data.Char (digitToInt)

same :: Integer -> Integer -> Bool
same x y = (show x) \\ (show y) == []

smallestSameDigits :: Integer -> Integer
smallestSameDigits n = head $ dropWhile hassame [1..]
	where hassame k = all (same n) (map (* n) [2..6])

digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0

lychrel :: Integer -> Integer -> Bool
lychrel x n
	| n == 0		 = lychrel (x + backwards x) (n + 1)
	| n == 50 		 = True
	| palindrome x = False
	| otherwise		 = lychrel (x + backwards x) (n + 1)
		where
			backwards n = readDigits $ reverse $ digits n 10
			palindrome p = p == backwards p