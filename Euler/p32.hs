import Data.List
import Data.Char
import Control.Monad

-- boilerplate
same :: (Eq a) => [a] -> [a] -> Bool
same x y = ((x \\ y) == []) && ((y \\ x) == [])

digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0
-- /boilerplate

concat' :: [Integer] -> Integer
concat' xs = readDigits $ concatMap (flip digits 10) xs

pandigital :: Integer -> Bool
pandigital x = same (digits x 10) [1..9]

panprod :: Integer -> Integer -> Bool
panprod x y = pandigital $ concat' [x, y, x * y]

pandigitals :: Integer
pandigitals = sum $ nub (concat [
	[x * y | x <- [1..10], y <- [1000..10000], panprod x y],
	[x * y | x <- [10..100], y <- [100..1000], panprod x y] ])