import Data.Char

digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

sumfifthpowers :: Integer -> Bool
sumfifthpowers n = (n == sum [k ^ 5 | k <- digits n])

p30 :: Integer
p30 = sum $ filter sumfifthpowers [10..999999]