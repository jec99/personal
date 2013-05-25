{-
there can be no 3-, 5-, 6-, 8-, 9-pandigital prime: sum 1..n
	are respectively 6, 15, 21, 36, 45, all divisibly by 3
there can be no 2-, 1-pandigital prime: 12, 21, 1 are not prime

so the only options are 4- and 7-pandigital primes

permutations is a fast function
-}

import Data.List

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- (merge [5,11..sq] [7,13..sq])]
		where
			sq = floor (sqrt (fromIntegral n))
			merge xs ys = concat [xs, ys]

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0

p41 :: Integer
p41 = max (maximum $ f 4) (maximum $ f 7)
	where
		f x = filter prime . map readDigits $ permutations [1..x]