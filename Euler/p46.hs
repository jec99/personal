import Data.List

isSquare :: Integer -> Bool
isSquare n = isInt $ sqrt n'
	where n' = fromIntegral n :: Double

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

primesTo :: Integer -> [Integer]
primesTo n = 2 : sieve [3,5..n]
	where
		sieve (p:xs)
			| p * p > n = p : xs
			| True 			= p : sieve [x | x <- xs, rem x p /= 0]

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- [2..sq]]
	| otherwise 	 = all (/= 0) [mod n k | k <- (concat [[5,11..sq],[7,13..sq]])]
		where sq 		 = floor (sqrt (fromIntegral n))

p46 :: Integer -> Maybe Integer
p46 n = find nsc $ filter npr [9,11..n]
	where
		nsc l = not (success l)
		npr p = not (prime p)

success :: Integer -> Bool
success n = any prime [n - 2 * sq | sq <- map (^ 2) [1..sqrt' n]]
	where
		sqrt' l = (toInteger . floor . sqrt) (fromIntegral n :: Double)