import Data.List
import Data.Function

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- (concat [[5,11..sq],[7,13..sq]])]
		where sq 		 = floor (sqrt (fromIntegral n))

primesTo :: Integer -> [Integer]
primesTo n = 2 : sieve [3,5..n]
	where
  	sieve (p:xs)
			| p * p > n = p : xs
			| True			= p : sieve [x | x <- xs, rem x p /= 0]

-- fold over
longest :: [Integer] -> Integer -> Integer -> (Integer,Integer)
longest ls limit x = head $ dropWhile (not . prime . snd) l
	where l = dropWhile ((> limit) . snd) $
						(reverse . zip [1..]) $
						(tail . scanl (+) 0 . dropWhile (< x))
						ls

-- better code
p n = takeWhileSum n primes
takeWhileSum n = takeWhileArr (\x -> sum x <= n)
takeWhileArr f xs = takeWhileF f [] xs
	where
		takeWhileF f rs [] = reverse rs
		takeWhileF f rs (x:xs)
			| f (x:rs) = takeWhileF f (x:rs) xs
			| otherwise = reverse rs

primeSums n = map (map (\x -> (prime x, x)) . takeWhile (< n) . scanl1 (+))
							tails (primesTo n))

main = print . maximum $ map index (primeSums 1000000)
	where
		index x = if null $ ind x
								then (0,0)
								else (last $ ind x, snd (x !! (last $ ind x)))
		ind = findIndices (fst)