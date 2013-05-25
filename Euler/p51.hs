import Data.List
import Data.Function

prime :: Int -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- (concat [[5,11..sq],[7,13..sq]])]
		where sq 		 = floor (sqrt (fromIntegral n))

primesTo :: Int -> [Int]
primesTo n = 2 : sieve [3,5..n]
	where
  	sieve (p:xs)
			| p * p > n = p : xs
			| True			= p : sieve [x | x <- xs, rem x p /= 0]

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

-- replace the ind digits of n with k
-- note that we must have repeated digits to replace
replace :: Int -> [Int] -> Int -> Int
replace n inds k = fst $ foldl (\(acc,n) x -> if elem n inds
															  then (10 * acc + k, n + 1)
															  else (10 * acc + x, n + 1)) (0,0) $ digits n

check :: Int -> Int -> Bool
check k n = length m >= k
	where
		m = maximumBy (compare `on` length) $
				map (\xs -> (filter prime . map (replace n xs)) [0..9]) $
				(tail . subsequences) [0..(length . show) n - 1]

p51 :: Maybe Int
p51 = find (check 8) $ ((primesTo 1000000) \\ (primesTo 100))