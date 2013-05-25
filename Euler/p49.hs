import Data.List

-- better code
p49 :: [(Integer,Integer,Integer)]
p49 = [(a,b,c) | a <- p4,
								 b <- dropWhile (<= a) p4,
								 sort (show a) == sort (show b),
								 let c = 2 * b - a,
								 elem c p4,
								 sort (show a) == sort (show c)]
	where p4 = filter prime [1000..10000]

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3 			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- (concat [[5,11..sq],[7,13..sq]])]
		where sq 		 = floor (sqrt (fromIntegral n))

-- worse code
digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

primesTo m = 2 : sieve [3, 5..m] where
	sieve [] = []
	sieve (p:xs) = p : sieve (xs `minus` [p, p + 2 * p..m])

minus (x:xs) (y:ys) = case (compare x y ) of
	LT -> x : minus xs (y:ys)
	EQ -> 		minus xs ys
	GT -> 		minus (x:xs) ys
minus xs _ = xs

same' :: [Integer] -> [Integer] -> Bool
same' xs ys = xs \\ ys == []

okay :: Integer -> Integer -> Bool
okay start step = prime (start + 2 * step) && and [c1,c2,c3] && d
	where
		c1 = same' (digits start 10) (digits (start + step) 10)
		c2 = same' (digits start 10) (digits (start + 2 * step) 10)
		c3 = same' (digits (start + step) 10) (digits (start + 2 * step) 10)
		d  = start + 2 * step < 10000

okay' :: (Integer,[Integer]) -> (Integer,Integer)
okay' (p,ls)
	| found == [] = (0,0)
	| otherwise   = (p, head found)
	where
		found = (filter (okay p) . map (flip (-) p)) ls

-- processes the whole list
process :: Maybe (Integer,[Integer])
process = find check setup
	where
		l = ((primesTo 10000) \\ (primesTo 1000)) \\ [1487,4817,8147]
		setup = [(x, filter (> x) l) | x <- l]
		check (p,ls)
			| okay' (p,ls) == (0,0) = False
			| otherwise = True
