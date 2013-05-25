digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

p40 :: Integer
p40 = product [l !! (k - 1) | k <- xs]
	where
		l  = concatMap (flip digits 10) [1..200000]
		xs = [1,10,100,1000,10000,100000,1000000]