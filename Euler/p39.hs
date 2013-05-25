import Data.List

primitives :: Integer -> [(Integer,Integer,Integer)]
primitives b = [triple m n | m <- [2..sqrt' b], n <- [1..(m - 1)], okay m n]
	where
		sqrt' x 	 = (toInteger . floor . sqrt) (fromIntegral x :: Double)
		okay x y   = 2 * x * (x + y) <= b
		triple m n = (m ^ 2 - n ^ 2, 2 * m * n, m ^ 2 + n ^ 2)
-- a + b + c == m^2 - n^2 + 2mn + m^2 + n^2 == 2(m^2 + mn) = 2m(m + n)

-- nubBy same $ triples b

same :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
same (a, b, _) (x, y, _)
	| (a >= b) == (x >= y) = (div a gab, div b gab) == (div x gxy, div y gxy)
	| otherwise						 = (div b gab, div a gab) == (div x gxy, div y gxy)
	where
		gab = gcd a b
		gxy = gcd x y

same' :: (Integer,Integer,Integer) -> (Integer,Integer,Integer) -> Bool
same' (a, b, _) (x, y, _)
	| (a >= b) == (x >= y) = (a == x && b == y)
	| otherwise 					 = (a == y && b == x)

lst :: (Integer,Integer,Integer) -> Integer
lst (_, _, x) = x

triples :: Integer -> [(Integer,Integer,Integer)]
triples b = concatMap multiples $ primitives b
	where
		okay (x, y, z) 			= x + y + z <= b
		multiples (x, y, z) = takeWhile okay [(k * x, k * y, k * z) | k <- [1..]]

-- nubBy same' $ filter  $ triples 1000

solutions :: Integer -> [Int]
solutions bound = map (\k -> length (nubBy same' (filter (flip okay k) ls))) [1..bound]
	where
		okay (a, b, c) k = a + b + c == k
		ls = triples bound