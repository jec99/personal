import Data.List

isPentagonal :: Integer -> Bool
isPentagonal n = isInt $ (sqrt (24 * n' + 1) + 1) / 6
	where n' = fromIntegral n :: Double

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

pentagonal :: Integer -> Integer
pentagonal n = div (n * (3 * n - 1)) 2

p44 :: Maybe (Integer, Integer)
p44 = find okay [(pentagonal a, pentagonal b) | a <- [1..], b <- [1..a - 1]]
	where
		okay (n, m) = isPentagonal (n - m) && isPentagonal (n + m)