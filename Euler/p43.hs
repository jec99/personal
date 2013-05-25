{-
pentagonal n = div (n * (3 * n - 1)) 2

hexagonal n = n * (2 * n - 1)
-}

import Data.List

triangle :: Integer -> Integer
triangle n = div (n * (n + 1)) 2

isPentagonal :: Integer -> Bool
isPentagonal n = isInt $ (sqrt (24 * n' + 1) + 1) / 6
	where n' = fromIntegral n :: Double

isHexagonal :: Integer -> Bool
isHexagonal n = isInt $ (sqrt (8 * n' + 1) + 1) / 4
	where n' = fromIntegral n :: Double

isInt :: (RealFrac a) => a -> Bool
isInt x = x == fromInteger (round x)

p43 :: Maybe Integer
p43 = find okay [triangle k | k <- [286..]]
	where okay x = isHexagonal x && isPentagonal x