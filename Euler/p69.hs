import Data.List
import Data.Function

{-
if gcd m n == 1 then totient(m * n) = totient m * totient n
-}

factor :: Integer -> [Integer]
factor 0 = []
factor 1 = []
factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2..n]
					 in (prime :) $ factor $ div n prime

factmults :: Integer -> [(Integer,Integer)]
factmults n = map (\xs -> (head xs, toInteger (length xs))) . group . factor $ n

totient :: Integer -> Integer
totient n = product . map tot . factmults $ n
	where
		tot (p, mult) = p ^ mult - p ^ (mult - 1)

totp :: Integer -> Integer -> Integer
totp p n = p ^ mult - p ^ (mult - 1)

xover :: Integer -> (Integer, Double)
xover n = (n, (fromIntegral n :: Double) / (fromIntegral (totient n) :: Double))

p69 :: Integer
p69 = maximumBy (compare `on` xover) [1..100000]
	where
		xover n = (fromIntegral n :: Double) / (fromIntegral (totient n) :: Double)