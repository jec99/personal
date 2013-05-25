module Main where

import Data.List
import qualified Data.Map as Map

digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0

cycles :: [Integer] -> [[Integer]]
cycles xs = map (\n -> (take l . drop n .cycle) xs) [0..(l - 1)]
	where l = length xs

circularPrimesTo :: Integer -> [Integer]
circularPrimesTo n = 2 : filter circularPrime (primesTo n)
	where circularPrime p
					| any even (digits p) = False
					| otherwise = all prime $ map readDigits $ cycles (digits p)

prime :: Integer -> Bool
prime n
	| n < 2 			 = False
	| n == 2 			 = True
	| n == 3			 = True
	| even n 			 = False
	| mod n 3 == 0 = False
	| otherwise 	 = all (/= 0) [mod n k | k <- (merge [5,11..sq] [7,13..sq])]
		where sq 		 = floor (sqrt (fromIntegral n))

primesTo :: Integer -> [Integer]
primesTo n = sieve [2..n]

sieve :: [Integer] -> [Integer]
sieve xs = sieve' xs Map.empty
	where
		sieve' []     table = []
		sieve' (x:xs) table = 
			case Map.lookup x table of
					Nothing 	 -> x : sieve' xs (Map.insert (x*x) [x] table)
					Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
				where
					reinsert table prime = Map.insertWith (++) (x + prime) [prime] table

merge :: [a] -> [a] -> [a]
merge xs ys = concat [xs, ys]

main = do
	print $ length $ circularPrimesTo 1000000