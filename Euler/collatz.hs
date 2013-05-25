module Main where

import Data.Tuple
import Data.List (sortBy)
import Data.Function (on)

collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz res where res = if even n then div n 2 else 3 * n + 1

main :: IO()
main = do
	let seqx = map (\x -> (x, length $ collatz x)) [999999, 999997..1]
	print . fst . head $ sortBy (flip compare `on` snd) seqx