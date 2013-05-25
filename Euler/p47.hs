import Data.List

f :: Int -> [Int]
f 1 = []
f n = let prime = head $ dropWhile ((/= 0) . mod n) [2..n]
					 in (prime :) $ f $ div n prime

fpair :: Int -> [(Int,Int)]
fpair n = map (\xs -> (head xs, length xs)) . group . f $ n

c4pf :: Int -> Bool
c4pf s = c f1 && c f2 && c f3 && c f4 && d [f1,f2,f3,f4]
	where
		f1 	= fpair s
		f2 	= fpair (s + 1)
		f3 	= fpair (s + 2)
		f4 	= fpair (s + 3)
		c l = length l == 4
		d l	= (length . nub . concat) l == 16

p47 :: Maybe Int
p47 = find c4pf [1..]