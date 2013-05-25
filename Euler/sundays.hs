import Data.Bits
import Data.Char
import Data.List

-- jan 1 1901 was a tuesday
-- let years = [1901..2000]

merge :: [a] -> [a] -> [a]
merge xs ys = concat [xs, ys]

isleap :: Int -> Bool
isleap n
	| mod n 400 == 0 = True
	| mod n 100 == 0 = False
	| mod n 4 == 0 = True
	| otherwise = False

daysinyear :: Int -> Int
daysinyear year = if isleap year then 366 else 365

--months :: Int -> Int -> [(Int, Int)]
months year = zip (concat (map (\n -> [1..n]) months)) days where
	months	 = if isleap year
							 then [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
							 else [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	startday = mod (sum (map daysinyear [1901..(year - 1)])) 7 + 1
	days		 = drop startday $ concat $ repeat [1..7]

sundays year = length $ filter (== (1, 7)) (months year)