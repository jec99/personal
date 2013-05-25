import Data.List

readDigits :: [Integer] -> Integer
readDigits = foldl (\acc x -> 10 * acc + x) 0

digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

property :: [Integer] -> Bool
property [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10]
	| odd d4 = False
	| mod (d3 + d4 + d5) 3 /= 0 = False
	| notElem d6 [0,5] = False
	| mod (readDigits [d5,d6,d7]) 7 /= 0 = False
	| mod (readDigits [d6,d7,d8]) 11 /= 0 = False
	| mod (readDigits [d7,d8,d9]) 13 /= 0 = False
	| mod (readDigits [d8,d9,d10]) 17 /= 0 = False
	| otherwise = True

p42 :: Integer
p42 = sum $ map readDigits $ filter property $ permutations [0..9]