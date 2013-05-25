import Data.Char (ord)
import Data.List (sort)

main = do
	contents <- getContents
	print $ scores $ sort $ lines contents

score :: (Integer, [Char]) -> Integer
score (x, xs) = x * (sum $ map toInteger (map ((flip (-) 64) . ord) xs))

scores :: [[Char]] -> Integer
scores xs = sum $ map score $ zip (map toInteger [1..(length xs)]) xs