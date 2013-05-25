import Data.List
import Data.Char

-- boilerplate
same :: (Eq a) => [a] -> [a] -> Bool
same x y = ((x \\ y) == []) && ((y \\ x) == [])

digits :: Int -> [Int]
digits n = map digitToInt $ show n

readDigits :: [Int] -> Int
readDigits = foldl (\acc x -> 10 * acc + x) 0
-- /boilerplate

concat' :: [Int] -> Int
concat' xs = readDigits $ concatMap digits xs

panprod :: Int -> Int -> Bool
panprod n k = same (concatMap (\l -> digits (k * l)) [1..n]) [1..9]

pandigitals :: Int -> Int -> [Int]
pandigitals n d = filter (panprod n) [1..10 ^ d]

maxpan :: Int -> Int
maxpan n = maximum (map (\k -> concat' (map (* k) [1..n])) (pandigitals n 5))