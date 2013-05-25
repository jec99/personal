import Data.List
import Data.Char
import Data.Function

digitsum :: Integer -> Int
digitsum n = foldl (\acc x -> acc + digitToInt x) 0 $ show n

p56 :: Int
p56 = maximum . map digitsum $ [a ^ b | a <- [1..100], b <- [1..100]]