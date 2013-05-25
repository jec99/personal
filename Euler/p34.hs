factorial :: Integer -> Integer
factorial n = product [1..n]

digits :: Integer -> Integer -> [Integer]
digits 0 _ = []
digits x b = digits (div x b) b ++ [mod x b]

factdigits :: Integer -> Bool
factdigits n = n == (sum . map factorial $ digits n 10)