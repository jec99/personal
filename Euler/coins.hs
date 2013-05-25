{-
combinations :: Int -> Int
combinations n
	| n < 0 		= 0
	| n == 0 		= 0
	| n == 1 		= 1
	| otherwise = length (1 : map combinations subtractions)
		where subtractions = [n - k | k <- [1, 2, 5, 10, 20, 50, 100, 200], n - k >= 0]
-}

combinations :: Int -> [Int] -> [[Int]]
combinations n xs
	| n == 0 		= [xs]
	| otherwise = map (\k -> combinations (n - k) (k : xs)) subtractions
		where subtractions = filter (<= n) [1,2,5,10,20,50,100,200]
