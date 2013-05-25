coins :: [Int]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

withcoins :: Int -> Int -> [[Int]]
withcoins 1 x = [[x]]
withcoins n x = concatMap addCoin [0..div x (coins !! (n - 1))]
	where addCoin k = map (++ [k]) (withcoins (n - 1) (x - k * coins !! (n - 1)))