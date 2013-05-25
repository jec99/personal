isInt x = x == fromInteger (round x)

isPower n k = (round ((fn) ** (1 / fk)) ^ k) == n
	where
		fk = fromIntegral k :: Double
		fn = fromIntegral n :: Double

p63 = length . filter okay $ [20000000..30000000]
	where
		okay x = isPower x $ length . show $ x