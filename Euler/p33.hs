import Data.List

spurious :: (Int, Int) -> Bool
spurious (a, b) = fdigits a b && ldigits a b
	where
		fdigits x y = (mod x 10) == (div y 10)
		ldigits x y = x * (mod y 10) == y * (div x 10)