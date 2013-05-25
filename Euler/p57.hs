import Data.Ratio
import Data.List

f :: Rational -> Rational
f x = toRational 1 + 1 / (toRational 1 + x)

p57 :: Int
p57 = (length . filter okay . take 1000) $ iterate f (toRational 1)
	where
		okay r = (length . show . numerator) r > (length . show . denominator) r