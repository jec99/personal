import Data.List

p47 :: Maybe Int
p47 = find (all ((== 4) . snd)) . map (take 4) . tails
					. zip [1..] . map (length . factors) $ [1..]

