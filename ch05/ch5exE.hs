import Data.List

nub' :: (Ord a) => [a] -> [a]
nub' xs = nub'' $ sort xs
    where
        nub'' [] = []
        nub'' (x:xs) = x : nub'' (dropWhile (==x) xs)
