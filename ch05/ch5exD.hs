import Data.List

nodups :: (Ord a) => [a] -> Bool
nodups xs = all (\(x,y) -> x /= y) $ zip xs' $ tail xs' where xs' = sort xs
