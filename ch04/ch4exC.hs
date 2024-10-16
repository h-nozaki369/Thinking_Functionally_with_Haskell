disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint []     _      = True
disjoint _      []     = True
disjoint (x:xs) (y:ys) = case compare x y of
                             EQ -> False
                             LT -> disjoint xs (y:ys)
                             GT -> disjoint (x:xs) ys
