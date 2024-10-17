minimum' :: (Ord a) => [a] -> a
minimum' [x] = x
minimum' (x:xs) = if x < y then x else y where y = minimum' xs
