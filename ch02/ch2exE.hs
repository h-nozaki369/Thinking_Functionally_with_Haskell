first :: (a -> Bool) -> [a] -> Maybe a
first p xs | null xs   = Nothing
           | p x       = Just x
           | otherwise = first p (tail xs)
           where
               x = head xs
