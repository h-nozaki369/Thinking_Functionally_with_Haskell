takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else x:xs

words' :: String -> [String]
words' xs | null xs' = []
          | otherwise = ys : words' zs
    where
        xs' = dropWhile whiteSpace xs
        (ys, zs) = span (not . whiteSpace) xs'

whiteSpace :: Char -> Bool
whiteSpace c = c == ' ' || c == '\t' || c == '\n'
