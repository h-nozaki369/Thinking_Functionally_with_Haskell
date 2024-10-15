first :: (a -> Bool) -> [a] -> [a]
first p xs | null xs   = error "Empty list"
           | p x       = x
	   | otherwise = first p (tail xs)
	   where
	       x = head xs

firstMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
firstMap p f xs | null xs   = error "Empty list"
                | p y       = y
	        | otherwise = firstMap p f (tail xs)
	        where
	            y = f $ head xs
