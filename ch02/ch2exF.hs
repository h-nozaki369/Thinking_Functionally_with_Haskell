exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | even n = let y = exp' x (n `div` 2)     in y * y
         | odd n  = let y = exp' x ((n-1) `div` 2) in y * y * x 
