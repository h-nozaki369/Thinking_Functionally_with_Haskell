fracExp :: (Fractional a, Integral b) => a -> b -> a
fracExp x y | y < 0     = 1.0 / x ^ (-y)
            | otherwise = x ^ y
