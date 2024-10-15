-- 3 + (-2) == 1
-- subtract 2 3 == 1

subtract' :: Num a => a -> a -> a
subtract' = flip (-)
