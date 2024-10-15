-- No, because (/) is not an operator of the Integral class.
-- Instead, following could be OK.

div' :: Integral a => a -> a -> a
div' x y = floor (fromIntegral x / fromIntegral y)
