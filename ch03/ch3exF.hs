sqrt' :: Float -> Float
sqrt' x = until
              (\y -> abs (y * y - x) < epsilon * x)
              (\y -> (y + x / y) / 2)
              x

epsilon :: Float
epsilon = 0.000001
