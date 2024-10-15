type Interval = (Integer, Integer)

shrink :: Float -> Interval -> Interval
shrink x (m, n) = if (p^2) `leq` x then (p, n) else (m, p)
                  where
                      p = choose (m, n)

choose :: Interval -> Integer
choose (m, n) = (m + n) `div` 2

bound :: Float -> Interval
bound x = (0, upper x)

upper :: Float -> Integer
upper x = until (x `lt`) (* 2) 1

leq :: Integer -> Float -> Bool
leq x y = fromInteger x <= y

lt :: Float -> Integer -> Bool
lt x y = x < fromInteger y

isqrt :: Float -> Integer
isqrt x = if x < 0 then
              error "Error, negative value"
          else
              fst (until unit (shrink x) (bound x))
          where
              unit (m, n) = m + 1 == n

