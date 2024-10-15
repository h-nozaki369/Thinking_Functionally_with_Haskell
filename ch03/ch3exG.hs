data Nat = Zero | Succ Nat

instance Eq Nat where
    Zero   == Zero   = True
    Zero   == Succ n = False
    Succ m == Zero   = False
    Succ m == Succ n = m == n

instance Show Nat where
    show Zero            = "Zero"
    show (Succ Zero)     = "Succ Zero"
    show (Succ (Succ n)) = "Succ (" ++ show (Succ n) ++ ")"

instance Ord Nat where
    Zero   <= Zero   = True
    Zero   <= Succ n = True
    Succ m <= Zero   = False
    Succ m <= Succ n = m <= n

instance Num Nat where
    m + Zero = m
    m + (Succ n) = Succ (m + n)

    m * Zero = Zero
    m * (Succ n) = m * n + m

    abs n = n
    signum Zero = Zero
    signum (Succ n) = Succ Zero

    m - Zero = m
    Zero - Succ n = Zero
    Succ m - Succ n = m - n

    fromInteger x
        | x <= 0 = Zero
        | otherwise = Succ (fromInteger (x - 1))

divMod' :: Nat -> Nat -> (Nat, Nat)
divMod' _ Zero  = error "Error, dvide by zero"
divMod' x y
    | x >= y    = let (q, r) = divMod' (x - y) y
                  in (Succ q, r)
    | otherwise = (Zero, x)
