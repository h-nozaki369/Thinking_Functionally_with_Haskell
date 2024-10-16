type Matrix a = [Row a]
type Row a    = [a]

add1 :: Matrix Int -> Matrix Int
add1 = map (map (+1))

addM :: Matrix Int -> Matrix Int -> Matrix Int
addM xm ym = zipWith (+) <$> xm <*> ym
