type Matrix a = [Row a]
type Row a    = [a]

add1 :: Matrix Int -> Matrix Int
add1 = map (map (+1))

addM :: Matrix Int -> Matrix Int -> Matrix Int
addM = zipWith (zipWith (+))

transpose :: [[a]] -> [[a]]
transpose [xs] = [[x] | x <- xs]
transpose (xs : xss) = zipWith (:) xs (transpose xss)

mulM :: Matrix Int -> Matrix Int -> Matrix Int
mulM xss yss = [[sum (zipWith (*) xs ys')] | xs <- xss, ys' <- yss']
               where yss' = transpose yss
