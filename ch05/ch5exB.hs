-- size [[], []] is 2 x 0.
-- size [] is infinite x 0.

-- version processing row by row
transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs : xss) = zipWith (:) xs (transpose xss)

-- version processing column by column
transpose' :: [[a]] -> [[a]]
transpose' ([]:xss) = []
transpose' xss = map head xss : transpose' (map tail xss)
