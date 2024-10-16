cubics :: Int -> [(Int, Int, Int, Int)]
cubics n = [(x, y, z, w) | x <- [1..n], y <- [x..n], z <- [(x+1)..n], w <- [z..n], x^3+y^3 == z^3+w^3]

main :: IO ()
main = print $ (cubics 100) !! 1
