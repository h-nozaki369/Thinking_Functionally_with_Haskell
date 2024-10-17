> cp :: [[a]] -> [[a]]

Definition of cp in chapter 5.
cp [] = [[]]
cp (xs : xss) = [x : ys | x <- xs, ys <- yss] where yss = cp xss

> cp = foldr (\xs yss -> [x : ys | x <- xs, ys <- yss]) [[]]

  length . cp
= length . foldr (\xs yss -> [x : ys | x <- xs, ys <- yss]) [[]]

length ((\xs yss -> [x : ys | x <- xs, ys <- yss]) x y)
= h x (length y)

  map length
= foldr ((:) . length) []

  product . map length
= product . foldr ((:) . length) []
= foldr h b
= foldr h 1

  product (((:) . length) xs yss)
= h xs (((:) . length) yss)
