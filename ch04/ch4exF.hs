data List a = Nil | Snoc (List a) a deriving Show

head' :: List a -> a
head' Nil = error "Empty List"
head' (Snoc Nil x) = x
head' (Snoc xs x)  = head' xs

last' :: List a -> a
last' Nil = error "Empty List"
last' (Snoc xs x) = x

toList :: [a] -> List a
toList xs = toList' xs Nil
            where
                toList' [] ys = ys
                toList' (x:xs) ys = toList' xs (Snoc ys x) 

fromList :: List a -> [a]
fromList xs = fromList' xs []
              where
                  fromList' Nil ys = ys
                  fromList' (Snoc xs x) ys = fromList' xs (x : ys)

main :: IO ()
main = do
    print $ toList [1..4]
    print $ fromList (Snoc (Snoc (Snoc Nil 1) 2) 3)
