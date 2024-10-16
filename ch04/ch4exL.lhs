  cross (f, g) . cross (h, k)
=    { definition of cross }
  cross (f, g) . fork (h . fst, k . snd)
=    { cross-fork raw }
  fork (f . (h . fst), g . (k . snd))
=    { assosiative raw of . }
  fork ((f . h) . fst, (g . k) . snd)
=    { definition of cross }
  cross (f . h, g . k)

  cross (id, id)
=    { definition of cross }
  fork (id . fst, id . snd)
=    { apply id }
  fork (fst, snd)

If we apply fork (fst, snd) to (x, y):
  fork (fst, snd) (x , y)
=    { definition of fork }
  (fst (x, y), snd (x, y))
=    { apply fst and snd }
  (x, y)

Therefore
  cross (id, id) = id

> class Bifunctor p where
>     bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
>
> newtype Pair a b = P (a, b)
>
> instance Bifunctor Pair where
>     bimap f g (P (x, y)) = P (f x, g y)
>
> cross :: (a -> b, c -> d) -> Pair a c -> Pair b d
> cross (f, g) p = bimap f g p
>
> instance Bifunctor Either where
>     bimap f g (Left a) = Left (f a)
>     bimap f g (Right b) = Right (g b)
