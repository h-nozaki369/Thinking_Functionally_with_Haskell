No, because only x is incremented but y stays at 0 forever.

> allPairs = [(x, sumPairs - x) | sumPairs <- [0..], x <- [0..sumPairs]]
