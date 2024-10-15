addSum :: CIN -> CIN
addSum xs = xs ++ show (n `div` 10) ++ show (n `mod` 10)
            where n = (sum . map getDigit) xs

type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

valid :: CIN -> Bool
valid xs = (read . drop 8) xs == (sum . map getDigit) (take 8 xs)

main :: IO ()
main = do
    print $ addSum "63245134"
    print $ valid "6324513428"
    print $ valid "6324513425"
    print $ valid "6324523428"
