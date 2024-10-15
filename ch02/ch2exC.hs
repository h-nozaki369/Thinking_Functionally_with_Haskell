import Data.Char

toUpperTitle :: String -> String
toUpperTitle = unwords . map toUpperFirst . words

toUpperFirst :: String -> String
toUpperFirst s = (toUpper . head) s : (tail s)

main :: IO ()
main = print$ toUpperTitle "The morphology of prex - an essay in meta-algorithmics"
