import Data.Char

isPalindrome :: String -> Bool
isPalindrome xs = normalize == reverse normalize
                  where normalize = (map toLower . filter isAlpha) xs

palindrome :: IO ()
palindrome = do
    putStrLn "Enter a string:"
    txt <- getLine;
    let resp = if isPalindrome txt then "Yes!" else "No!"
    putStrLn resp
