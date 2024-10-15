showDate :: Date -> String
showDate (d, m, y) = ordinal d ++ " " ++ month m ++ ", " ++ show y

type Date = (Int, Int, Int)  -- day, month, year

ordinal :: Int -> String
ordinal d = case d `mod` 10 of
                1 | d == 11   -> "11th"
                  | otherwise -> show d ++ "st"
                2 | d == 12   -> "12th"
                  | otherwise -> show d ++ "nd"
                3 | d == 13   -> "13th"
                  | otherwise -> show d ++ "rd"
                _ -> show d ++ "th"

month :: Int -> String
month m = monthName !! (m - 1)
          where
              monthName = ["January", "February", "March", "April", "May", "June",
                           "July", "August", "September", "October", "November", "December"]
