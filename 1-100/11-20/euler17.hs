toLetters:: [Int] -> [Char]
toLetters []    = ""
toLetters [0]  = ""
toLetters [1]  = "one"
toLetters [2]  = "two"
toLetters [3]  = "three"
toLetters [4]  = "four"
toLetters [5]  = "five"
toLetters [6]  = "six"
toLetters [7]  = "seven"
toLetters [8]  = "eigth"
toLetters [9]  = "nine"
toLetters [1, 0]  = "ten"
toLetters [1, 1]  = "eleven"
toLetters [1, 2]  = "twelve"
toLetters [1, 3]  = "thirteen"
toLetters [1, 4]  = "fourteen"
toLetters [1, 5]  = "fifteen"
toLetters [1, 6]  = "sixteen"
toLetters [1, 7]  = "seventeen"
toLetters [1, 8]  = "eighteen"
toLetters [1, 9]  = "nineteen"
toLetters [2, c]  = "twenty" ++ toLetters [c]
toLetters [3, c]  = "thirty" ++ toLetters [c]
toLetters [4, c]  = "forty" ++ toLetters [c]
toLetters [5, c]  = "fifty" ++ toLetters [c]
toLetters [6, c]  = "sixty" ++ toLetters [c]
toLetters [7, c]  = "seventy" ++ toLetters [c]
toLetters [8, c]  = "eighty" ++ toLetters [c]
toLetters [9, c]  = "ninety" ++ toLetters [c]
toLetters [0, c]  = toLetters [c]
toLetters [a, b, c]  = toLetters [a] ++ "hundred" ++ (if (b == 0) && (c == 0) then "" else "and") ++ toLetters [b,c]
toLetters [1, 0, 0, 0] = "onethousand"
toLetters _ = ""

toList :: Int -> [Int]
toList = reverse . toList'
    where toList' i
            | q == 0    = [r]
            | otherwise = r : toList' q
            where (q,r) = divMod i 10

euler17 = sum (map (length . toLetters . toList) [1..1000])
