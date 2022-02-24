import Data.List ( sort )

isPandigital :: [Char] -> Bool
isPandigital = ("123456789" ==) . sort

euler38 :: [Char]
euler38 = maximum [s | n <- [9182..9876], let s = show n ++ show (2 * n), isPandigital s]
