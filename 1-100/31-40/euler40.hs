import Data.List ( intercalate )

str :: [Char]
str = intercalate "" [show x | x <- [1..10^6]]

intN :: Int -> Int
intN n  = read [str !! (n - 1)]

euler40 = intN 1 * intN 10 * intN 100 * intN (10^3) * intN (10^4) * intN (10^5) * intN (10^6)
