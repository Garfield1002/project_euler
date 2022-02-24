toDigits :: Integral t => t -> [t]
toDigits n
    | q == 0    = [r]
    | otherwise = r : toDigits q
    where (q, r) = quotRem n 10

main :: IO ()
main = print . maximum $ [sum . toDigits $ a^b | a <- [1..100], b <- [1..100]]