toList :: Integer -> [Integer]
toList = reverse . toList'
    where toList' i
            | q == 0    = [r]
            | otherwise = r : toList' q
            where (q,r) = divMod i 10

fac :: Integer -> Integer
fac n = product [1..n]
euler20 = sum $ toList $ fac 100