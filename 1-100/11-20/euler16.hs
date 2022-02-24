readInt :: Char -> Integer
readInt a = read [a] ::Integer

euler16 = sum $ map readInt (show $ 2^1000)