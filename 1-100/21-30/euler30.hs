-- (log x) * 9^5 = x => x = 355,000

f :: Integral t => t -> t
f x
    | q == 0    = r^5
    | otherwise = r^5 + f q
    where (q, r) = divMod x 10

euler30 :: Integer
euler30 = sum [x | x <- [1..355000], f x == x]