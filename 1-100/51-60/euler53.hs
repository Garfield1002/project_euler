combinations :: Integral a => a -> a -> a
combinations n k = quot (product [n - k + 1..n]) (product [1..k])

euler53 :: Int
euler53 = length [x | n <- [1..100], k <- [1..n], let x = combinations n k, x > 1000000]
