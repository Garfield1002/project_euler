coins :: [Integer]
coins = [1, 2, 5, 10, 20, 50, 100, 200]

euler31' 0 _  = 1
euler31' _ [] = 0 -- never happens
euler31' v (h:t)
    | h == 1     = 1
    | v - h >= 0 = euler31' (v - h) (h:t) + euler31' v t
    | otherwise  = euler31' v t

euler31 = euler31' 200 [200, 100, 50, 20, 10, 5, 2, 1]