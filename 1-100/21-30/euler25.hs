fib :: Integer -> Integer
fib = fst . fib'
        where fib' 1 = (1, 1)
              fib' n = (x + y, x) where (x, y) = fib' (n - 1)

fibList :: [Integer]
fibList = map snd fibList'
        where fibList' = (1,1) : map (\ (x, y) -> (x + y, x)) fibList'


f :: [(Integer, b)] -> [(Integer, b)]
f = filter ((1000 <=) . length . show . fst)

euler25 :: Integer
euler25 = snd $ head ( f (zip fibList [1..]))
