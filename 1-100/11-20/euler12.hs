isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

countDivisors :: Integer -> Integer
countDivisors n = sum [c | d <- [1..s]
                    , let (q,r) = divMod n d
                    , let c = if q /= d then 2 else 1
                    , r == 0]
    where s = isqrt n

triangleNumbers :: [Integer]
triangleNumbers = map (\i -> i * (i+1) `div` 2) [1..]
-- triangleNumbers = map fst triangleNumbers'
    -- where triangleNumbers' = (1,2) : map (\(n, i) -> (n + i, i + 1)) triangleNumbers'

euler12 :: Integer
euler12 = head [x | x <- triangleNumbers, countDivisors x > 500]