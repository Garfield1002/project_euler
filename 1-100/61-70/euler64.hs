import Data.List

{-
We represent the fraction
\frac{\alpha \sqrt{N} + \beta}{\gamma}
as the tuple (\alpha, \beta, \gamma)
-}
step :: (Int, Int, Int) -> Int -> (Int, (Int, Int, Int))
step (a, b, c) n =
    let e = truncate $ (fromIntegral a * sqrt (fromIntegral n) + fromIntegral b) / fromIntegral c in
    let (a', b', c') = (a * c, c^2 * e - b * c, a^2 * n - (b - c * e)^2) in
    let g = gcd a' $ gcd b' c' in
    (e, (quot a' g, quot b' g, quot c' g))

frac :: Int -> [Int]
frac n = frac' (1, 0, 1) [] []
    where frac' s acc acc' =
            let (e, s') = step s n in
            if s' `elem` acc'
            then e:acc
            else frac' s' (e:acc) (s':acc')

generator :: (Eq a, Num a) => a -> [a]
generator n = generator' 2 2
    where generator' i s
            | i == n + 1 = []
            | i == s * s = generator' (i + 1) (s + 1)
            | otherwise = i : generator' (i + 1) s

euler64 = length . filter ((1 == ) . flip rem  2) $ [(+) (-1) . length . frac $ n | n <- generator (10^4) ]