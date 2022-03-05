-- 428.0 / 999.0 was found by running `euler71 $ 10 ^ 3`

euler71 :: Int -> (Int, Int)
euler71 m = euler71' 1 1 1 8
  where
    euler71' n d n' d'
      | d > m = (n', d')
      | n >= d = euler71' (floor (428.0 / 999.0 * fromIntegral (d + 1))) (d + 1) n' d'
      | 7 * n >= 3 * d = euler71' (floor (428.0 / 999.0 * fromIntegral (d + 1))) (d + 1) n' d'
      | gcd n d /= 1 = euler71' (n + 1) d n' d'
      | d' * n > n' * d = euler71' (n + 1) d n d
      | otherwise = euler71' (n + 1) d n' d'

main :: IO ()
main = print . euler71 $ 10 ^ 6
