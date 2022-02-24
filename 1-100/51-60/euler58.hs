primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p * p) xs

isPrime :: Int -> Bool
isPrime n = isPrime' primes
    where isPrime' ~(p:ps) = p * p > n || (rem n p /= 0 && isPrime' ps)

diags :: [Int]
diags = diagsMap 1 0 0
    where   diagsMap x s 0 = diagsMap x (s + 2) 4
            diagsMap x s n = x : diagsMap (x + s) s (n - 1)

euler58 ~(h:t) c n
--   | n == 13 = fromIntegral c / fromIntegral (n - 1)
  | fromIntegral c / fromIntegral n < 0.1 = 1 + 2 * (1 + quot (n - 2) 4)
  | isPrime h = euler58 t (c + 1) (n + 1)
  | otherwise = euler58 t c (n + 1)

f :: Integer -> Integer
f = (1 +) . (2 *) . (+ 1) . flip quot 4 . flip (-) 2

main = print . euler58 (drop 5 diags) 3 $ 5