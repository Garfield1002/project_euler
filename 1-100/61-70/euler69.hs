primes :: [Integer]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

euler69 :: [Integer] -> Integer -> Integer
euler69 ~(h:t) n =
    if n * h > 10^6 then n else euler69 t (n * h)
