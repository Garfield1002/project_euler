import Data.List (nub)

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs


primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n primes
    where primeFactors' n' ~(h:t)
            | n' == 1    = []
            | r == 0     = h:primeFactors' q (h:t)
            | otherwise  = primeFactors' n' t
            where (q, r) = quotRem n' h

bound :: Int
bound = 4

euler47 :: [Int] -> Int -> [Int]
euler47 l n
    | length l == bound                          = l
    | (length . nub . primeFactors $ n) == bound = euler47 (n:l) (n+1)
    | otherwise                                  = euler47 [] (n+1)

main = print . euler47 [] . product . take bound $ primes
