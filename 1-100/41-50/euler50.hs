import Data.List(nub)

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: Integral a => [a] -> [a] -> [a]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

isPrime :: Int -> Bool
isPrime n = isPrime' primes where isPrime' ~(p:ps) = p * p > n || (rem n p /= 0 && isPrime' ps)

euler50 = takeWhile (< (10^6, 0)) [(p, n) | n <- [1..], let p = sum (take n (tail.tail.tail.tail.tail $ primes)), isPrime p]
