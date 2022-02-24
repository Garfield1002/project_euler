type Set a = [a]

powerset :: Set a -> Set (Set a)
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- l] ++ l
            where l = powerset xs

primes :: [Int]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997, 1009]

primeFactors :: Int -> Set Int
primeFactors i = primeFactors' i primes []
        where primeFactors' i (h:t) fac
                | i == 1        = fac
                | mod i h == 0  = primeFactors' (div i h) (h:t) (h:fac)
                | otherwise     = primeFactors' i t fac
              primeFactors' i [] fac = fac -- should never be used

divisors :: Int -> Set Int
divisors n = map product (powerset $ primeFactors n)


isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

divisors' :: Int -> Set Int
divisors' n = divisors' 1 []
    where divisors' i l
            | i > isqrt n = l
            | otherwise   = let (q, r) = divMod n i in if r == 0 then divisors' (i + 1) (i:q:l) else divisors' (i + 1) l

d :: Int -> Int
d i = sum (divisors' i) - i

isAmicable :: Int -> Bool
isAmicable i = d (d i) == i && d i /= i

euler21 = sum [i | i <- [1..10000],  isAmicable i]
