import Control.Parallel.Strategies ( parList, rdeepseq, rpar, using )

primes :: [Int]
primes = 2: 3: sieve (tail primes) [5,7..]

sieve :: [Int] -> [Int] -> [Int]
sieve ~(p:ps) xs = h ++ sieve ps [x | x <- t, rem x p /= 0]
  where (h,~(_:t)) = span (< p*p) xs

isSquare :: Integral a => a -> Bool
isSquare = (0.0 ==) . snd . properFraction . sqrt . fromIntegral

isComposite :: Int -> Bool
isComposite n = isComposite' primes False
    where isComposite' ~(p:ps) flag = p * p < n &&
                                      if rem n p == 0
                                      then flag || isComposite' ps True
                                      else isComposite' ps flag
isGoldbach :: Int -> Bool
isGoldbach n = isGoldbach' (tail primes)
    where isGoldbach' ~(p:ps) = p == n || (p < n && (isSquare (quot (n - p) 2) || isGoldbach' ps))

main :: IO ()
main = print . head $ [n | n <- [2..], odd n && not (isGoldbach n)]
