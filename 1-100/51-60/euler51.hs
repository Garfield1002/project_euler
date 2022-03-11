import Data.List
import Utils.Miscellaneous (digits2num, num2digits)
import Utils.Primes (isPrime, primes)

-- | `replace x y l` replaces `x` by `y` in the list `l`
replace :: Eq a => a -> a -> [a] -> [a]
replace x y [] = []
replace x y (h : t)
  | h == x = y : replace x y t
  | otherwise = h : replace x y t

isPart8Prime :: [Integer] -> Bool
isPart8Prime l = foo 0
  where
    foo d
      | d >= 10 = False
      | numberD < 2 = foo (d + 1)
      | otherwise = ((< 3) . length . filter (not . (\x -> x > 10 ^ 4 && isPrime x) . digits2num) $ [replace d n l | n <- [0 .. 9], n /= d]) || foo (d + 1)
      where
        numberD = length . filter (== d) $ l

main :: IO ()
main = print . head . filter (isPart8Prime . num2digits) . takeWhile (< 10 ^ 6) . dropWhile (< 10 ^ 4) $ primes
