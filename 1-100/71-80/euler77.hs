import           Data.Array (listArray, (!), elems)
import           Utils.Primes (isPrime)

-- To find the amount of sums to n, we look for the amount of sums to n - k for k in [1..n-1].
-- We also need to prevent any overlap.
-- To do so, we only consider the sums using integers smaller than k.
-- We will take care of dynamic programming and create a table `phi` where `phi x y`
-- is the set of all ways to write `x` as a sum of integers lesser then `y`.
-- | The `sums n` function returns the set of all ways to write an integer `n` as a sum of integers.
sums :: Int -> [[Int]]
sums n = phi ! (5, n)
  where
    go x y
      | y > x = phi ! (x, x)
      | not . isPrime . fromIntegral $ y = phi ! (x, y - 1)
      | y <= 1 = [] -- map (y:) (phi ! (x - y, y))
      | x - y <= 0 = [y]:phi ! (x, y - 1)
      | otherwise = map (y:) (phi ! (x - y, y)) ++ (phi ! (x, y - 1))

    phi = listArray ((1, 1), (n, n)) [go x y | x <- [1 .. n], y <- [1 .. n]]

-- | The `sumsLength n` function returns amount of different ways to write an integer `n` as a sum of integers.
-- It is much quicker then simply calling `length . sums`, but will yield the same result.
sumsLength :: Int -> [Int]
sumsLength n = [r | x <- [1 .. n], let r = phi ! (x, n)]
  where
    go x y
      | y > x = phi ! (x, x)
      | not . isPrime . fromIntegral $ y = phi ! (x, y - 1)
      | y <= 1 = 0
      | x - y <= 0 = 1 + phi ! (x, y - 1)
      | otherwise = phi ! (x - y, y) + phi ! (x, y - 1)

    phi = listArray ((1, 1), (n, n)) [go x y | x <- [1 .. n], y <- [1 .. n]]

main :: IO ()
main = print . head . dropWhile ((< 5000) . snd) . zip [1 ..] . sumsLength
  $ 100
