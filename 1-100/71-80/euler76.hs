import Data.Array (array, listArray, (!))
import Data.List (inits, nub, sort)

-- To find the amount of sums to n, we look for the amount of sums to n - k for k in [1..n-1].
-- We also need to prevent any overlap.
-- To do so,

sums :: Int -> [[Int]]
sums n = phi ! (n, n)
  where
    go 1 1 = [[1]]
    go x y
      | y > x = phi ! (x, x)
      | y <= 1 = map (y :) (phi ! (x - y, y))
      | x - y <= 0 = [y] : phi ! (x, y - 1)
      | otherwise = map (y :) (phi ! (x - y, y)) ++ (phi ! (x, y - 1))
    phi = listArray ((1, 1), (n, n)) [go x y | x <- [1 .. n], y <- [1 .. n]]

sumsLength :: Int -> Int
sumsLength n = phi ! (n, n)
  where
    go 1 1 = 1
    go x y
      | y > x = phi ! (x, x)
      | y <= 1 = phi ! (x - y, y)
      | x - y <= 0 = 1 + phi ! (x, y - 1)
      | otherwise = phi ! (x - y, y) + phi ! (x, y - 1)
    phi = listArray ((1, 1), (n, n)) [go x y | x <- [1 .. n], y <- [1 .. n]]

main :: IO ()
main = print $ sumsLength 100 - 1
