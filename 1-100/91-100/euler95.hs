import           Data.List (maximumBy)
import           Data.Ord (comparing)
import           System.TimeIt (timeIt)
import           Data.Set (empty, insert, member)

limit :: Int
limit = 10 ^ 6

sumOfFactors :: Int -> Int
sumOfFactors n = subtract n
  . (\s -> if isPerfectSquare n
           then s - root
           else s)
  . sum
  . map (\x -> x + n `div` x)
  . filter (\x -> n `mod` x == 0)
  . takeWhile (\p -> p * p <= n)
  $ [1 ..]
  where
    root = round (sqrt . fromIntegral $ n :: Double)

    isPerfectSquare x = root * root == x

chain :: Int -> (Int, Int)
chain n = chain' empty n n 1
  where
    chain' previous k m len
      | s > limit = (0, 0)
      | s < n = (0, 0)
      | s == n = (len, n)
      | member s previous = (0, 0)
      | otherwise = chain' (insert s previous) s (min m s) (len + 1)
      where
        s = sumOfFactors k

sol :: (Int, Int)
sol = maximumBy (comparing fst) $ map chain [1 .. limit]

main = timeIt $ print sol
