import           Data.Array                 (Array, bounds, listArray, (!))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as L
import           Data.Set                   (Set, empty, insert, member)
import           Util                       (split)


-- | transforms a string to a square array (y, x)
arrFromStr :: [[Char]] -> Array (Int, Int) Int
arrFromStr s = listArray ((1, 1), (yMax, xMax))
                         [ e | row <- tbl, e <- take xMax row ]
  where
    tbl  = map (map read . split ',') s
    yMax = length tbl
    xMax = minimum $ map length tbl

s :: [[Char]]
s =
    ["131,673,234,103,18","201,96,342,965,150","630,803,746,422,111","537,699,497,121,956","805,732,524,37,331"]

shortestPath costs = shortestPath' (L.sort [(costs ! (y, 1), y, 1) | y <- [1..yMax]]) empty
  where
    shortestPath' ~((cost, y, x) : neighbors) visited
        | x == xMax = cost
        | otherwise =
            let (n1, v1) = explore neighbors visited (1, 0) in
            let (n2, v2) = explore n1 v1 (-1, 0) in
            let (n3, v3) = explore n2 v2 (0, 1) in
            shortestPath' (L.sort n3) v3
        where
            explore neighbors visited (dy, dx)
                | x + dx > xMax
                || y + dy > yMax
                || x + dx < 1
                || y + dy < 1
                || member (y + dy, x + dx) visited = (neighbors, visited)
                | otherwise = ((cost + costs ! (y + dy, x + dx), y + dy, x + dx):neighbors, insert (y + dy, x + dx) visited)
    (_, (yMax, xMax)) = bounds costs

main = do
    str <- map B.unpack . B.lines <$> B.readFile "euler81.txt"
    print . shortestPath . arrFromStr $ str
