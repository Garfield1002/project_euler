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

shortestPath costs = shortestPath' [(costs ! (1, 1), 1, 1)] empty
  where
    shortestPath' ~((cost, y, x) : neighbors) visited
        | x == xMax && y == yMax = cost
        | x == xMax = if member (x, y + 1) visited
            then shortestPath' neighbors visited
            else shortestPath' (L.sort $ (cost + costs ! (y + 1, x), y + 1, x):neighbors) (insert (x, y + 1) visited)
        | y == xMax = if member (x + 1, y) visited
            then shortestPath' neighbors visited
            else shortestPath' (L.sort $ (cost + costs ! (y, x + 1), y, x  + 1):neighbors) (insert (x + 1, y) visited)
        | otherwise =
            let (neighbors', visited') =
                    if member (x, y + 1) visited
                    then (neighbors, visited)
                    else ((cost + costs ! (y + 1, x), y + 1, x):neighbors, insert (x, y + 1) visited) in
                if member (x + 1, y) visited
                then shortestPath' (L.sort neighbors') visited'
                else shortestPath' (L.sort $ (cost + costs ! (y, x + 1), y, x  + 1):neighbors') (insert (x + 1, y) visited')

    (_, (yMax, xMax)) = bounds costs

main = do
    str <- map B.unpack . B.lines <$> B.readFile "euler81.txt"
    print . shortestPath . arrFromStr $ str
