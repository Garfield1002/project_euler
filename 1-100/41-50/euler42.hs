import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (ord)
import Util (split)

triangles :: [Int]
triangles = [quot (n * (n+1)) 2 | n <- [1..]]

elemOrd :: Ord t => t -> [t] -> Bool
elemOrd n [] = False
elemOrd n (x:xs) = n == x || (x < n && elemOrd n xs)

isTriangle :: [Char] -> Bool
isTriangle s = elemOrd (sum [ord c - 64 | c <- s]) triangles

main = do
    l <- B.unpack . head . B.lines <$> B.readFile "euler42.txt"
    print $ length . filter isTriangle . map (filter ('\"' /=)) . split ',' $ l
