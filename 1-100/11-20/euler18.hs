import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)

toInt :: ByteString -> Int
toInt s = int where Just(int, _) = B.readInt s

toList :: ByteString -> [Int]
toList bs = toInt <$> B.split ' ' bs

sumList :: [Int] -> [Int] -> [Int]
sumList (h:t) (h':t') = (h + h'):sumList t t'
sumList _ _           = []

maxList :: [Int] -> [Int]
maxList (a:b:t) = max a b : maxList (b:t)
maxList _       = []

euler18 :: [[Int]] -> Int
euler18 (a:b:t) = euler18 $ sumList (maxList a) b:t
euler18 [[a]]   = a
euler18 _       = 0

main = do
    matrix <- map toList . B.lines <$> B.readFile "euler67.txt"
    print $ euler18 $ reverse matrix


