import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)

toInt :: ByteString -> Integer
toInt s = int where Just(int, _) = B.readInteger s

euler13 = do
    int <- map toInt . B.lines <$> B.readFile "euler13.txt"
    print $ take 10 . show $ sum int