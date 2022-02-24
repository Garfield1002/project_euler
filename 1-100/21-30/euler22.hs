import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Char (ord, isSpace)
import Data.Bifunctor (Bifunctor(first))
import Data.List ( sort )

score :: [Char] ->  Int
score s = sum [ord c - 64 | c <- s, not $ isSpace c]

euler22 l = euler22' l 1 0
        where euler22' (h:t) i s = euler22' t (i + 1) (s + i * h)
              euler22' [] i s    = s

main = do
    l <- map  B.unpack . B.lines <$> B.readFile "euler22.txt"
    print $ euler22 (map score (sort l))
