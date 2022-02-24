import qualified Data.ByteString.Lazy.Char8 as B
import Data.List ( transpose )
import Data.ByteString.Lazy.Char8 (ByteString)

toInt :: ByteString -> Int
toInt s = int where Just(int, _) = B.readInt s

toList :: ByteString -> [Int]
toList bs = toInt <$> B.split ' ' bs

diamond :: [[a]] -> [[a]]
diamond = tail . go [] where
    go b es_ = [h | h:_ <- b] : case es_ of
        []   -> transpose ts
        e:es -> go (e : ts) es
        where ts = [t | _:t <- b]

prod4 :: Int -> [Int] -> Int
prod4 m l = case l of
    w:x:y:z:t -> if w*x*y*z > m then prod4 (w*x*y*z) $ x:y:z:t else prod4 m $ x:y:z:t
    _ -> m

main = do
    matrix <- map toList . B.lines <$> B.readFile "euler11.txt"
    let combinations = [matrix, diamond matrix, transpose matrix, diamond $ reverse <$> matrix]
    let max = maximum $ maximum . map (prod4 0) <$> combinations
    print max