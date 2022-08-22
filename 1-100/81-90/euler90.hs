import qualified Data.Set as S
import qualified Data.List as L

combinations :: Int -> [a] -> [[a]]
combinations k ns = filter ((k ==) . length) $ L.subsequences ns

r :: Int -> Int -> S.Set Int -> S.Set Int -> Bool
r x1 x2 d1 d2 = if x2 == 6
                then r' x1 x2 d1 d2 || r' x1 9 d1 d2
                else r' x1 x2 d1 d2
  where
    r' x1 x2 d1 d2 = (x1 `elem` d1 && x2 `elem` d2)
      || (x1 `elem` d2 && x2 `elem` d1)

rs :: [S.Set Int -> S.Set Int -> Bool]
rs = [r 0 1, r 0 4, r 0 6, r 1 6, r 1 8, r 2 5, r 3 6, r 4 6]

isValid :: S.Set Int -> S.Set Int -> Bool
isValid d1 d2 = and [r d1 d2 | r <- rs]

sol = length . filter id
  $ [isValid (S.fromAscList d1) (S.fromAscList d2)
    | d1 <- combinations 6 [0 .. 9]
    , d2 <- combinations 6 [0 .. 9]]
-- mistake with ||
-- div by 2