{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as B
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Char (chr)
import Data.List (group, sort)
import Data.Bits

split3 :: [a] -> [[a]]
split3 (h1:h2:h3:t) = let ~[t1, t2, t3] = split3 t in [h1:t1, h2:t2, h3:t3]
split3 (   h1:h2:t) = let ~[t1, t2, t3] = split3 t in [h1:t1, h2:t2,    t3]
split3 (      h1:t) = let ~[t1, t2, t3] = split3 t in [h1:t1,    t2,    t3]
split3 _            = [[], [], []]

toBits :: String -> Int
toBits s = read s :: Int

getMostFrequent :: [Int] -> Int
getMostFrequent = snd . maximum . map (\ l -> (length l, head l)) . group . sort

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

spaceToKey :: Int -> Int
spaceToKey n = xor n 32

main :: IO ()
main = do
    encrypted   <- B.unpack . head . B.lines <$> B.readFile "euler59.txt";
    let txt = map toBits . wordsWhen (== ',') $ encrypted in
        let key = map (spaceToKey . getMostFrequent) . split3 $ txt in
        let decrypted = zipWith xor txt (cycle key) in
        print (sum decrypted, map chr decrypted)
