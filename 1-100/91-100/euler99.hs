import qualified Data.ByteString.Lazy.Char8 as B
import Util (split)

numberToExp :: [String] -> Integer
numberToExp [n1, n2] = (read n1 :: Integer) ^ (read n2 :: Integer)
numberToExp _ = 1

main :: IO ()
main =
  do
    lines <- map B.unpack . B.lines <$> B.readFile "euler99.txt"
    let numbers = map (numberToExp . split ',') lines
    let indexed = zip numbers [1 ..]
    print . snd . maximum $ indexed
