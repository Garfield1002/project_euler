import qualified Data.ByteString.Lazy.Char8 as B


fromRomanNumeral :: Num a => [Char] -> a
fromRomanNumeral ('M':t)      = 1000  + fromRomanNumeral t
fromRomanNumeral ('C':'M':t)  = 900   + fromRomanNumeral t
fromRomanNumeral ('D':t)      = 500   + fromRomanNumeral t
fromRomanNumeral ('C':'D':t)  = 400   + fromRomanNumeral t
fromRomanNumeral ('C':t)      = 100   + fromRomanNumeral t
fromRomanNumeral ('X':'C':t)  = 90    + fromRomanNumeral t
fromRomanNumeral ('L':t)      = 50    + fromRomanNumeral t
fromRomanNumeral ('X':'L':t)  = 40    + fromRomanNumeral t
fromRomanNumeral ('X':t)      = 10    + fromRomanNumeral t
fromRomanNumeral ('I':'X':t)  = 9     + fromRomanNumeral t
fromRomanNumeral ('V':t)      = 5     + fromRomanNumeral t
fromRomanNumeral ('I':'V':t)  = 4     + fromRomanNumeral t
fromRomanNumeral ('I':t)      = 1     + fromRomanNumeral t
fromRomanNumeral _            = 0


toRomanNumeral :: Int -> [Char]
toRomanNumeral n
  | n >= 1000 = 'M' : toRomanNumeral (n - 1000)
  | n >= 900  = 'C' : 'M' : toRomanNumeral (n - 900)
  | n >= 500  = 'D' : toRomanNumeral (n - 500)
  | n >= 400  = 'C' : 'D' : toRomanNumeral (n - 400)
  | n >= 100  = 'C' : toRomanNumeral (n - 100)
  | n >= 90   = 'X' : 'C' : toRomanNumeral (n - 90)
  | n >= 50   = 'L' : toRomanNumeral (n - 50)
  | n >= 40   = 'X' : 'L' : toRomanNumeral (n - 40)
  | n >= 10   = 'X' : toRomanNumeral (n - 10)
  | n >= 9    = 'I' : 'X' : toRomanNumeral (n - 9)
  | n >= 5    = 'V' : toRomanNumeral (n - 5)
  | n >= 4    = 'I' : 'V' : toRomanNumeral (n - 4)
  | otherwise = replicate n 'I'


main :: IO ()
main = do
  old_numbers <- map B.unpack . B.lines <$> B.readFile "euler89.txt"
  let numbers = map (toRomanNumeral . fromRomanNumeral) old_numbers
  -- B.writeFile "euler89_s.txt" $ B.pack $ foldl (\ acc line -> acc ++ '\n' : line) "" numbers
  print $ (sum . map length $ old_numbers) - (sum . map length $ numbers)
