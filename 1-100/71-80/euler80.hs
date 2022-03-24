import           Utils.Miscellaneous (num2digits)

newton :: Integral t => t -> t -> t
newton n xn
  | abs (xn' - xn) <= 1 = xn
  | otherwise = newton n xn'
  where
    xn' = quot (xn + quot n xn) 2

euler80 :: Integer
euler80 =
  sum [f x | x <- [2 .. 99], x `notElem` [4, 9, 16, 25, 36, 49, 64, 81]]
  where
    f x = sum . drop 11 . num2digits $ newton (x * 10 ^ 220) (10 ^ 110)
