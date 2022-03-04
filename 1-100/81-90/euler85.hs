import Utils.Miscellaneous (combinations)

-- A rectangle needs 2 vertical lines and 2 horizontal lines. In a w by h grid there are
-- w + 1 vertical lines and h + 1 horizontal lines.
-- Therefore there are `combinations (w + 1) 2 * combinations (h + 1) 2` rectangles in a w by h grid

-- | The `rectangles w h` function returns the amount of rectangles in a `w` by `h` grid.
rectangles :: Integral a => a -> a -> a
rectangles w h = combinations (w + 1) 2 * combinations (h + 1) 2

euler85 :: Integral a => a -> a -> a -> a -> (a, a)
euler85 w h s a
  | h > w = (s, a)
  | up < 2 * 10 ^ 6 = euler85 w (h + 1) s a -- should never occur
  | down > 2 * 10 ^ 6 = euler85 w (h - 1) s a
  | up - 2 * 10 ^ 6 < s = euler85 (w - 1) h (up - 2 * 10 ^ 6) (w * h)
  | 2 * 10 ^ 6 - down < s = euler85 (w - 1) h (2 * 10 ^ 6 - down) (w * h - w)
  | otherwise = euler85 (w - 1) h s a
  where
    up = rectangles w h
    down = rectangles w (h - 1)

-- To find the starting value we look for the width surrounding 2 * 10 ^ 6

-- > combinations 2000 2 - 2*10^6
-- -1000
-- > combinations 2001 2 - 2*10^6
-- 1000

main :: IO ()
main = print . euler85 2001 2 1000 $ 1
