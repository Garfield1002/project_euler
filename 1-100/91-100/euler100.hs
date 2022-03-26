-- the coefficients used to solve the generic two integer variable equation where found here
-- https://www.alpertron.com.ar/QUAD.HTM
euler100 :: Int
euler100 = next 15 21
  where
    next x y
      | y < 10 ^ 12 = next (3 * x + 2 * y - 2) (4 * x + 3 * y - 3)
      | otherwise = x
