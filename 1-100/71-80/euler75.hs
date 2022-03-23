import           Data.List (sort, group)
import           Data.Tree (flatten, unfoldTree, Tree(Node, rootLabel))
import           Data.Array.Unboxed ((//), (!), accumArray, elems, UArray)

-- | Generates all of the primitive pythagorean triplets using a Tree of primitive Pythagorean triples
pT :: Tree (Int, Int)
pT = unfoldTree genTree (2, 1)
  where
    genTree (m, n) = ((m, n), [(2 * m - n, m), (2 * m + n, m), (m + 2 * n, n)])

-- Removes any node greater than 1500000
prune :: Tree Int -> Tree Int
prune (Node p l) = Node p . map prune . filter ((<= 1500000) . rootLabel) $ l

-- All Primitive triplets Perimeter under l
maina = print
  . length
  . filter (== 1)
  . elems
  . (\ns -> accumArray (+) 0 (1, 1500000) [(n, 1) | n <- ns] :: UArray Int Int)
  . concatMap (\p -> takeWhile (<= 1500000) [p, 2 * p ..])
  . flatten
  . prune
  . fmap (\(m, n) -> 2 * m * (m + n))
  $ pT
