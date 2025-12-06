module ExerciseSet4 where
  import Test.QuickCheck

  a = [1, 2, 3]
  b = [4, 3, 2, 1]
  c = [9, 10]
  abc = [a, b, c]
  
  maxInLists :: [[Integer]] -> Integer
  maxInLists [] = 0
  maxInLists xs = maximum (map maximum xs)

  -- 1st : General trees
  -- def
  data GTree a = Leaf a | Gnode [GTree a] -- leaf node and list of subtrees
    deriving (Eq,Ord,Show,Read)

  -- depth
  depth :: GTree a -> Integer
  depth (Leaf _) = 1
  depth (Gnode trees) = 1 + maximum (map depth trees) 

  -- whether an element occurs in a GTree
  isInTree :: Eq a => GTree a -> a -> Bool
  isInTree (Leaf val) x = val == x
  isInTree (Gnode trees) x = foldr (||) False (map (\subtree -> isInTree subtree x) trees)
--  isInTree (Gnode (tree:trees)) x

  t2 = Gnode [Leaf 'a', Leaf 'b', Leaf 'c']
  -- mapTree
