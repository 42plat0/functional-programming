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
  inTree :: Eq a => GTree a -> a -> Bool
  inTree (Leaf val) x = val == x
  inTree (Gnode trees) x = foldr (||) False $ map (\subtree -> inTree subtree x) trees

  t2 = Gnode [Leaf 3, Leaf 4, Leaf 5]
  -- mapTree
  mapTree :: (a -> b) -> GTree a -> GTree b
  mapTree f (Leaf a) = Leaf (f a)
  mapTree f (Gnode trees) = Gnode (map (\subtree -> mapTree f subtree) trees)

