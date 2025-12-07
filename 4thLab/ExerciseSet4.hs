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
  t7 =
    Gnode
      [ Leaf 1
      , Gnode
          [ Leaf 2
          , Gnode [Leaf 3, Leaf 4]
          , Leaf 5
          ]
      , Gnode
          [ Gnode [Leaf 6]
          , Leaf 7
          ]
      ]

  -- mapTree
  mapTree :: (a -> b) -> GTree a -> GTree b
  mapTree f (Leaf a) = Leaf (f a)
  mapTree f (Gnode trees) = Gnode (map (\subtree -> mapTree f subtree) trees)

  -- 2nd: general expressions
  data Expr a = Lit a | EVar Var | Op (Ops a) [Expr a]
  type Ops a = [a] -> a
  type Var = Char
  type Valuation a = [(Var, a)]

  -- util
  sumNums :: Num a => [a] -> a
  sumNums = foldr (+) 0

  -- gets value of var
  lookupVarValue :: Valuation a -> Var -> Maybe a
  lookupVarValue [] _ = Nothing -- cia galetu buti monada Nothing, o radus = Just val
  lookupVarValue ((keyVar, val):xs) var
    | keyVar == var = Just val
    | otherwise = lookupVarValue xs var

  extract :: Maybe a -> a
  extract (Just x) = x
  extract Nothing = error "not found variable value"

  eval :: Num a => Valuation a -> Expr a -> a
  eval _ (Lit a) = a
  eval valuation (EVar var) = extract $ lookupVarValue valuation var
  eval valuation (Op func exprs) = func $ map (\expr -> eval valuation expr) exprs

  -- Expression: 42
  expr1 = Lit 42
  val1 = []  -- no variables needed
  -- Expected: 42
  test1 = eval val1 expr1

  -- Expression: x
  expr2 = EVar 'y'
  val2 = [('x', 10)]
  -- Expected: 10
  test2 = eval val2 expr2

  -- Expression: sum([1, x, 3])
  expr3 = Op sumNums [Lit 1, EVar 'x', Lit 3]
  val3 = [('x', 5)]
  -- Expected: 1 + 5 + 3 = 9
  test3 = eval val3 expr3


  -- 3rd 
