module ExerciseSet4 where
  import Test.QuickCheck
  import Data.List

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


  -- 3rd  regular expressions
  --
  -- splits
  splits :: [a] -> [([a], [a])]
  splits list = splitsAccumulator list [] 0

  splitsAccumulator :: [a] -> [([a], [a])] -> Int -> [([a], [a])]
  splitsAccumulator list acc idx
    | splitSndLength > 0 = splitsAccumulator list (acc ++ [splitVal]) (idx+1)
    | otherwise = acc ++ [splitVal]
    where
      splitVal = splitAt idx list
      splitSndLength = length $ snd $ splitVal
  --
  --
  type RegExp = String -> Bool

  epsilon :: RegExp
  epsilon = (=="")

  char :: Char -> RegExp
  char ch = (==[ch])

  -- matches one of patterns
  (|||) :: RegExp -> RegExp -> RegExp
  (|||) e1 e2 = \x -> e1 x || e2 x

  -- matches first and second part of string to patterns
  concatRegExp :: RegExp -> RegExp -> RegExp
  concatRegExp e1 e2 = \x -> or [e1 y && e2 z | (y, z) <- splits x]

  -- matches string that is empty or of N occurences
  star :: RegExp -> RegExp
  star p = epsilon ||| (concatRegExp p (star p))

  -- matches zero or one occurences of pattern P
  option :: RegExp -> RegExp
  option p = epsilon ||| p

  -- matches one or more occurences of pattern P
  plus :: RegExp -> RegExp
  plus p = p ||| concatRegExp p (plus p)

  -- 4th Result
  data MyResult a = OK a | Error String
    deriving Show

  isMyFavoriteNumber :: Integral a => a -> MyResult Bool
  isMyFavoriteNumber num
    | num == 42 = OK True
    | otherwise = Error "Not my favorite number"
  
  isOK :: Bool -> MyResult String
  isOK b
    | b == True = OK "Good"
    | otherwise = Error "Not Good"

  -- unwraps MyResult like a Monad
  bindMyResult :: MyResult a -> (a -> MyResult b) -> MyResult b
  bindMyResult (Error msg) _ = Error msg 
  bindMyResult (OK val) f = f val

  composeMyResult :: (a -> MyResult b) -> (b -> MyResult c) -> (a -> MyResult c)
  composeMyResult f g newFuncArg = bindMyResult (f newFuncArg) g

  testCompose = composeMyResult isMyFavoriteNumber isOK

  tc1 = testCompose 13
  tc2 = testCompose 14
--
-- 5th Goldbach conjecture
  primes :: [Integer]
  primes = sieve [2..]

  sieve (x:xs) =
    x : sieve [y | y <- xs, y `mod` x > 0]

  goldbach :: Integer -> Bool
  goldbach n = length allEvens == length primePairsForEvens
    where 
      allEvens = getEven [4..n]
      primePairsForEvens = [ findPairForTarget en (getPrimeSums [] (getNumPrimes en)) | en <- allEvens ]

  isEven num = num `mod` 2 == 0

  getEven :: [Integer] -> [Integer]
  getEven [] = []
  getEven nums = filter isEven nums

  getNumPrimes :: Integer -> [Integer]
  getNumPrimes n = takeWhile (<n) primes

  getPrimeSums :: [(Integer, Integer)] -> [Integer] -> [(Integer, Integer)]
  getPrimeSums acc [] = acc
  getPrimeSums acc (x:xs) = getPrimeSums (acc ++ map (\y -> (y, x)) (x:xs)) xs

  findPairForTarget :: Integer -> [(Integer, Integer)] -> (Integer, Integer)
  findPairForTarget _ [] = (0, 0)  -- No pair found
  findPairForTarget target ((x, y):pairs)
    | x + y == target = (x, y)
    | otherwise       = findPairForTarget target pairs

  -- 6th Stream
  data Stream a = Cons a (Stream a)

  ones :: Stream Int
  ones = Cons 1 (ones)

  -- to list
  streamToList :: Stream a -> [a]
  streamToList (Cons x xs) = x : streamToList xs

  -- iterate with function
  streamIterate :: (a -> a) -> a -> Stream a
  streamIterate f x = Cons x (streamIterate f (f x))

  -- interleaving of stream elements
  streamInterleave :: Stream a -> Stream a -> Stream a
  streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

  evens = streamIterate (+2) 0
  odds = streamIterate (+2) 1
  interleaved = take 100 (streamToList (streamInterleave evens odds))

--
