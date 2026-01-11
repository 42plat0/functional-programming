module ExamPrep where
  
  sign :: Integer -> Integer
  sign 0 = 0
  sign x 
    | x < 0 = -1
    | otherwise = 1

  safeHead :: [a] -> Maybe a
  safeHead [] = Nothing
  safeHead (x:xs) = Just x

  -- that returns "equilateral", "isosceles", "scalene", or "invalid"
  triangleType :: Int -> Int -> Int -> String
  triangleType x y z 
    | not valid = "invalid"
    | xEqY && yEqZ = "equilateral"
    | xEqY || yEqZ || xEqZ = "isosceles"
    | not xEqY && not xEqZ && not yEqZ = "scalene" 
    where
      xEqY = x == y
      xEqZ = x == z
      yEqZ = y == z
      valid = x+y > z && x+z > y && z+y > x


  f x 
      | xPow + 1 > 10 = xPow + 1
      | otherwise = x - 1
    where 
      xPow = x * x

  -- primitive recursion 
  power :: Integer -> Integer -> Integer
  power x 0 = 1
  power x 1 = x
  power x pow 
    | pow < 0 = error "not handling powers to the negative"
    | otherwise = x * (power x (pow - 1))

  sumTo :: Integer -> Integer
  sumTo n
    | n == 0 = n
    | n > 0 = n + sumTo(n - 1)
    | otherwise = error "no negative nums pls :)"

  length' :: [a] -> Integer
  length' [] = 0
  length' (_:xs) = 1 + length' xs

  countEven :: [Integer] -> Integer
  countEven [] = 0
  countEven (x:xs)
    | mod x 2 == 0 = 1 + countEven xs
    | otherwise = countEven xs

  isCountEvenOk = length' nums == countEven nums
    where
      nums = [2,4..100]
  
  -- list compr
  squareEvens :: [Integer] -> [Integer]
  squareEvens [] = []
  squareEvens xs = [x*x | x <- xs, even x]

  pairs :: [a] -> [(a, a)]
  pairs [] = []
  pairs (x:[]) = []
  pairs (x:y:xys) = [(x,y)] ++ pairs xys

  divisors :: Integer -> [Integer]
  divisors x = [y | y <- [1..x], mod x y == 0]

  removeDuplicates :: Eq a => [a] -> [a]
  removeDuplicates [] = []
  removeDuplicates (x:xs)
    | elem x xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs

  -- higher ord functions
  addOneToAll :: [Integer] -> [Integer]
  addOneToAll xs = map (+1) xs

  sumOfSquares :: [Integer] -> Integer
  sumOfSquares = foldr (+) 0 . map (\x -> x * x)

  countPositives :: [Int] -> Int
  countPositives = length . filter (>0) 

  applyTwice :: (a -> a) -> a -> a
  applyTwice f = f . f

  quadruple :: Int -> Int
  quadruple = (*4)


  -- defined dtypes
  data Shape = Circle Float | Rectangle Float Float | Triangle Float Float Float

  -- use Float as return instead of Integer since you'd need to round all return values to be able to pass errors. stupid GPT
  perimeter :: Shape -> Float
  perimeter (Circle r) = 2 * pi * r
  perimeter (Rectangle w h) = w * h
  perimeter (Triangle x y z) = x + y + z

  data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
    deriving Show
  
  treeSum :: Num a => Tree a -> a
  treeSum (Empty) = 0
  treeSum (Leaf x) = x
  treeSum (Node x t1 t2) = x + treeSum t1 + treeSum t2

  treeSize :: Tree a -> Int
  treeSize (Empty) = 0
  treeSize (Leaf _) = 1
  treeSize (Node _ t1 t2) = treeSize t1 + treeSize t2

  treeMap :: (a -> b) -> Tree a -> Tree b
  treeMap _ (Empty) = Empty
  treeMap f (Leaf x) = Leaf (f x)
  treeMap f (Node x t1 t2) = Node (f x) (treeMap f t1) (treeMap f t2)

  t2 :: Tree Int
  t2 = Node 3 (Leaf 4) (Leaf 5)
  t3 :: Tree Int
  t3 = Node 1
        (Node 2 (Leaf 3) (Leaf 4))
        (Leaf 5)
  t4 :: Tree Int
  t4 = Node 10
          (Node 5 (Leaf 1) (Node 2 (Leaf 3) (Leaf 4)))
          (Leaf 6)

  -- types
  class Describable a where
    describe :: a -> String

  instance Describable Bool where
      describe True  = "Yes"
      describe False = "No"
  
  instance Describable Shape where
    describe (Circle _)  = "Circle"
    describe (Rectangle _ _) = "Rectangle"
    describe (Triangle _ _ _)  = "Triangle"

  describeList :: Describable a => [a] -> [String]
  describeList  = map (describe)


  data Color = Red | Green | Blue
  class MyEq a where
    equalz :: a -> a -> Bool

  instance MyEq Integer where
    equalz x y = x == y

  instance MyEq Color where
    equalz Red Red = True
    equalz Green Green = True
    equalz Blue Blue = True
    equalz _ _ = False

  allEquals :: MyEq a => [a] -> Bool
  allEquals [] = True
  allEquals (x:xs) = all (equalz x) xs

  replicate' :: Integer -> a -> [a]
  replicate' num x
    | num > 0 = x : replicate' (num - 1) x
    | otherwise = []

  toInteger' :: String -> Integer
  toInteger' = read

  reverseInt :: Integer -> Integer
  reverseInt = toInteger' . reverse . show

  isPalindrome :: Eq a => [a] -> Bool
  isPalindrome [] = True
  isPalindrome  (x:[]) = True
  isPalindrome xs
    | head xs == last xs = isPalindrome (init $ tail xs)
    | otherwise = False

  isPrime :: Integer -> Bool
  isPrime n
    | n <= 1    = False
    | otherwise = null [ x | x <- [2 .. n-1], mod n x == 0 ]

  primes :: Integer -> [Integer]
  primes upTo = [x | x <- [2..upTo], isPrime x]


  cartesianProduct :: [a] -> [b] -> [(a,b)]
  cartesianProduct  xs ys = [(x, y) | x <- xs, y <- ys]

  positions :: Eq a => a -> [a] -> [Int] 
  positions x lst = [snd p | p <- pos, fst p == x]
    where
      pos = zip lst [0..(length lst) - 1]

--Exercise 1: Use filter and map together to write squarePositives :: [Int] -> [Int] that squares only the positive numbers in a list.
  squarePositives :: [Int] -> [Int]
  squarePositives = map (\x -> x^2) . filter(>0)
--Exercise 2: Implement myMaximum :: Ord a => [a] -> a using foldr1 or foldl1.
  myMaximum :: Ord a => [a] -> a
  myMaximum = foldr1 (\x y -> if x >= y then x else y) 
--Exercise 3: Write partition' :: (a -> Bool) -> [a] -> ([a], [a]) using foldr that splits a list into elements that satisfy a predicate and those that don't (returns a tuple of two lists).
  partition' :: (a -> Bool) -> [a] -> ([a], [a])
  partition' f xs = ((foldr (\x acc -> if f x == True then x : acc else acc) [] xs), (foldr (\x acc -> if f x == False then x : acc else acc) [] xs))
--Exercise 4: Create pipeline :: [a -> a] -> a -> a that takes a list of functions and composes them all together using foldr or foldl. Example: pipeline [(+1), (*2), (^2)] 3 should compute (((3^2)*2)+1).
  pipeline :: [a -> a] -> a -> a
  pipeline funcs x = foldr (id) x funcs
--  pipeline funcs x = foldr1 (.) funcs x
--
--Exercise 1: Define a Result e a datatype similar to Either but specifically for error handling, with constructors Error e and Success a. Write mapResult :: (a -> b) -> Result e a -> Result e b and isError :: Result e a -> Bool.
--
--
--Exercise 2: Create a Stack a datatype that wraps a list. Implement push :: a -> Stack a -> Stack a, pop :: Stack a -> Maybe (a, Stack a), and peek :: Stack a -> Maybe a. Make it an instance of Show.
  data Stack a = Stack [a]
    deriving Show

  push :: a -> Stack a -> Stack a
  push x (Stack xs) = Stack (xs ++ [x])

  pop :: Stack a -> Maybe (a, Stack a)
  pop (Stack []) = Nothing
  pop (Stack xs) = Just ((last xs), Stack (init xs))

  peek :: Stack a -> Maybe a
  peek (Stack []) = Nothing
  peek (Stack xs) = Just (head xs)
--
--Exercise 3: Define a Direction datatype with North | South | East | West. Write opposite :: Direction -> Direction and turnRight :: Direction -> Direction. Then create a Position datatype with x and y coordinates and write move :: Direction -> Int -> Position -> Position.
--Exercise 4: Create a JSON datatype that can represent: JNull, JBool Bool, JNumber Double, JString String, JArray [JSON], JObject [(String, JSON)]. Write stringify :: JSON -> String that converts it to a string representation.
