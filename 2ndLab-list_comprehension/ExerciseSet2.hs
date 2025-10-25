module ExerciseSet2  where
  import Test.QuickCheck
  import Data.List -- for isPrefixOf
  import Data.Char(isDigit, toUpper)
  import Data.Ord
  import qualified Data.Map.Strict as Map

  myFilter :: (a -> Bool) -> [a] -> [a]
  myFilter p xs = [x | x <- xs, p x]

  sumList :: [Float] -> Float
  sumList [] = 0
  sumList (x:xs) = x + sumList xs

  -- 1st. average
  average :: [Float] -> Float
  average [] = 0
  average xs = (sumList xs) / fromIntegral (length xs)

  -- 2nd. divides
  -- recursion
  divideHelper :: Integer -> [Integer] -> [Integer]
  divideHelper 0 _ = []
  divideHelper _ [] = []
  divideHelper num (x:xs)
    | mod num x == 0 = (x : divideHelper num xs)
    | otherwise = divideHelper num xs

  divides :: Integer -> [Integer]
  divides x = divideHelper x [2..(x - 1)]

  -- comprehension
  dividesCompr :: Integer -> [Integer]
  dividesCompr num = [x |x <- [2..(num - 1)], mod num x == 0]

  -- primes
  isPrime :: Integer -> Bool
  isPrime 0 = False
  isPrime 1 = False
  isPrime x 
    | x > 0 = length (divides x) < 1
    | otherwise = error "Number should be positive natural"

  getPrimesUpTo :: Integer -> [Integer]
  getPrimesUpTo num = [x | x <- [1..num], isPrime x]

  --3rd. prefix 
  prefix :: String -> String -> Bool
  prefix [] _ = True 
  prefix _ [] = False
  prefix (x:xs) (y:ys) 
    | x == y = prefix xs ys
    | otherwise = False

  prop_prefix :: String -> String -> Bool
  prop_prefix xs ys = 
    prefix xs ys == isPrefixOf xs ys

  substring :: String -> String -> Bool
  substring [] _ = False
  substring _ [] = False
  substring xs (y:ys)
    | prefix xs (y:ys) == True = True
    | otherwise = substring xs ys

  --4th permut
  -- how my permut function works
  -- gets list of tuples for each element in a list in form of (Integer, numberOfOccurences)
  -- then creates a set of all integers appearing in both lists 
  -- goes through each element in a set and gets their count in both tuples
  -- if count is the same, it goes again with other elements
  -- otherwise it returns false
  permut :: [Integer] -> [Integer] -> Bool
  permut [] [] = True
  permut [] _ = False
  permut _ [] = False
  permut (x:xs) (y:ys)
    | length (x:xs) /= length (y:ys) = False
    | otherwise = checkPermutations valSet firstTuple secondTuple
    where
      valSet = nub (nub (x:xs) ++ nub(y:ys)) 
      firstTuple = listOfElemOccurenceTuples (x:xs)
      secondTuple = listOfElemOccurenceTuples (y:ys)

  -- For each Integer in list, generates tuple with that element and number of occurences in a list
  listOfElemOccurenceTuples :: [Integer] -> [(Integer, Integer)]
  listOfElemOccurenceTuples xs = [(x, numberOfOccurences x xs) | x <- nub xs]

  -- gets count of times element appears in a list
  numberOfOccurences :: Integer -> [Integer] -> Integer
  numberOfOccurences x ys = fromIntegral (length (myFilter (==x) ys))

  -- gets count from tuple (element, count). If doesn't find, returns 0
  countOfElem :: Integer -> [(Integer, Integer)] -> Integer
  countOfElem _ [] = 0
  countOfElem x ((elem, count):ys)
    | x == elem = count
    | otherwise = countOfElem x ys

  -- goes through set of elements appearing in both lists 
  -- and checks if it appears the same amount of times in both lists of tuples 
  -- first argument is set of all values appearing in both lists
  checkPermutations :: [Integer] -> [(Integer, Integer)] -> [(Integer, Integer)] -> Bool
  checkPermutations [] _ _ = True
  checkPermutations (x:xs) (ys) (zs)
    | countOfElem x ys == countOfElem x zs = checkPermutations xs ys zs
    | otherwise = False

  -- start DEFINE PERMUT FUNCTION FOR PROP 
  -- count how many times an element occurs in a list
  count :: Integer -> [Integer] -> Int
  count x = length . filter (==x)

  -- check if two lists are permutations
  arePermutations :: [Integer] -> [Integer] -> Bool
  arePermutations a b = all (\x -> count x a == count x b) (nub (a ++ b))
  -- end DEFINE PERMUT FUNCTION FOR PROP 

  -- permut prop
  prop_permut :: [Integer] -> [Integer] -> Bool
  prop_permut xs ys =
    permut xs ys == arePermutations xs ys
   
  --5th capitalise
  capitalise :: String -> String
  capitalise [] = []
  capitalise xs = [toUpper x | x <- xs, isDigit x == False]

  --6th
  -- util, data
  toFloat x = fromRational x :: Float
  shopBasket = [("obuolys", toFloat 0.5), ("obuolys", toFloat 1.5), ("bananas", toFloat 10), ("bananas", toFloat 10)]
  --6.1 itemTotal
  itemTotal :: [(String, Float)] -> [(String, Float)]
  itemTotal = Map.toList . Map.fromListWith (+)

  -- 6.2 item discount
  itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
  itemDiscount _ _ [] = []
  itemDiscount key discount ((x, y):xs)
    | x == key = (x, (y * discount_prc)) : itemDiscount key discount xs
    | otherwise = (x, y) : itemDiscount key discount xs
    where
      discount_prc = fromIntegral(discount) / 100
