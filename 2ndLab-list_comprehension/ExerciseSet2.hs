module ExerciseSet2  where
  import Test.QuickCheck
  import Data.List -- for isPrefixOf

  -- 1st. average
  sumList :: [Float] -> Float
  sumList [] = 0
  sumList (x:xs) = x + sumList xs

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


