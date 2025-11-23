module ExerciseSet3 where
  import Test.QuickCheck
  import Test.QuickCheck.Function  -- for Fun wrapper
  
  -- 1st overlaps
  -- Let's say PosXY defines center of shape in 2d plane
  data PosXY = PosXY Int Int 
    deriving (Show, Ord, Eq)

  getX :: PosXY -> Int
  getX (PosXY x _) = x

  getY :: PosXY -> Int
  getY (PosXY _ y) = y

  data Shape = Circle Float PosXY | Rectangle Float Float PosXY
    deriving (Show, Ord, Eq)

  getShapeX :: Shape -> Int
  getShapeX (Circle _ pos) = getX pos
  getShapeX (Rectangle _ _ pos) = getX pos

  getShapeY :: Shape -> Int
  getShapeY (Circle _ pos) = getY pos
  getShapeY (Rectangle _ _ pos) = getY pos

  getShapeBoundingX :: Shape -> (Float, Float)
  getShapeBoundingX (Circle r pos) = ((x - r), (x + r))
    where 
      x = fromIntegral (getX pos)
  getShapeBoundingX (Rectangle w _ pos) = ((x - w), (x + w))
    where 
      x = fromIntegral (getX pos)

  getShapeBoundingY :: Shape -> (Float, Float)
  getShapeBoundingY (Circle r pos) = ((y - r), (y + r))
    where 
      y = fromIntegral (getY pos)
  getShapeBoundingY (Rectangle _ h pos) = ((y - h), (y + h))
    where 
      y = fromIntegral (getY pos)

  overlaps :: Shape -> Shape -> Bool
  overlaps shape1 shape2 
    | fst(boundingX1) > fst(boundingX2) && fst(boundingX1) < snd(boundingX2) && fst(boundingY1) > fst(boundingY2) && fst(boundingY1) < snd(boundingY2) = True
    | snd(boundingX1) > fst(boundingX2) && snd(boundingX1) < snd(boundingX2) && snd(boundingY1) > fst(boundingY2) && snd(boundingY1) < snd(boundingY2) = True
    | otherwise = False
      where
        boundingX1 = getShapeBoundingX shape1
        boundingX2 = getShapeBoundingX shape2
        boundingY1 = getShapeBoundingY shape1
        boundingY2 = getShapeBoundingY shape2
      

  --2nd - any & and
  -- FILTER
  any0 :: (a->Bool) -> [a] -> Bool
  any0 f list = length (filter f list) > 0

  all0 :: (a->Bool) -> [a] -> Bool
  all0 f list = length (filter f list) == length list 

  -- MAP+FOLDR
  any1 :: (a->Bool) -> [a] -> Bool
  any1 f = foldr (||) False . map f

  all1 :: (a->Bool) -> [a] -> Bool
  all1 f = foldr (&&) True . map f

  --prop for all any any
  prop_any :: Fun Int Bool -> [Int] -> Bool
  prop_any (Fun _ fun) list =
    any0 fun list == any1 fun list

  prop_all :: Fun Int Bool -> [Int] -> Bool
  prop_all (Fun _ fun) list =
    all0 fun list == all1 fun list

  -- 3rd unzip with foldr
  unzip0 :: [(a, b)] -> ([a], [b])
  unzip0 list = foldr (\(x, y) (xs,ys) -> (x:xs, y:ys)) ([], []) list

--- UTILS
  sumList :: Num a => [a] -> a
  sumList [] = 0
  sumList (x:xs) = x + sumList xs

  average :: [Float] -> Float
  average [] = 0
  average xs = (sumList xs) / fromIntegral (length xs)

  -- 4th length
  -- using MAP
  lengthG0 :: [a] -> Int
  lengthG0 = sumList . map (\x -> 1)
  -- using FOLDR
  lengthG1 :: [a] -> Int
  lengthG1 = foldr (\_ y -> y + 1) 0

  prop_length :: [a] -> Bool
  prop_length xs =
    length xs == lengthG0 xs && length xs == lengthG1 xs

  -- 5th ff
  sumUntil :: Integer -> Integer -> [Integer] -> Integer
  sumUntil _ acc [] = acc
  sumUntil limit acc (x:xs) 
    | (acc + x) > limit = acc
    | otherwise = sumUntil limit (acc + x) xs
  --
  ff :: Integer -> [Integer] -> Integer
  ff _ [] = 0
  ff bound list = sumUntil bound 0 . map (*10) . filter (>0) $ list

  --
  -- 6th total
  --
  -- nuo 0 iki n kartu sumuoti funkcijos rezultatus
  -- tai funkcija total (pridekViena) 9 = 0 pridekViena + 1 pridekViena + 2 pridekViena ... n pridekViena
  -- tai jei sufoldintume lista gautume rezultata ez
  -- o gaut lista gallim [0..n]
  -- kiekvienam listo itemui darom transformacija, gauta lista sufoldinam
  --
  -- mappinant tas pats, 
  --
  --
  totalG :: (Integer -> Integer) -> Integer -> Integer
  totalG f limit = sumList . map f $ toLimitNumbers
  -- or
  -- total f limit = sumList (map f toLimitNumbers)
    where
      toLimitNumbers = [0..limit]

-- Unfinished foldr implementation
--  totalG1 :: (Integer -> Integer) -> Integer -> Integer
 -- totalG1 f limit = foldr f 0 [0..limit]

  -- 7th 
  {- 
   Exercise 7. Define a function iter n f that composes the given function
    f ::a -> a with itself n ::Integer times, e.g., iter 2 f = f . f.
    Give two versions of this function: one based on recursion, and the one
    based on the idea of first creating (by using replicate) the list of n copies of
    f and then folding this list. For the cases when n ≤ 0, the Prelude function
    id, defined as id x = x, should be returned.
   -}
  iterRec :: Int -> (a -> a) -> (a -> a)
  iterRec n f
    | n > 0 = (f . iterRec (n-1) f)
    | otherwise = id 

  -- with replicate and folding
  iterRep :: Int -> (a -> a) -> [a -> a]
  iterRep n f = replicate n f

  -- id is the identity element of function composition
  -- f . id = f
  -- id . f = f
  -- so a way to return a function
  compose :: [a -> a] -> a -> a
  compose funcs v = foldl (.) id funcs $ v

  iter :: Int -> (a -> a) -> a -> a
  iter n func x = compose (iterRep n func) x

  -- usage: iter [N-times] [FUNCTION] [VALUE]
  incrementedBy3nTimes n val = iter n (+3) val
  -- ghci> incrementedBy3nTimes 2 1
  -- 7

  -- using suggested quickCheck Function specific function
  -- to handle function failing and print counter example
  -- prop for both iter versions
  prop_iter :: Int -> Fun Int Int -> Int -> Bool
  prop_iter n (Fun _ func) x = 
    (iterRec n func) x == (iter n func) x


  -- 8th splits
  splits :: [a] -> [([a], [a])]
  splits list = splitsAccumulator list [] 0 

  splitsAccumulator :: [a] -> [([a], [a])] -> Int -> [([a], [a])]
  splitsAccumulator list acc idx
    | splitSndLength > 0 = splitsAccumulator list (acc ++ [splitVal]) (idx+1)
    | otherwise = acc ++ [splitVal]
    where
      splitVal = splitAt idx list
      splitSndLength = length $ snd $ splitVal



