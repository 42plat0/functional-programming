module ExerciseSet3 where
  import Test.QuickCheck
  
  -- TODO test -- not tested
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
  any0 f [] = error "Provide list with members"
  any0 f list = length (filter f list) > 0

  all0 :: (a->Bool) -> [a] -> Bool
  all0 f [] = error "Provide list with members"
  all0 f list = length (filter f list) == length list 

  -- MAP+FOLDR
  -- TODO
  --
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
  --  nuo 0 iki n kartu sumuoti funkcijos rezultatus
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
--  totalG1 (f) limit = foldr f 0 [0..limit]

  -- 7th 
  {- 
   Exercise 7. Define a function iter n f that composes the given function
    f ::a -> a with itself n ::Integer times, e.g., iter 2 f = f . f.
    Give two versions of this function: one based on recursion, and the one
    based on the idea of first creating (by using replicate) the list of n copies of
    f and then folding this list. For the cases when n ≤ 0, the Prelude function
    id, defined as id x = x, should be returned.
   -}
  
  -- 8th 
  {-
  which returns all the ways that a list can be split into two consecutive ones,
  e.g.,
  splits "Spy" == [("","Spy"),("S","py"),("Sp","y"),("Spy","")]
  -}
  splits :: [a] -> [([a],[a])]
  splits [] = [([], [])]
  


