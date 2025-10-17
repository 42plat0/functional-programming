module ExerciseSet1 where
    import Test.QuickCheck
    
    -- 1st
    nAnd :: Bool -> Bool -> Bool
    nAnd x y = not(x && y)

    nAnd_1 :: Bool -> Bool -> Bool
    nAnd_1 True x = not x
    nAnd_1 False _ = True 

    nAnd_2 :: Bool -> Bool -> Bool
    nAnd_2 True True = False
    nAnd_2 True False = True
    nAnd_2 False True = True
    nAnd_2 False False = True

    prop_nAnds x y = 
        nAnd x y == nAnd_1 x y 
        && nAnd_1 x y == nAnd_2 x y

    -- 2nd
    nDigits :: Integer -> Int
    nDigits n 
        | n < 0 = length (show ((*) n (-1)))
        | otherwise = length (show n)
    
    prop_nDigits n = 
        nDigits n == length (show (abs n))

    --3rd quadratic
    nRoots :: Float -> Float -> Float -> Int
    nRoots a b c
        | a == 0 = error "the first argument should be non-zero!"
        | b ^ 2 > 4.0 * a * c = 2
        | b ^ 2 == 4.0 * a * c = 1
        | b ^ 2 < 4.0 * a * c = 0

    prop_nRootsNonZero a b c =
        a /= 0 ==> nRoots a b c >= 0 && nRoots a b c <= 2
    
    --4th larger-smaller root
    smallerRoot :: Float -> Float -> Float -> Float
    smallerRoot a b c 
        | nRoots a b c == 2 = min quadraticPos quadraticNeg
        | nRoots a b c == 1 = -b / doubleA
        | nRoots a b c == 0 = error "no solution"
        where 
            doubleA = 2 * a
            quadraticPos = (-b + sqrt(b^2 - 4 * a * c)) / doubleA
            quadraticNeg = (-b - sqrt(b^2 - 4 * a * c)) / doubleA

    largerRoot :: Float -> Float -> Float -> Float
    largerRoot a b c
        | nRoots a b c == 2 = max quadraticPos quadraticNeg
        | nRoots a b c <= 1 = smallerRoot a b c
        where 
            doubleA = 2 * a
            quadraticPos = (-b + sqrt(b^2 - 4 * a * c)) / doubleA
            quadraticNeg = (-b - sqrt(b^2 - 4 * a * c)) / doubleA

    -- TODO write prop for this 
    -- without failling on case i dont wanna check
    -- prop_roots a b c =
    --     a /= 0.0 ==> smallerRoot a b c < largerRoot a b c 

    -- 5th recursion power2
    power2 :: Integer -> Integer
    power2 n
        | n == 0 = 1
        | n < 0 = error "Negative exponent"
        | otherwise = 2 * power2 (n - 1)
    
    prop_power2 :: Integer -> Property
    prop_power2 n =
        n > 0 ==> power2 n == 2 ^ n
    
    --6th mult addition
    mult :: Integer -> Integer -> Integer
    mult m n
        | n == 0 = 0
        | n < 0 = -m + mult m (n + 1)
        | otherwise = m + mult m (n - 1)
    
    prop_mult :: Integer -> Integer -> Bool
    prop_mult m n =
        mult m n == m * n
    
    --7th product function
    prod :: Integer -> Integer -> Integer
    prod from to 
        | to == from = from
        | to < from = 1
        | otherwise = mult from (prod (from + 1) to)
    
    -- refactored fact as special case of prod
    fact :: Integer -> Integer
    fact n = prod 1 n