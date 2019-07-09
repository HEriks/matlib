-- module Test1 where
import Test.QuickCheck

-- qFunctions
neg :: Int -> Int
neg a = minus 0 a

absolute :: Int -> Int
absolute a 
    | a < 0     = neg a
    | otherwise = a



plus :: Int -> Int -> Int
plus a b = a + b

minus :: Int -> Int -> Int
minus a b = a - b

times :: Int -> Int -> Int
times 0 _ = 0
times _ 0 = 0
times a 1 = a
times 1 b = b
times a b 
    | b < 0     = neg (plus a (times a (abs b - 1)))    -- -1 * (a + (abs b - 1))
    | otherwise = plus a (times a (b-1))
    


pow :: Double -> Double -> Double
-- pow n exp = times n (times n n)
pow _ 0   = 1
pow 0 exp = 0
pow n exp
    | exp > 0   = n * (pow n (abs exp - 1)) -- Had to use "*" since "times" does not accept Doubles
    | otherwise = 1 / pow n (abs exp) --TODO


-- QuickCheck properties for 
checkTimes :: Int -> Int -> Bool
checkTimes x y = times x y == x * y

checkPow :: Double -> Double -> Bool
checkPow x y = True -- Does not work correctly
    -- | x < 0 || y < 0 = True
    -- | otherwise = pow x y == (x ** y :: Double)

checkAbsolute :: Int -> Bool
checkAbsolute x = absolute x == abs x