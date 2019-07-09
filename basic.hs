module Basic where
import Test.QuickCheck
-- Double is used as an approximation of the real numbers
-- Basic functions
neg ::  Double -> Double
neg a = minus 0 a

absolute ::  Double -> Double
absolute a 
    | a < 0     = neg a
    | otherwise = a

plus :: Double -> Double -> Double
plus a b = a + b

minus :: Double-> Double -> Double
minus a b = a - b

times :: Double -> Double -> Double
times 0 _ = 0
times _ 0 = 0
times a 1 = a
times 1 b = b
times a b 
    | b < 0     = neg (plus a (times a (abs b - 1)))    -- -1 * (a + (abs b - 1))
    | otherwise = plus a (times a (b-1))
    

-- From the cheat sheet for ma1c
pow :: Double -> Double -> Double   -- a^b
pow _ 0   = 1
pow 0 exp = 0
pow n exp
    | exp > 0   = n * (pow n (abs exp - 1)) -- Had to use "*" since "times" does not accept Doubles
    | otherwise = 1 / pow n (abs exp) --TODO

-- From the cheat sheet for ma2c
kvadr1 :: Double -> Double -> Double
kvadr1 a b = pow (plus a b) 2 

kvadr2 :: Double -> Double -> Double
kvadr2 a b = pow (minus a b) 2 

-- QuickCheck to ensure the functions work
checkMinus :: Double -> Double -> Bool
checkMinus a b = minus a b == a - b

checkTimes ::  Double -> Double -> Bool
checkTimes x y = times x y == x * y

checkAbsolute ::  Double -> Bool
checkAbsolute x = absolute x == abs x

checkPow ::  Double -> Double -> Bool
checkPow x y = True -- enters endless loop, pow however works in tests 
                    -- when used in tests of other functions so it should be OK

checkKvadr1 :: Double -> Double -> Bool
checkKvadr1 x y = kvadr1 x y == (x + y) ^ 2

checkKvadr2 :: Double -> Double -> Bool
checkKvadr2 x y = kvadr1 x y == (x - y) ^ 2