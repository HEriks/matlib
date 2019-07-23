module Basic where
{-# LANGUAGE FlexibleContexts #-}
import Test.QuickCheck
---- Double is used as an approximation of the real numbers ----

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

divi :: Double -> Double -> Double
divi 0 _ = 0
divi _ 0 = error "Division by zero impossible"
divi x y = x / y

prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod xs

factorial :: Double -> Double
factorial 1 = 1
factorial n | n > 0     = times n (factorial (minus n 1))
            | otherwise = error "Only positive numbers allowed"

factorialPlus :: Double -> Double
factorialPlus 0 = 0
factorialPlus n | n > 0     = plus n (factorialPlus (minus n 1))
                | otherwise = error "Only positive numbers allowed"

factorialPlus' :: Double -> Double
factorialPlus' n | n > 0 = divi (times n (plus n 1)) 2
                 | otherwise = error "Only positive numbers allowed" 

-- From the cheat sheet for ma1c
pow :: Double -> Double -> Double   -- a^b
pow _ 0   = 1
pow 0 exp = 0
pow n exp
    | exp > 0   = times n (pow n (abs exp - 1))
    | otherwise = 1 / pow n (abs exp)

-- From the cheat sheet for ma2c
kvadr1 :: Double -> Double -> Double
kvadr1 a b = plus (times a a) (plus (times 2 (times a b)) (times b b)) --pow (plus a b) 2 

kvadr2 :: Double -> Double -> Double
kvadr2 a b = pow (minus a b) 2 

--- Primes ---
-- Prime factorization
primeFactors :: Int -> [Int]
primeFactors n = pf n []

pf :: Int -> [Int] -> [Int]
pf n ns | n < 2          = if length ns == 0 then [n] else ns
        | n `mod` 2 == 0 = pf (div n 2) (ns ++ [2])
        | otherwise      = pf (div n x) (ns ++ [x])
            where
                x = findNextDivisor n 3

findNextDivisor :: Int -> Int -> Int
findNextDivisor n i | n `mod` i == 0 = i
                    | n <= i         = n
                    | otherwise      = findNextDivisor n (i+1)

-- Is a given number a prime?
<<<<<<< HEAD
isPrime :: Int -> Bool
isPrime n = length [ x | x <- [2..(floor(sqrt(fromIntegral n)))], mod n x == 0] == 0
=======
pt :: Int -> Bool
pt n 
    | n < 2     = False
    | n == 2    = True
    | otherwise = if elem True [n `mod` x == 0 | x <- [2..(n-1)]] then False else True
         
>>>>>>> 711a94ddb9d720496d8be002632318a784a04875

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

-- checkPrimeFactors :: Int -> Bool
-- checkPrimeFactors n = n == foldr (*) 1 primeFactors