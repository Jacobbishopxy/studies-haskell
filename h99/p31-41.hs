-- file: p31-41.hs
-- author: Jacob Xie
-- date: 2024/01/11 23:45:51 Thursday
-- brief:

import Data.List (group, union)

-- P31: Determine whether a given integer number is prime
-- ans: https://wiki.haskell.org/99_questions/Solutions/31
myIsPrime :: (Integral a) => a -> Bool
myIsPrime k = k > 1 && foldr (\p r -> p * p > k || k `rem` p /= 0 && r) True primesTME

primesTME :: (Integral a) => [a]
primesTME = 2 : gaps 3 (join [[p * p, p * p + 2 * p ..] | p <- primes'])
  where
    primes' = 3 : gaps 5 (join [[p * p, p * p + 2 * p ..] | p <- primes'])
    join ((x : xs) : t) = x : union xs (join (pairs t))
    pairs ((x : xs) : ys : t) = (x : union xs ys) : pairs t
    gaps k xs@(x : t)
      | k == x = gaps (k + 2) t
      | otherwise = k : gaps (k + 2) xs

-- P32: Determine the greatest common divisor of two positive integer numbers
myGCD :: Integer -> Integer -> Integer
myGCD a b
  | b == 0 = abs a
  | otherwise = myGCD b $ a `mod` b

-- P33: Determine whether two positive integer numbers are coprime
myCoprime :: Integer -> Integer -> Bool
myCoprime a b = myGCD a b == 1

-- P34: Calculate Euler's totient function phi(m)
myTotient :: Int -> Int
myTotient 1 = 1
myTotient x = length $ takeWhile (<= x) primesTME

-- P35: Determine the prime factors of a given positive integer
myPrimeFactors :: Int -> [Int]
myPrimeFactors n = f n 2
  where
    f :: Int -> Int -> [Int]
    f 1 _ = []
    f n x
      | n `mod` x == 0 = x : f (n `div` x) x
      | otherwise = f n $ x + 1

-- P36: Determine the prime factors and their multiplicities of a given positive integer
myPrimeFactorsMult :: Int -> [(Int, Int)]
myPrimeFactorsMult = map encode . group . myPrimeFactors
  where
    encode xs = (head xs, length xs)

-- P37: Calculate Euler's totient function phi(m) (improved)

-- P38: Compare the two methods of calculating Euler's totient function

-- P39: A list of prime numbers in a give range

-- P40: Goldbach's conjecture

-- P41: A list of even numbers and their Goldbach compositions in a give range
