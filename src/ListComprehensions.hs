module ListComprehensions where

import Data.List

--------------------------------------------------------------------------------

-- Returns True iff the first argument can be divided by the second with no remainder.
dividesBy :: Integer -> Integer -> Bool
dividesBy a b = mod a b == 0


-- Whether or not the argument is prime.
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (or [dividesBy n x | x <- [2 .. floor (sqrt (fromIntegral n))]])


-- The infinite list of all prime numbers.
primes :: [Integer]
primes = filter isPrime [x | x <- [2 ..]]


-- All possible outcomes of rolling two die, with number of sides m and n, including duplicates.
rollOutcomes :: Int -> Int -> [Int]
rollOutcomes = error "Not implemented"


-- The Cartesian product of two lists.
cartProd :: [a] -> [b] -> [(a,b)]
cartProd = error "Not implemented"


-- Given a function and two lists, apply f to every pair of elements from the two lists.
cartProdWith :: (a -> b -> c) -> [a] -> [b] -> [c]
cartProdWith = error "Not implemented"


-- Implement rollOutcomes again, using the cartProdWith function defined above.
rollOutcomes' :: Int -> Int -> [Int]
rollOutcomes' = error "Not implemented"


-- The full lowercase alphabet.
letters :: [Char]
letters = error "Not implemented"


-- The five lowercase vowels.
vowels :: [Char]
vowels = error "Not implemented"


-- The twenty-one lowercase consonants.
consonants :: [Char]
consonants = error "Not implemented"


-- A function which gives back "FizzBuzz" if the number is a multiple of 3 and 5; "Fizz" if only a multiple of 3; "Buzz" if only a multiple of 5; and the number as a string otherwise.
fb :: Integer -> String
fb = error "Not implemented"

-- Using `map`, compute the full FizzBuzz from 1 up to the argument given.
fizzbuzzTo :: Integer -> [String]
fizzbuzzTo = error "Not implemented"


-- How many times is "Fizz" printed between 1 and 1000?
howManyFizzes :: Int
howManyFizzes = error "Not implemented"