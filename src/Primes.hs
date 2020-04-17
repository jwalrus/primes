module Primes where

data PrimeError = TooLarge | InvalidValue deriving Eq

instance Show PrimeError where
    show TooLarge = "Value exceeds max bound"
    show InvalidValue = "Value is not a valid candidate for prime checking"

maxN :: Int
maxN = 10000

primes :: [Int]
primes = sieve [2..maxN]

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2 = Left InvalidValue
          | n >= maxN = Left TooLarge
          | otherwise = Right (n `elem` primes)

displayResult :: Either PrimeError Bool -> String
displayResult (Right True) = "It is prime!"
displayResult (Right False) = "It is NOT prime :("
displayResult (Left primeError) = show primeError

unsafePrimeFactors :: Int -> [Int] -> [Int]
unsafePrimeFactors 0 _ = []
unsafePrimeFactors 1 _ = []
unsafePrimeFactors n [] = []
unsafePrimeFactors n (next:primes) = if n `mod` next == 0
                                     then next : unsafePrimeFactors (n `div` next) (next:primes)
                                     else unsafePrimeFactors n primes

primeFactors :: Int -> Either PrimeError [Int]
primeFactors n | n < 2 = Left InvalidValue
               | n >= maxN = Left TooLarge
               | otherwise = Right (unsafePrimeFactors n primesLessThanN)
    where primesLessThanN = filter (<= n) primes 
