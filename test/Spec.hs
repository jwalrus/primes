import Test.QuickCheck
import Data.Either
import Primes

prop_validPrimesOnly val = if val < 2 || val >= maxN
                           then isLeft result
                           else isRight result
    where result = isPrime val

prop_primesArePrime val = if result == Right True
                          then length divisors == 0   
                          else True
    where result = isPrime val
          divisors = filter ((==0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val = if result == Right False
                                 then length divisors > 0
                                 else True
    where result = isPrime val
          divisors = filter ((==0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val = if (isLeft result)
                               then True
                               else product (fromRight [0] result) == val
    where result = primeFactors val

prop_allFactorsPrime val = if (isLeft result)
                           then True
                           else all (== Right True) resultsPrime
    where result = primeFactors val
          resultsPrime = map isPrime (fromRight [0] result)

main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_primesArePrime
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_nonPrimesAreComposite
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_factorsMakeOriginal
    quickCheckWith stdArgs { maxSuccess = 1000 } prop_allFactorsPrime
