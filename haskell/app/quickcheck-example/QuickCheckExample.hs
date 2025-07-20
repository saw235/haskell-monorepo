module Main where

import Data.List (reverse, sort)
import System.Random (randomRIO)
import System.Exit (exitFailure)

-- =============================================================================
-- SIMPLE QUICKCHECK-STYLE PROPERTY TESTING (MANUAL IMPLEMENTATION)
-- =============================================================================

-- This is a simplified version of QuickCheck that demonstrates the core concepts
-- without requiring the actual QuickCheck library

-- Property: A function that should always return True
type Property = Bool

-- Test configuration
data TestConfig = TestConfig
  { numTests :: Int,
    maxShrinks :: Int
  }

defaultConfig :: TestConfig
defaultConfig = TestConfig 100 10

-- Simple random number generator for testing
randomInt :: (Int, Int) -> IO Int
randomInt (min, max) = randomRIO (min, max)

randomList :: Int -> IO [Int]
randomList len = sequence [randomInt (-100, 100) | _ <- [1 .. len]]

-- Test runner that generates random inputs and returns success/failure
quickCheck :: String -> (Int -> IO Bool) -> IO Bool
quickCheck name prop = do
  putStrLn $ "Testing: " ++ name
  results <- sequence [prop i | i <- [1 .. 100]]
  let passed = length (filter id results)
  let failed = length (filter not results)
  putStrLn $ "  Passed: " ++ show passed ++ " tests"
  putStrLn $ "  Failed: " ++ show failed ++ " tests"
  if failed == 0
    then do
      putStrLn $ "  PASS: " ++ name ++ " passed all tests!"
      return True
    else do
      putStrLn $ "  FAIL: " ++ name ++ " failed " ++ show failed ++ " tests!"
      return False

-- =============================================================================
-- PROPERTY DEFINITIONS
-- =============================================================================

-- Property 1: Addition is commutative
-- This should always be true: a + b = b + a
prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative a b = a + b == b + a

-- Property 2: Addition is associative
-- This should always be true: (a + b) + c = a + (b + c)
prop_addition_associative :: Int -> Int -> Int -> Bool
prop_addition_associative a b c = (a + b) + c == a + (b + c)

-- Property 3: Zero is the identity for addition
-- This should always be true: a + 0 = a
prop_zero_identity :: Int -> Bool
prop_zero_identity a = a + 0 == a

-- Property 4: List reversal is its own inverse
-- This should always be true: reverse (reverse xs) = xs
prop_reverse_inverse :: [Int] -> Bool
prop_reverse_inverse xs = reverse (reverse xs) == xs

-- Property 5: Sorting a list twice is the same as sorting it once
-- This should always be true: sort (sort xs) = sort xs
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

-- Property 6: Length of concatenated lists equals sum of lengths
-- This should always be true: length (xs ++ ys) = length xs + length ys
prop_concat_length :: [Int] -> [Int] -> Bool
prop_concat_length xs ys = length (xs ++ ys) == length xs + length ys

-- Property 7: Absolute value is always non-negative
-- This should always be true: abs x >= 0
prop_abs_nonnegative :: Int -> Bool
prop_abs_nonnegative x = abs x >= 0

-- Property 8: Absolute value of negative number is positive
-- This should always be true: if x < 0 then abs x > 0 else abs x >= 0
prop_abs_negative_positive :: Int -> Bool
prop_abs_negative_positive x = if x < 0 then abs x > 0 else abs x >= 0

-- =============================================================================
-- PROPERTY THAT WILL FAIL (FOR DEMONSTRATION)
-- =============================================================================

-- This property is FALSE - it will fail for some inputs
-- QuickCheck would find the smallest counterexample
prop_false_property :: Int -> Bool
prop_false_property x = x * x >= x -- This fails for x = 0

-- =============================================================================
-- TEST GENERATORS
-- =============================================================================

-- Generate random test data for our properties
generateAdditionTest :: Int -> IO Bool
generateAdditionTest _ = do
  a <- randomInt (-1000, 1000)
  b <- randomInt (-1000, 1000)
  return $ prop_addition_commutative a b

generateAssociativeTest :: Int -> IO Bool
generateAssociativeTest _ = do
  a <- randomInt (-1000, 1000)
  b <- randomInt (-1000, 1000)
  c <- randomInt (-1000, 1000)
  return $ prop_addition_associative a b c

generateZeroIdentityTest :: Int -> IO Bool
generateZeroIdentityTest _ = do
  a <- randomInt (-1000, 1000)
  return $ prop_zero_identity a

generateReverseTest :: Int -> IO Bool
generateReverseTest _ = do
  len <- randomInt (0, 20)
  xs <- randomList len
  return $ prop_reverse_inverse xs

generateSortTest :: Int -> IO Bool
generateSortTest _ = do
  len <- randomInt (0, 20)
  xs <- randomList len
  return $ prop_sort_idempotent xs

generateConcatTest :: Int -> IO Bool
generateConcatTest _ = do
  len1 <- randomInt (0, 10)
  len2 <- randomInt (0, 10)
  xs <- randomList len1
  ys <- randomList len2
  return $ prop_concat_length xs ys

generateAbsTest :: Int -> IO Bool
generateAbsTest _ = do
  x <- randomInt (-1000, 1000)
  return $ prop_abs_nonnegative x

generateAbsNegativeTest :: Int -> IO Bool
generateAbsNegativeTest _ = do
  x <- randomInt (-1000, 1000)
  return $ prop_abs_negative_positive x

generateFalseTest :: Int -> IO Bool
generateFalseTest _ = do
  x <- randomInt (-100, 100)
  return $ prop_false_property x

-- =============================================================================
-- SHRINKING EXAMPLE (SIMPLIFIED)
-- =============================================================================

-- When a test fails, try to find a smaller counterexample
shrinkInt :: Int -> [Int]
shrinkInt x
  | x > 0 = [0, x `div` 2, x - 1]
  | x < 0 = [0, x `div` 2, x + 1]
  | otherwise = []

findSmallestCounterexample :: (Int -> Bool) -> Int -> IO (Maybe Int)
findSmallestCounterexample prop x = do
  let candidates = x : shrinkInt x
  let failures = filter (not . prop) candidates
  case failures of
    [] -> return Nothing
    (smallest : _) -> return (Just smallest)

-- =============================================================================
-- MAIN FUNCTION
-- =============================================================================

main :: IO ()
main = do
  putStrLn "=== QuickCheck-Style Property Testing Examples ==="
  putStrLn "This demonstrates the core concepts of property-based testing"
  putStrLn ""

  putStrLn "1. Testing addition commutativity..."
  test1 <- quickCheck "Addition is commutative" generateAdditionTest
  
  putStrLn "\n2. Testing addition associativity..."
  test2 <- quickCheck "Addition is associative" generateAssociativeTest
  
  putStrLn "\n3. Testing zero identity..."
  test3 <- quickCheck "Zero is identity for addition" generateZeroIdentityTest
  
  putStrLn "\n4. Testing list reverse inverse..."
  test4 <- quickCheck "Reverse is its own inverse" generateReverseTest
  
  putStrLn "\n5. Testing sort idempotence..."
  test5 <- quickCheck "Sort is idempotent" generateSortTest
  
  putStrLn "\n6. Testing concatenation length..."
  test6 <- quickCheck "Concatenation length property" generateConcatTest
  
  putStrLn "\n7. Testing absolute value non-negative..."
  test7 <- quickCheck "Absolute value is non-negative" generateAbsTest
  
  putStrLn "\n8. Testing absolute value of negative numbers..."
  test8 <- quickCheck "Absolute value of negative is positive" generateAbsNegativeTest
  
  putStrLn "\n9. Testing a property that will fail (demonstrating shrinking)..."
  test9 <- quickCheck "False property (should fail)" generateFalseTest
  
  putStrLn "\n10. Demonstrating shrinking..."
  putStrLn "Finding smallest counterexample for false property:"
  counterexample <- findSmallestCounterexample prop_false_property 5
  case counterexample of
    Just x -> putStrLn $ "  Smallest counterexample: " ++ show x
    Nothing -> putStrLn "  No counterexample found"

  putStrLn "\n=== QuickCheck Examples Completed ==="
  putStrLn ""
  putStrLn "Key Concepts Demonstrated:"
  putStrLn "1. Properties: Boolean functions that should always be true"
  putStrLn "2. Test Generation: Random input generation"
  putStrLn "3. Test Execution: Running properties with many inputs"
  putStrLn "4. Shrinking: Finding smaller counterexamples when tests fail"
  putStrLn "5. Reporting: Clear feedback on test results"
  
  -- Exit with appropriate code based on test results
  let allTestsPassed = test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9
  if allTestsPassed
    then do
      putStrLn "\nPASS: All tests passed!"
      return ()
    else do
      putStrLn "\nFAIL: Some tests failed!"
      System.Exit.exitFailure 
