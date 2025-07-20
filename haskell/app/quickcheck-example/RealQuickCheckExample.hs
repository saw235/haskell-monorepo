module Main where

import Test.QuickCheck
import Data.List (sort, reverse, nub)
import Control.Monad (when)

-- =============================================================================
-- REAL QUICKCHECK EXAMPLES
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
-- CONDITIONAL PROPERTIES
-- =============================================================================

-- Property that only holds under certain conditions
-- This should be true: if x > 0 and y > 0, then x + y > 0
prop_positive_sum :: Int -> Int -> Property
prop_positive_sum x y = 
  x > 0 && y > 0 ==> x + y > 0

-- Property with more complex conditions
-- This should be true: if xs is sorted, then sort xs == xs
prop_sorted_unchanged :: [Int] -> Property
prop_sorted_unchanged xs = 
  isSorted xs ==> sort xs == xs
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:ys) = x <= y && isSorted (y:ys)

-- =============================================================================
-- CUSTOM DATA TYPE EXAMPLE
-- =============================================================================

-- Define a simple data type
data Point = Point Int Int deriving (Show, Eq)

-- Generate random Points for QuickCheck
instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Point x y)
  
  -- Define shrinking for Points
  shrink (Point x y) = 
    [Point x' y' | (x', y') <- shrink (x, y)]

-- Property: Distance from origin is always non-negative
prop_distance_nonnegative :: Point -> Bool
prop_distance_nonnegative (Point x y) = 
  let distance = sqrt (fromIntegral (x*x + y*y))
  in distance >= 0

-- Property: Points are equal to themselves
prop_point_reflexive :: Point -> Bool
prop_point_reflexive p = p == p

-- =============================================================================
-- PROPERTIES THAT WILL FAIL (FOR DEMONSTRATION)
-- =============================================================================

-- This property is FALSE - it will fail and show shrinking
-- QuickCheck will find the smallest counterexample
prop_false_property :: Int -> Bool
prop_false_property x = x * x >= x  -- This fails for x = 0

-- Another failing property
prop_division_property :: Int -> Int -> Property
prop_division_property x y = 
  y /= 0 ==> x `div` y * y + x `mod` y == x

-- =============================================================================
-- CUSTOM GENERATORS
-- =============================================================================

-- Generate only positive integers
positiveInt :: Gen Int
positiveInt = abs <$> arbitrary `suchThat` (/= 0)

-- Generate sorted lists
sortedList :: Gen [Int]
sortedList = sort <$> arbitrary

-- Generate lists with no duplicates
uniqueList :: Gen [Int]
uniqueList = nub <$> arbitrary

-- Properties using custom generators
prop_positive_addition :: Property
prop_positive_addition = forAll positiveInt $ \x ->
  forAll positiveInt $ \y ->
    x + y > 0

prop_sorted_list_property :: Property
prop_sorted_list_property = forAll sortedList $ \xs ->
  isSorted xs
  where
    isSorted [] = True
    isSorted [_] = True
    isSorted (x:y:ys) = x <= y && isSorted (y:ys)

-- =============================================================================
-- MAIN FUNCTION
-- =============================================================================

main :: IO ()
main = do
  putStrLn "=== Real QuickCheck Property Testing Examples ==="
  putStrLn "This demonstrates the full power of QuickCheck"
  putStrLn ""
  
  putStrLn "1. Testing addition commutativity..."
  quickCheck prop_addition_commutative
  
  putStrLn "\n2. Testing addition associativity..."
  quickCheck prop_addition_associative
  
  putStrLn "\n3. Testing zero identity..."
  quickCheck prop_zero_identity
  
  putStrLn "\n4. Testing list reverse inverse..."
  quickCheck prop_reverse_inverse
  
  putStrLn "\n5. Testing sort idempotence..."
  quickCheck prop_sort_idempotent
  
  putStrLn "\n6. Testing concatenation length..."
  quickCheck prop_concat_length
  
  putStrLn "\n7. Testing absolute value non-negative..."
  quickCheck prop_abs_nonnegative
  
  putStrLn "\n8. Testing absolute value of negative numbers..."
  quickCheck prop_abs_negative_positive
  
  putStrLn "\n9. Testing conditional properties..."
  quickCheck prop_positive_sum
  quickCheck prop_sorted_unchanged
  
  putStrLn "\n10. Testing custom Point data type..."
  quickCheck prop_distance_nonnegative
  quickCheck prop_point_reflexive
  
  putStrLn "\n11. Testing custom generators..."
  quickCheck prop_positive_addition
  quickCheck prop_sorted_list_property
  
  putStrLn "\n12. Testing properties that will fail (demonstrating shrinking)..."
  putStrLn "This will show QuickCheck's shrinking in action:"
  quickCheck prop_false_property
  
  putStrLn "\n13. Testing division property..."
  quickCheck prop_division_property
  
  putStrLn "\n=== Real QuickCheck Examples Completed ==="
  putStrLn ""
  putStrLn "Key QuickCheck Features Demonstrated:"
  putStrLn "1. Automatic test generation with 'arbitrary'"
  putStrLn "2. Conditional properties with '==>'"
  putStrLn "3. Custom data types with Arbitrary instances"
  putStrLn "4. Custom generators with 'forAll'"
  putStrLn "5. Automatic shrinking of failing test cases"
  putStrLn "6. Rich reporting with counterexamples" 