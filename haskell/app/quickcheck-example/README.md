# QuickCheck Property-Based Testing

This directory contains examples demonstrating **QuickCheck**, Haskell's revolutionary property-based testing library. QuickCheck fundamentally changed how we think about testing by introducing the concept of **property-based testing**.

## What is QuickCheck?

QuickCheck is a property-based testing library that automatically generates test cases based on **properties** you define. Instead of writing specific test cases, you write boolean functions that should always return `True` for all valid inputs.

### 🎯 **The Core Idea**

**Traditional Testing (Unit Tests):**
```haskell
-- You write specific test cases
test_addition_1 = assertEqual "2+3=5" 5 (2 + 3)
test_addition_2 = assertEqual "0+5=5" 5 (0 + 5)
test_addition_3 = assertEqual "-1+6=5" 5 (-1 + 6)
-- ... many more specific cases
```

**QuickCheck (Property-Based Testing):**
```haskell
-- You write a property that should always be true
prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative a b = a + b == b + a
-- QuickCheck automatically tests this with 100+ random inputs!
```

## How QuickCheck Works

### 1. **Property Definition**
You write a boolean function that represents a property that should always hold:

```haskell
-- This property says: "addition is commutative"
prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative a b = a + b == b + a
```

### 2. **Automatic Test Generation**
QuickCheck automatically generates random test data using the `Arbitrary` typeclass:

```haskell
-- QuickCheck uses this to generate random Ints
instance Arbitrary Int where
  arbitrary = choose (-1000, 1000)  -- Simplified example
```

### 3. **Test Execution**
QuickCheck runs your property with many randomly generated inputs:

```haskell
quickCheck prop_addition_commutative
-- Output: +++ OK, passed 100 tests.
```

### 4. **Shrinking (When Tests Fail)**
If a test fails, QuickCheck tries to find the **smallest** counterexample:

```haskell
-- If this fails, QuickCheck will try smaller values
prop_false_property :: Int -> Bool
prop_false_property x = x * x >= x  -- Fails for x = 0

-- QuickCheck might find: "Falsifiable (after 3 tests): 0"
```

## Key Concepts

### 🔧 **Properties**
Properties are boolean functions that should always return `True`:

```haskell
-- Mathematical properties
prop_commutative :: Int -> Int -> Bool
prop_commutative a b = a + b == b + a

-- Invariant properties
prop_reverse_inverse :: [Int] -> Bool
prop_reverse_inverse xs = reverse (reverse xs) == xs

-- Business logic properties
prop_positive_after_abs :: Int -> Bool
prop_positive_after_abs x = abs x >= 0
```

### 🎲 **Arbitrary Typeclass**
The `Arbitrary` typeclass defines how to generate random test data:

```haskell
class Arbitrary a where
  arbitrary :: Gen a
  shrink :: a -> [a]  -- For shrinking counterexamples

-- Built-in instances for common types
instance Arbitrary Int where
  arbitrary = choose (-1000, 1000)
  shrink x = [x `div` 2, x - 1, x + 1]

instance Arbitrary [a] where
  arbitrary = sized $ \n -> vectorOf n arbitrary
  shrink xs = take (length xs `div` 2) xs : [tail xs, init xs]
```

### 🎯 **Conditional Properties**
Properties that only hold under certain conditions:

```haskell
-- Only test when both numbers are positive
prop_positive_sum :: Int -> Int -> Property
prop_positive_sum x y = 
  x > 0 && y > 0 ==> x + y > 0

-- Only test when the list is sorted
prop_sorted_unchanged :: [Int] -> Property
prop_sorted_unchanged xs = 
  isSorted xs ==> sort xs == xs
```

### 🛠️ **Custom Generators**
You can create custom generators for specific test data:

```haskell
-- Generate only positive integers
positiveInt :: Gen Int
positiveInt = abs <$> arbitrary `suchThat` (/= 0)

-- Generate sorted lists
sortedList :: Gen [Int]
sortedList = sort <$> arbitrary

-- Use custom generators in properties
prop_positive_addition :: Property
prop_positive_addition = forAll positiveInt $ \x ->
  forAll positiveInt $ \y ->
    x + y > 0
```

## Examples in This Directory

### 1. **Manual QuickCheck Example** (`QuickCheckExample.hs`)
A simplified implementation that demonstrates the core concepts without external dependencies.

**Run with:**
```bash
bazel run //haskell/app/quickcheck-example:quickcheck-example
```

**What it demonstrates:**
- Basic property definitions
- Random test generation
- Test execution and reporting
- Simple shrinking implementation

### 2. **Real QuickCheck Example** (`RealQuickCheckExample.hs`)
Full QuickCheck implementation using the actual library.

**Run with:**
```bash
bazel run //haskell/app/quickcheck-example:real-quickcheck-example
```

**What it demonstrates:**
- Real QuickCheck properties
- Conditional properties with `==>`
- Custom data types with `Arbitrary` instances
- Custom generators with `forAll`
- Automatic shrinking
- Rich reporting

## QuickCheck vs Traditional Testing

| Aspect | Traditional Testing | QuickCheck |
|--------|-------------------|------------|
| **Test Cases** | Manually written | Automatically generated |
| **Coverage** | Limited by imagination | Extensive random coverage |
| **Maintenance** | High (update when code changes) | Low (properties often unchanged) |
| **Bug Finding** | Finds expected bugs | Finds unexpected edge cases |
| **Documentation** | Shows specific examples | Documents general properties |
| **Confidence** | "These specific cases work" | "This property holds for all inputs" |

## Real-World Benefits

### 🐛 **Finding Unexpected Bugs**
```haskell
-- You might think this property is obvious
prop_list_length :: [Int] -> Bool
prop_list_length xs = length xs >= 0

-- But QuickCheck might find edge cases you never considered
-- like very large lists, empty lists, etc.
```

### 📚 **Documentation Through Properties**
```haskell
-- This property documents that sorting is idempotent
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = sort (sort xs) == sort xs

-- Anyone reading this knows: "sorting twice is the same as sorting once"
```

### 🔄 **Regression Testing**
```haskell
-- When you refactor code, run the same properties
-- If they still pass, you haven't broken the fundamental behavior
prop_my_function_behavior :: Input -> Bool
prop_my_function_behavior input = 
  someInvariant (myFunction input)
```

### 🎯 **Specification-Driven Development**
```haskell
-- Write properties first, then implement
prop_parse_roundtrip :: String -> Bool
prop_parse_roundtrip s = 
  parse (show (parse s)) == parse s

-- This guides your implementation
```

## Advanced QuickCheck Features

### 🎭 **Stateful Testing**
QuickCheck can test stateful operations:

```haskell
-- Test that a stack implementation works correctly
prop_stack_operations :: [StackOp] -> Bool
prop_stack_operations ops = 
  -- Simulate operations and verify invariants
  all isValidState (scanl applyOp emptyStack ops)
```

### 📊 **Coverage Testing**
QuickCheck can track test coverage:

```haskell
-- Ensure all code paths are tested
prop_with_coverage :: Int -> Property
prop_with_coverage x = 
  cover 10 (x < 0) "negative" $
  cover 10 (x == 0) "zero" $
  cover 10 (x > 0) "positive" $
  abs x >= 0
```

### 🔧 **Custom Shrinking**
You can define how to shrink failing test cases:

```haskell
instance Arbitrary MyType where
  arbitrary = -- generate random MyType
  shrink x = -- produce smaller versions of x
```

## Best Practices

### ✅ **Write Meaningful Properties**
```haskell
-- Good: Tests fundamental behavior
prop_commutative :: Int -> Int -> Bool
prop_commutative a b = a + b == b + a

-- Bad: Tests implementation details
prop_internal_representation :: Int -> Bool
prop_internal_representation x = 
  internalField x == expectedValue
```

### ✅ **Use Conditional Properties Sparingly**
```haskell
-- Good: Use custom generators instead
prop_positive_addition :: Property
prop_positive_addition = forAll positiveInt $ \x ->
  forAll positiveInt $ \y ->
    x + y > 0

-- Avoid: Too many conditions can make tests slow
prop_conditional :: Int -> Int -> Property
prop_conditional x y = 
  x > 0 && y > 0 && x < 100 && y < 100 ==> x + y > 0
```

### ✅ **Test Invariants, Not Implementation**
```haskell
-- Good: Tests that the result is sorted
prop_sort_result_sorted :: [Int] -> Bool
prop_sort_result_sorted xs = isSorted (sort xs)

-- Avoid: Tests specific sorting algorithm details
prop_sort_uses_quicksort :: [Int] -> Bool
prop_sort_uses_quicksort xs = 
  -- Implementation-specific test
```

## Common Patterns

### 🔄 **Round-trip Properties**
```haskell
-- Test that parsing and showing work together
prop_parse_show_roundtrip :: MyType -> Bool
prop_parse_show_roundtrip x = 
  parse (show x) == Just x
```

### 🔄 **Inverse Properties**
```haskell
-- Test that operations have inverses
prop_reverse_inverse :: [a] -> Bool
prop_reverse_inverse xs = 
  reverse (reverse xs) == xs
```

### 🔄 **Idempotent Properties**
```haskell
-- Test that operations are idempotent
prop_sort_idempotent :: [Int] -> Bool
prop_sort_idempotent xs = 
  sort (sort xs) == sort xs
```

### 🔄 **Commutative Properties**
```haskell
-- Test that operations commute
prop_addition_commutative :: Int -> Int -> Bool
prop_addition_commutative a b = 
  a + b == b + a
```

## When to Use QuickCheck

### ✅ **Great for:**
- Mathematical functions
- Data structure implementations
- Parser/pretty-printer pairs
- Serialization/deserialization
- State machine implementations
- Any function with clear invariants

### ❌ **Not ideal for:**
- UI testing
- Integration testing with external systems
- Performance testing
- Testing with very specific requirements
- Testing side effects (use `IO` properties carefully)

## The Impact of QuickCheck

QuickCheck revolutionized testing by introducing:

1. **Property-based testing** as a paradigm
2. **Automatic test generation** from specifications
3. **Shrinking** to find minimal counterexamples
4. **Type-driven testing** through the `Arbitrary` typeclass

This approach has been adopted by many other languages and testing frameworks, making QuickCheck one of Haskell's most influential contributions to software engineering.

## Running the Examples

```bash
# Build both examples
bazel build //haskell/app/quickcheck-example:all

# Run the manual example
bazel run //haskell/app/quickcheck-example:quickcheck-example

# Run the real QuickCheck example
bazel run //haskell/app/quickcheck-example:real-quickcheck-example
```

## Further Reading

- [QuickCheck Paper](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf)
- [QuickCheck Manual](https://hackage.haskell.org/package/QuickCheck)
- [Property-Based Testing with QuickCheck](https://www.fpcomplete.com/haskell/tutorial/property-based-testing-quickcheck/)

---

**QuickCheck shows that the best tests are not specific examples, but general properties that your code should always satisfy.** 