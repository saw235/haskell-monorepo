# Simple Lens Example (3 Operations Only)

This is a **super simple** example of lenses. It shows the core principle: **clean data access and modification**.

## What is a Lens?

Think of it like **smart getters and setters** for immutable data:

- Each lens provides one "operation" on your data
- You can compose them together to get complex operations
- The type system makes sure you can't access non-existent fields

## The Three Basic Operations

### 1. Getting - "I need to read a value"
```haskell
getName :: Person -> String
getName person = person ^. name
```
- Like reading a field from a record
- You can access nested data easily

### 2. Setting - "I need to set a value"
```haskell
setAge :: Int -> Person -> Person
setAge newAge person = person & age .~ newAge
```
- Like updating a field in a record
- You can update nested data cleanly

### 3. Modifying - "I need to change a value"
```haskell
incrementAge :: Person -> Person
incrementAge person = person & age %~ (+1)
```
- Like transforming a field value
- You can apply functions to data

## The Magic: Combining Operations

### Multiple Operations Together
```haskell
updatePerson :: Person -> Person
updatePerson person = person
  & name .~ "Alice Smith"
  & age %~ (+5)
  & address . city .~ "New York"
```
This can update multiple fields at once!

## The Core Principle

**Lenses let you:**
1. Take simple operations (get, set, modify)
2. Compose them together with (.)
3. Get clean, safe data access and modification

**The type system ensures you can't access non-existent fields or modify data incorrectly.**

## Running the Example

```bash
# Build it
bazel build //haskell/app/lenses-simple:lenses-simple

# Run it
bazel run //haskell/app/lenses-simple:lenses-simple
```

## What You'll See

The example shows:
1. Each operation working by itself
2. Multiple operations working together

You'll see how the same data can be accessed and modified in clean, composable ways!

## Why This Matters

This is Haskell's **superpower** for data manipulation:
- **Type-safe access** to nested data
- **No runtime errors** from accessing non-existent fields
- **Clear composition** of data operations
- **Composable** - you can build complex operations from simple ones

## Real-World Analogy

Think of it like a **smart form system**:
- **Getting**: Reading values from form fields
- **Setting**: Updating form fields with new values
- **Modifying**: Transforming form values (like converting to uppercase)

A **lens composition** is like having all these capabilities work together seamlessly, and the type system makes sure you don't try to access fields that don't exist!

## The Key Insight

**Without lenses:**
```haskell
-- You'd have to manually update nested records
updatePersonManual :: Person -> Person
updatePersonManual person = person
  { personName = "Alice Smith"
  , personAge = personAge person + 5
  , personAddress = (personAddress person)
    { addressCity = "New York"
    }
  }
```

**With lenses:**
```haskell
-- The type system handles everything for you
updatePerson :: Person -> Person
updatePerson person = person
  & name .~ "Alice Smith"
  & age %~ (+5)
  & address . city .~ "New York"
```

The second version is **safer** because the type system ensures you can't access non-existent fields!

## Next Steps

After understanding this simple example:
1. Look at the more complex example in `lenses-explanation/`
2. Try adding more nested data structures
3. Learn about traversals and prisms
4. Explore how lenses work with monad transformers
5. See real-world applications in web frameworks 