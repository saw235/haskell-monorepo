# Simple Monad Transformer Example (2 Effects Only)

This is a **super simple** example of monad transformers. It shows the core principle: **combining 2 effects safely**.

## What is a Monad Transformer?

Think of it like **LEGO blocks for effects**:

- Each transformer adds one "capability" to your computation
- You can stack them together to get multiple capabilities
- The type system makes sure you can't mess up

## The Two Basic Effects

### 1. Reader - "I need to read something"
```haskell
type SimpleReader = ReaderT String IO
```
- Like reading from a config file
- You can ask for a value, but you can't change it

### 2. State - "I need to remember something"
```haskell
type SimpleState = StateT Int IO
```
- Like keeping a counter
- You can read and write to it

## The Magic: Combining Two Effects

### Two Effects Together
```haskell
type ReaderWithState = ReaderT String (StateT Int IO)
```
This can both read a name AND keep a counter!

## The Core Principle

**Monad transformers let you:**
1. Take simple effects (Reader, State)
2. Stack them together like LEGO blocks
3. Get a computation that can do BOTH safely

**The type system ensures you can't forget to handle state or access configuration incorrectly.**

## Running the Example

```bash
# Build it
bazel build //haskell/app/monad-transformer-simple:monad-transformer-simple

# Run it
bazel run //haskell/app/monad-transformer-simple:monad-transformer-simple
```

## What You'll See

The example shows:
1. Each effect working by itself
2. Two effects working together

You'll see how the same function can read a name AND update a counter - both at once!

## Why This Matters

This is Haskell's **superpower**:
- **Type-safe composition** of effects
- **No runtime errors** from forgetting to handle something
- **Clear separation** of what each part does
- **Composable** - you can build big things from small pieces

## Real-World Analogy

Think of it like a **smart greeting system**:
- **Reader**: Guest list (you can read names, but not change the list)
- **State**: Visitor counter (you can see how many visitors and update it)

A **monad transformer stack** is like having both capabilities in one system, and the type system makes sure you don't forget to update the counter when greeting someone!

## The Key Insight

**Without monad transformers:**
```haskell
-- You'd have to manually pass everything around
greetUser :: String -> Int -> (String, Int)
greetUser name counter = 
  ("Hello " ++ name ++ "! You are visitor #" ++ show (counter + 1), counter + 1)
```

**With monad transformers:**
```haskell
-- The type system handles everything for you
greetUser :: ReaderWithState String
greetUser = do
  name <- ask        -- Read the name
  current <- get     -- Get current counter
  put (current + 1)  -- Update counter
  return $ "Hello " ++ name ++ "! You are visitor #" ++ show (current + 1)
```

The second version is **safer** because the type system ensures you can't forget to update the counter!

## Next Steps

After understanding this simple example:
1. Look at the more complex example in `monad-transformer-example/`
2. Try adding a third effect (like Writer for logging)
3. Learn about MTL type classes
4. Explore real-world applications 