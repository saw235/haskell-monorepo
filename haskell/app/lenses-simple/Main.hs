{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Char (toUpper)

-- ============================================================================
-- CORE PRINCIPLE: Lenses let you access and modify nested data cleanly
-- ============================================================================

-- Think of lenses as "smart getters and setters" for immutable data
-- Just like how monad transformers let you combine effects, lenses let you
-- compose data access operations

-- ============================================================================
-- STEP 1: Simple Data Structures
-- ============================================================================

-- Simple person with address
data Person = Person
  { _personName :: String
  , _personAge :: Int
  , _personAddress :: Address
  } deriving (Show)

data Address = Address
  { _addressStreet :: String
  , _addressCity :: String
  } deriving (Show)

-- Generate lenses automatically (this creates: personName, personAge, personAddress, addressStreet, addressCity)
makeLenses ''Person
makeLenses ''Address

-- ============================================================================
-- STEP 2: Simple Examples of Each Operation
-- ============================================================================

-- Example person
alice :: Person
alice = Person "Alice" 30 (Address "123 Main St" "Boston")

-- GETTING values (view or ^.)
getName :: Person -> String
getName person = person ^. personName

getAge :: Person -> Int
getAge person = person ^. personAge

getStreet :: Person -> String
getStreet person = person ^. personAddress . addressStreet

-- SETTING values (set or .~)
setAge :: Int -> Person -> Person
setAge newAge person = person & personAge .~ newAge

setCity :: String -> Person -> Person
setCity newCity person = person & personAddress . addressCity .~ newCity

-- MODIFYING values (over or %~)
incrementAge :: Person -> Person
incrementAge person = person & personAge %~ (+1)

uppercaseName :: Person -> Person
uppercaseName person = person & personName %~ map toUpper

-- ============================================================================
-- STEP 3: Combining Operations (The Magic!)
-- ============================================================================

-- Multiple operations in one go
updatePerson :: Person -> Person
updatePerson person = person
  & personName .~ "Alice Smith"
  & personAge %~ (+5)
  & personAddress . addressCity .~ "New York"

-- Deep nested access and modification
updateStreet :: String -> Person -> Person
updateStreet newStreet person = person & personAddress . addressStreet .~ newStreet

-- ============================================================================
-- STEP 4: Running the Examples
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=== Simple Lens Examples (3 Operations Only) ==="
  putStrLn ""
  
  -- Test individual operations
  putStrLn "--- Individual Operations ---"
  
  -- Getting values
  putStrLn "1. Getting values:"
  putStrLn $ "   Alice's name: " ++ getName alice
  putStrLn $ "   Alice's age: " ++ show (getAge alice)
  putStrLn $ "   Alice's street: " ++ getStreet alice
  putStrLn ""
  
  -- Setting values
  putStrLn "2. Setting values:"
  let aliceOlder = setAge 31 alice
  putStrLn $ "   Alice after birthday: " ++ show aliceOlder
  
  let aliceMoved = setCity "New York" alice
  putStrLn $ "   Alice after moving: " ++ show aliceMoved
  putStrLn ""
  
  -- Modifying values
  putStrLn "3. Modifying values:"
  let aliceAged = incrementAge alice
  putStrLn $ "   Alice aged by 1 year: " ++ show aliceAged
  
  let aliceUpperCase = uppercaseName alice
  putStrLn $ "   Alice with uppercase name: " ++ show aliceUpperCase
  putStrLn ""
  
  -- Test combining operations
  putStrLn "--- Combining Operations ---"
  putStrLn "4. Multiple operations together:"
  let aliceUpdated = updatePerson alice
  putStrLn $ "   Alice completely updated: " ++ show aliceUpdated
  putStrLn ""
  
  putStrLn "=== The Core Principle ==="
  putStrLn ""
  putStrLn "Lenses let you:"
  putStrLn "1. Take simple operations (get, set, modify)"
  putStrLn "2. Compose them together with (.)"
  putStrLn "3. Get clean, safe data access and modification"
  putStrLn ""
  putStrLn "The type system ensures you can't access non-existent fields"
  putStrLn "or modify data incorrectly. It's like having a safety net!"
  putStrLn ""
  
  -- Show the comparison with manual approach
  putStrLn "=== COMPARISON: With vs Without Lenses ==="
  putStrLn ""
  
  putStrLn "WITH Lenses (clean):"
  putStrLn "updatePerson :: Person -> Person"
  putStrLn "updatePerson person = person"
  putStrLn "  & personName .~ \"Alice Smith\""
  putStrLn "  & personAge %~ (+5)"
  putStrLn "  & personAddress . addressCity .~ \"New York\""
  putStrLn ""
  
  putStrLn "WITHOUT Lenses (messy):"
  putStrLn "updatePersonManual :: Person -> Person"
  putStrLn "updatePersonManual person = person"
  putStrLn "  { personName = \"Alice Smith\""
  putStrLn "  , personAge = personAge person + 5"
  putStrLn "  , personAddress = (personAddress person)"
  putStrLn "      { addressCity = \"New York\""
  putStrLn "      }"
  putStrLn "  }"
  putStrLn ""
  
  putStrLn "=== Key Problems Lenses Solve ==="
  putStrLn ""
  putStrLn "1. VERBOSE SYNTAX: Without lenses, nested updates require"
  putStrLn "   lots of record syntax - hard to read and write!"
  putStrLn ""
  putStrLn "2. TYPE SAFETY: The type system can't catch if you forget to"
  putStrLn "   update all the nested records correctly"
  putStrLn ""
  putStrLn "3. COMPOSABILITY: Adding more fields makes the manual"
  putStrLn "   approach exponentially worse"
  putStrLn ""
  putStrLn "4. READABILITY: Manual record updates make code hard to read"
  putStrLn "   and understand"
  putStrLn ""
  putStrLn "5. MAINTAINABILITY: Changes require updating many record"
  putStrLn "   constructors"
  putStrLn ""
  
  putStrLn "=== Example completed! ===" 