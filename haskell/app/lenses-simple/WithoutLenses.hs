module WithoutLenses where

-- ============================================================================
-- WITHOUT LENSES: Manual record updates
-- ============================================================================

-- This shows how you'd have to do the same operations without lenses
-- It's verbose, error-prone, and hard to maintain

-- Same data structures as Main.hs
data Person = Person
  { personName :: String,
    personAge :: Int,
    personAddress :: Address
  }
  deriving (Show)

data Address = Address
  { addressStreet :: String,
    addressCity :: String
  }
  deriving (Show)

-- Example person
alice :: Person
alice = Person "Alice" 30 (Address "123 Main St" "Boston")

-- ============================================================================
-- Manual implementations of the same operations
-- ============================================================================

-- GETTING values (manual)
getNameManual :: Person -> String
getNameManual person = personName person

getAgeManual :: Person -> Int
getAgeManual person = personAge person

getStreetManual :: Person -> String
getStreetManual person = addressStreet (personAddress person)

-- SETTING values (manual)
setAgeManual :: Int -> Person -> Person
setAgeManual newAge person = person {personAge = newAge}

setCityManual :: String -> Person -> Person
setCityManual newCity person =
  person
    { personAddress =
        (personAddress person)
          { addressCity = newCity
          }
    }

-- MODIFYING values (manual)
incrementAgeManual :: Person -> Person
incrementAgeManual person = person {personAge = personAge person + 1}

uppercaseNameManual :: Person -> Person
uppercaseNameManual person = person {personName = map toUpper (personName person)}

-- COMBINING operations (manual)
updatePersonManual :: Person -> Person
updatePersonManual person =
  person
    { personName = "Alice Smith",
      personAge = personAge person + 5,
      personAddress =
        (personAddress person)
          { addressCity = "New York"
          }
    }

updateStreetManual :: String -> Person -> Person
updateStreetManual newStreet person =
  person
    { personAddress =
        (personAddress person)
          { addressStreet = newStreet
          }
    }

-- ============================================================================
-- Running manual examples
-- ============================================================================

runManualExamples :: IO ()
runManualExamples = do
  putStrLn "=== Manual Examples (Without Lenses) ==="
  putStrLn ""

  -- Test individual operations
  putStrLn "--- Individual Operations ---"

  -- Getting values
  putStrLn "1. Getting values:"
  putStrLn $ "   Alice's name: " ++ getNameManual alice
  putStrLn $ "   Alice's age: " ++ show (getAgeManual alice)
  putStrLn $ "   Alice's street: " ++ getStreetManual alice
  putStrLn ""

  -- Setting values
  putStrLn "2. Setting values:"
  let aliceOlder = setAgeManual 31 alice
  putStrLn $ "   Alice after birthday: " ++ show aliceOlder

  let aliceMoved = setCityManual "New York" alice
  putStrLn $ "   Alice after moving: " ++ show aliceMoved
  putStrLn ""

  -- Modifying values
  putStrLn "3. Modifying values:"
  let aliceAged = incrementAgeManual alice
  putStrLn $ "   Alice aged by 1 year: " ++ show aliceAged

  let aliceUpperCase = uppercaseNameManual alice
  putStrLn $ "   Alice with uppercase name: " ++ show aliceUpperCase
  putStrLn ""

  -- Test combining operations
  putStrLn "--- Combining Operations ---"
  putStrLn "4. Multiple operations together:"
  let aliceUpdated = updatePersonManual alice
  putStrLn $ "   Alice completely updated: " ++ show aliceUpdated
  putStrLn ""

  putStrLn "=== Manual Approach Problems ==="
  putStrLn ""
  putStrLn "Notice how:"
  putStrLn "1. Nested updates require lots of parentheses"
  putStrLn "2. You have to repeat field names multiple times"
  putStrLn "3. It's easy to forget to update all nested records"
  putStrLn "4. The code is much longer and harder to read"
  putStrLn "5. Adding more fields makes it exponentially worse"
  putStrLn ""

  putStrLn "=== Example completed! ==="
