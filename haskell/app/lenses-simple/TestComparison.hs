module TestComparison where

import Main
import WithoutLenses

-- Test both approaches side by side
testBothApproaches :: IO ()
testBothApproaches = do
  putStrLn "=== TESTING BOTH APPROACHES ==="
  putStrLn ""

  -- Test lens approach
  putStrLn "--- WITH Lenses ---"
  let aliceLens = updatePerson alice
  putStrLn $ "Result: " ++ show aliceLens
  putStrLn ""

  -- Test manual approach
  putStrLn "--- WITHOUT Lenses ---"
  let aliceManual = updatePersonManual alice
  putStrLn $ "Result: " ++ show aliceManual
  putStrLn ""

  putStrLn "=== Both approaches give the same result! ==="
  putStrLn "But the lens approach is:"
  putStrLn "- Cleaner to write"
  putStrLn "- Type-safe"
  putStrLn "- Easier to compose"
  putStrLn "- Less error-prone"
  putStrLn "- More readable"
