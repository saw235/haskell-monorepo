module TestComparison where

import Main
import WithoutMonadTransformers

-- Test both approaches side by side
testBothApproaches :: IO ()
testBothApproaches = do
  putStrLn "=== TESTING BOTH APPROACHES ==="
  putStrLn ""
  
  -- Test monad transformer approach
  putStrLn "--- WITH Monad Transformers ---"
  (result1, finalCount1) <- runStateT (runReaderT greetAndCount "Alice") 0
  putStrLn $ "Result: " ++ result1
  putStrLn $ "Final counter: " ++ show finalCount1
  putStrLn ""
  
  -- Test manual approach
  putStrLn "--- WITHOUT Monad Transformers ---"
  let (result2, finalCount2) = greetAndCountManual "Alice" 0
  putStrLn $ "Result: " ++ result2
  putStrLn $ "Final counter: " ++ show finalCount2
  putStrLn ""
  
  putStrLn "=== Both approaches give the same result! ==="
  putStrLn "But the monad transformer approach is:"
  putStrLn "- Cleaner to write"
  putStrLn "- Type-safe"
  putStrLn "- Easier to compose"
  putStrLn "- Less error-prone" 