module Main where

-- Import the generated parser modules
import qualified AbsSystemVerilogTest as Abs
import ErrMSystemVerilogTest (Err (..))
import qualified ErrMSystemVerilogTest as ErrM
import qualified LexSystemVerilogTest as Lex
import qualified ParSystemVerilogTest as Par
import System.Exit (exitFailure, exitSuccess)
import System.IO (readFile)

-- Test data structure
data TestResult = TestResult
  { testName :: String,
    input :: String,
    expected :: Bool,
    actual :: Bool
  }
  deriving (Show)

-- Helper function to parse SystemVerilog source text
parseSystemVerilog :: String -> Either String Abs.SourceText
parseSystemVerilog s = case Par.pSourceText (Par.myLexer s) of
  ErrM.Bad err -> Left err
  ErrM.Ok sourceText -> Right sourceText

-- Extract module names from parsed AST
extractModuleNames :: Abs.SourceText -> [String]
extractModuleNames (Abs.SourceTextModules modules) = map getModuleName modules
  where
    getModuleName (Abs.ModuleDeclTest ident) = getIdentName ident
    getIdentName (Abs.Ident name) = name

-- Test cases
testCases :: [String]
testCases =
  [ "module test;\nendmodule",
    "module simple_module;\nendmodule\n\nmodule counter;\nendmodule",
    "// Comment\nmodule memory_controller;\nendmodule"
  ]

-- Expected results (number of modules)
expectedModuleCounts :: [Int]
expectedModuleCounts = [1, 2, 1]

-- Run a single test
runTest :: String -> String -> Int -> IO TestResult
runTest name input expectedCount = do
  case parseSystemVerilog input of
    Left err -> do
      putStrLn $ "Parse error in test '" ++ name ++ "': " ++ err
      return $ TestResult name input True False
    Right sourceText -> do
      let moduleNames = extractModuleNames sourceText
      let actualCount = length moduleNames
      let success = actualCount == expectedCount
      putStrLn $
        "Test '"
          ++ name
          ++ "': "
          ++ "Expected "
          ++ show expectedCount
          ++ " modules, "
          ++ "got "
          ++ show actualCount
          ++ " modules"
      when success $ putStrLn $ "  Module names: " ++ show moduleNames
      return $ TestResult name input success success
  where
    when True action = action
    when False _ = return ()

-- Test parsing the actual test file
testFileContent :: IO TestResult
testFileContent = do
  putStrLn "Testing with actual test file..."
  content <- readFile "haskell/app/bnfc-systemverilog-parser/tests/minimal/test_input.sv"
  case parseSystemVerilog content of
    Left err -> do
      putStrLn $ "Failed to parse test file: " ++ err
      return $ TestResult "test_input.sv" content True False
    Right sourceText -> do
      let moduleNames = extractModuleNames sourceText
      let moduleCount = length moduleNames
      putStrLn $ "Successfully parsed test file with " ++ show moduleCount ++ " modules:"
      mapM_ (\name -> putStrLn $ "  - " ++ name) moduleNames
      return $ TestResult "test_input.sv" content True True

-- Main test runner
main :: IO ()
main = do
  putStrLn "Running SystemVerilog Minimal Parser Tests"
  putStrLn "=========================================="

  -- Run basic test cases
  results <-
    sequence $
      zipWith3
        (\i input expected -> runTest ("test_" ++ show i) input expected)
        [1 ..]
        testCases
        expectedModuleCounts

  -- Test with file content
  fileResult <- testFileContent

  let allResults = results ++ [fileResult]
  let passedTests = filter actual allResults
  let totalTests = length allResults
  let passedCount = length passedTests

  putStrLn ""
  putStrLn $ "Test Results: " ++ show passedCount ++ "/" ++ show totalTests ++ " passed"

  if passedCount == totalTests
    then do
      putStrLn "All tests passed!"
      exitSuccess
    else do
      putStrLn "Some tests failed!"
      exitFailure
