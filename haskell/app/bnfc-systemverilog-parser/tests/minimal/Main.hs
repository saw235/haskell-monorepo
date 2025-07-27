module Main where

import System.Exit (exitFailure, exitSuccess)
import System.IO (readFile)
import Test.HUnit
import Test.HUnit.Text (runTestTT)

-- Import the generated parser modules
import qualified AbsSystemVerilogTest as Abs
import ErrMSystemVerilogTest (Err (..))
import qualified ErrMSystemVerilogTest as ErrM
import qualified LexSystemVerilogTest as Lex
import qualified ParSystemVerilogTest as Par

-- =============================================================================
-- HELPER FUNCTIONS
-- =============================================================================

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

-- =============================================================================
-- TEST CASES
-- =============================================================================

-- Test parsing a single simple module
testSingleModule :: Test
testSingleModule = TestCase $ do
  let input = "module test;\nendmodule"
  case parseSystemVerilog input of
    Left err -> assertFailure $ "Failed to parse single module: " ++ err
    Right sourceText -> do
      let modules = extractModuleNames sourceText
      assertEqual "Should have exactly 1 module" 1 (length modules)
      assertEqual "Module name should be 'test'" "test" (head modules)

-- Test parsing multiple modules
testMultipleModules :: Test
testMultipleModules = TestCase $ do
  let input = "module first;\nendmodule\n\nmodule second;\nendmodule"
  case parseSystemVerilog input of
    Left err -> assertFailure $ "Failed to parse multiple modules: " ++ err
    Right sourceText -> do
      let modules = extractModuleNames sourceText
      assertEqual "Should have exactly 2 modules" 2 (length modules)
      assertEqual "First module should be 'first'" "first" (modules !! 0)
      assertEqual "Second module should be 'second'" "second" (modules !! 1)

-- Test parsing module with comments
testModuleWithComments :: Test
testModuleWithComments = TestCase $ do
  let input = "// This is a comment\nmodule commented;\nendmodule"
  case parseSystemVerilog input of
    Left err -> assertFailure $ "Failed to parse module with comments: " ++ err
    Right sourceText -> do
      let modules = extractModuleNames sourceText
      assertEqual "Should have exactly 1 module" 1 (length modules)
      assertEqual "Module name should be 'commented'" "commented" (head modules)

-- Test parsing empty input
testEmptyInput :: Test
testEmptyInput = TestCase $ do
  case parseSystemVerilog "" of
    Left err -> assertFailure $ "Empty input should parse successfully: " ++ err
    Right sourceText -> do
      let modules = extractModuleNames sourceText
      assertEqual "Empty input should have 0 modules" 0 (length modules)

-- Test parsing invalid input
testInvalidInput :: Test
testInvalidInput = TestCase $ do
  let input = "this is not valid systemverilog"
  case parseSystemVerilog input of
    Left _ -> return () -- Expected to fail
    Right _ -> assertFailure "Invalid input should fail to parse"

-- Test parsing the actual test file
testFileContent :: Test
testFileContent = TestCase $ do
  content <- readFile "haskell/app/bnfc-systemverilog-parser/tests/minimal/test_input.sv"
  case parseSystemVerilog content of
    Left err -> assertFailure $ "Failed to parse test file: " ++ err
    Right sourceText -> do
      let modules = extractModuleNames sourceText
      assertEqual "Test file should have 3 modules" 3 (length modules)
      assertBool "Should contain 'simple_module'" ("simple_module" `elem` modules)
      assertBool "Should contain 'counter'" ("counter" `elem` modules)
      assertBool "Should contain 'memory_controller'" ("memory_controller" `elem` modules)

-- Test lexer tokens
testLexer :: Test
testLexer = TestCase $ do
  let input = "module test;"
  let tokens = Par.myLexer input
  assertBool "Should produce some tokens" (not (null tokens))

-- =============================================================================
-- TEST SUITE ASSEMBLY
-- =============================================================================

-- All HUnit tests
hunitTests :: Test
hunitTests = TestList
  [ TestLabel "Single Module" testSingleModule,
    TestLabel "Multiple Modules" testMultipleModules,
    TestLabel "Module with Comments" testModuleWithComments,
    TestLabel "Empty Input" testEmptyInput,
    TestLabel "Invalid Input" testInvalidInput,
    TestLabel "Test File Content" testFileContent,
    TestLabel "Lexer" testLexer
  ]

-- =============================================================================
-- MAIN TEST RUNNER
-- =============================================================================

main :: IO ()
main = do
  putStrLn "Running SystemVerilog Minimal Parser Tests"
  putStrLn "==========================================="
  putStrLn ""
  
  -- Run HUnit tests
  putStrLn "=== Running HUnit Tests ==="
  hunitCounts <- runTestTT hunitTests
  
  putStrLn ""
  putStrLn "==========================================="
  putStrLn "Test Summary:"
  putStrLn $ "  Cases: " ++ show (cases hunitCounts)
  putStrLn $ "  Tried: " ++ show (tried hunitCounts)
  putStrLn $ "  Errors: " ++ show (errors hunitCounts)
  putStrLn $ "  Failures: " ++ show (failures hunitCounts)
  putStrLn ""
  
  -- Exit with appropriate status
  if failures hunitCounts > 0 || errors hunitCounts > 0
    then do
      putStrLn $ "ERROR: " ++ show (failures hunitCounts + errors hunitCounts) ++ " tests failed!"
      exitFailure
    else do
      putStrLn "All tests passed!"
      exitSuccess 