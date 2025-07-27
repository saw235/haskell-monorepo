module Main where

-- SystemVerilog test grammar imports
import qualified AbsSystemVerilogTest as Abs
import ErrMSystemVerilogTest (Err (..))
import qualified ErrMSystemVerilogTest as ErrM
import qualified LexSystemVerilogTest as Lex
import qualified ParSystemVerilogTest as Par

-- SystemVerilog AST analysis
data ModuleStats = ModuleStats
  { moduleCount :: Int,
    portCount :: Int,
    wireDecls :: Int,
    regDecls :: Int,
    assignStmts :: Int,
    instantiations :: Int,
    alwaysBlocks :: Int
  }
  deriving (Show)

emptyStats :: ModuleStats
emptyStats = ModuleStats 0 0 0 0 0 0 0

-- Analysis function for SystemVerilog modules
analyzeModule :: Abs.ModuleDeclaration -> ModuleStats
analyzeModule _ = emptyStats -- TODO: Implement proper module analysis

-- Analysis function for source text
analyzeSourceText :: Abs.SourceText -> ModuleStats
analyzeSourceText _ = emptyStats -- TODO: Implement proper source text analysis

-- Parse SystemVerilog source text (top-level)
parseSourceText :: String -> Either String ModuleStats
parseSourceText s = case Par.pSourceText (Par.myLexer s) of
  ErrM.Bad err -> Left err
  ErrM.Ok sourceText -> Right (analyzeSourceText sourceText)

-- Parse SystemVerilog module declaration
parseModule :: String -> Either String String
parseModule s = case Par.pModuleDeclaration (Par.myLexer s) of
  ErrM.Bad err -> Left err
  ErrM.Ok mod' -> Right (show mod') -- Show the parsed AST

-- Placeholder for expression parsing (not in minimal grammar)
parseExpression :: String -> Either String String
parseExpression _ = Left "Expression parsing not implemented in minimal grammar"

-- Interactive demo
main :: IO ()
main = do
  putStrLn "SystemVerilog BNFC Parser Demo"
  putStrLn "==============================="
  putStrLn ""
  
  putStrLn "This is a scaffolded SystemVerilog parser."
  putStrLn "The actual grammar files will be generated from the BNF you provide."
  putStrLn ""
  
  -- TODO: Add example parsing once BNF is provided
  putStrLn "Example usage (after BNF implementation):"
  putStrLn "- Parse SystemVerilog modules"
  putStrLn "- Analyze module structure"
  putStrLn "- Extract port lists"
  putStrLn "- Count wire/reg declarations"
  putStrLn "- Analyze always blocks"
  putStrLn ""
  
  putStrLn "Interactive Mode:"
  putStrLn "Type 'source: <systemverilog>' to parse complete source text"
  putStrLn "Type 'module: <module>' to parse module declarations"
  putStrLn "Type 'quit' to exit"
  putStrLn ""
  interactiveLoop

interactiveLoop :: IO ()
interactiveLoop = do
  putStr "> "
  input <- getLine
  case input of
    "quit" -> putStrLn "Goodbye!"
    _ | take 7 input == "source:" -> do
      let sourceCode = drop 8 input
      case parseSourceText sourceCode of
        Left err -> putStrLn $ "Source parse error: " ++ err
        Right stats -> putStrLn $ "Source statistics: " ++ show stats
      interactiveLoop
    -- Expression parsing removed from minimal grammar
    _ | take 7 input == "module:" -> do
      let modCode = drop 8 input
      case parseModule modCode of
        Left err -> putStrLn $ "Module parse error: " ++ err
        Right result -> putStrLn $ "Parsed module: " ++ result
      interactiveLoop
    _ -> do
      putStrLn "Unknown command. Use 'source: <code>', 'module: <module>', or 'quit'"
      interactiveLoop 