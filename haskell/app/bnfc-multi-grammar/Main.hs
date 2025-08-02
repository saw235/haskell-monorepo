module Main where

-- Expression parser imports

import qualified AbsExpression as AbsExpr
-- Statement parser imports

import qualified AbsStatement as AbsStmt
-- Use expression ErrM as the main one since both should be identical
import ErrMExpression (Err (..))
import qualified ErrMExpression as ErrMExpr
import qualified ErrMStatement as ErrMStmt
import qualified LexExpression as LexExpr
import qualified LexStatement as LexStmt
import qualified ParExpression as ParExpr
import qualified ParStatement as ParStmt

-- Simple expression evaluator
evalExpr :: AbsExpr.Expr -> Double
evalExpr expr = case expr of
  AbsExpr.EAdd e1 e2 -> evalExpr e1 + evalExpr e2
  AbsExpr.ESub e1 e2 -> evalExpr e1 - evalExpr e2
  AbsExpr.EMul e1 e2 -> evalExpr e1 * evalExpr e2
  AbsExpr.EDiv e1 e2 -> evalExpr e1 / evalExpr e2
  AbsExpr.EMod e1 e2 -> fromInteger $ round (evalExpr e1) `mod` round (evalExpr e2)
  AbsExpr.EPow e1 e2 -> evalExpr e1 ** evalExpr e2
  AbsExpr.ENeg e -> -(evalExpr e)
  AbsExpr.EPos e -> evalExpr e
  AbsExpr.EInt n -> fromInteger n
  AbsExpr.EDouble d -> d
  AbsExpr.EVar _ -> 42.0 -- Simplified: all variables = 42
  AbsExpr.ECall _ _ -> 100.0 -- Simplified: all function calls = 100
  AbsExpr.EParen e -> evalExpr e

-- Parse and evaluate expression
parseExpression :: String -> Either String Double
parseExpression s = case ParExpr.pExpr (ParExpr.myLexer s) of
  ErrMExpr.Bad err -> Left err
  ErrMExpr.Ok expr -> Right (evalExpr expr)

-- Simple statement analyzer (just counts statement types)
data StmtStats = StmtStats
  { assignments :: Int,
    ifStatements :: Int,
    whileLoops :: Int,
    functionDefs :: Int,
    expressions :: Int
  }
  deriving (Show)

emptyStats :: StmtStats
emptyStats = StmtStats 0 0 0 0 0

countStatements :: AbsStmt.Program -> StmtStats
countStatements (AbsStmt.Prog stmts) = foldr countStmt emptyStats stmts

countStmt :: AbsStmt.Stmt -> StmtStats -> StmtStats
countStmt stmt stats = case stmt of
  AbsStmt.VarDecl _ _ -> stats {assignments = assignments stats + 1}
  AbsStmt.Assign _ _ -> stats {assignments = assignments stats + 1}
  AbsStmt.IfStmt _ s -> addIf $ countStmt s stats
  AbsStmt.IfElse _ s1 s2 -> addIf $ countStmt s1 $ countStmt s2 stats
  AbsStmt.While _ s -> addWhile $ countStmt s stats
  AbsStmt.For _ _ _ s -> addWhile $ countStmt s stats -- Count for loops as while
  AbsStmt.FunDef _ _ s -> addFun $ countStmt s stats
  AbsStmt.Block stmts -> foldr countStmt stats stmts
  AbsStmt.ExprStmt _ -> stats {expressions = expressions stats + 1}
  AbsStmt.Return _ -> stats {expressions = expressions stats + 1}
  AbsStmt.ReturnVoid -> stats {expressions = expressions stats + 1}
  AbsStmt.Print _ -> stats {expressions = expressions stats + 1}
  where
    addIf s = s {ifStatements = ifStatements s + 1}
    addWhile s = s {whileLoops = whileLoops s + 1}
    addFun s = s {functionDefs = functionDefs s + 1}

-- Parse and analyze statements
parseProgram :: String -> Either String StmtStats
parseProgram s = case ParStmt.pProgram (ParStmt.myLexer s) of
  ErrMStmt.Bad err -> Left err
  ErrMStmt.Ok prog -> Right (countStatements prog)

-- Interactive demo
main :: IO ()
main = do
  putStrLn "Multi-Grammar BNFC Demo"
  putStrLn "======================="
  putStrLn ""

  -- Expression examples
  putStrLn "Expression Parser Examples:"
  putStrLn "---------------------------"
  testExpression "2 + 3 * 4"
  testExpression "2^3 + sqrt(16)"
  testExpression "-(5 + 3) * 2"
  testExpression "10 % 3 + 1"
  putStrLn ""

  -- Statement examples
  putStrLn "Statement Parser Examples:"
  putStrLn "-------------------------"
  testProgram simpleProgram
  testProgram complexProgram
  putStrLn ""

  -- Interactive mode
  putStrLn "Interactive Mode:"
  putStrLn "Type 'expr: <expression>' to parse expressions"
  putStrLn "Type 'prog: <program>' to parse programs"
  putStrLn "Type 'quit' to exit"
  putStrLn ""
  interactiveLoop

testExpression :: String -> IO ()
testExpression expr = do
  putStr $ "  " ++ expr ++ " = "
  case parseExpression expr of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right result -> putStrLn $ show result

testProgram :: String -> IO ()
testProgram prog = do
  putStrLn $ "Program:"
  putStrLn $ unlines $ map ("  " ++) $ lines prog
  case parseProgram prog of
    Left err -> putStrLn $ "Parse error: " ++ err
    Right stats -> putStrLn $ "Statistics: " ++ show stats
  putStrLn ""

simpleProgram :: String
simpleProgram =
  unlines
    [ "var x = 10;",
      "var y = x + 5;",
      "print(y);"
    ]

complexProgram :: String
complexProgram =
  unlines
    [ "function factorial(n) {",
      "  if (n == 0) {",
      "    return 1;",
      "  } else {",
      "    return n * factorial(n - 1);",
      "  }",
      "}",
      "",
      "var result = factorial(5);",
      "print(result);"
    ]

interactiveLoop :: IO ()
interactiveLoop = do
  putStr "> "
  input <- getLine
  case input of
    "quit" -> putStrLn "Goodbye!"
    _ | take 5 input == "expr:" -> do
      let expr = drop 6 input
      case parseExpression expr of
        Left err -> putStrLn $ "Expression parse error: " ++ err
        Right result -> putStrLn $ "Result: " ++ show result
      interactiveLoop
    _ | take 5 input == "prog:" -> do
      let prog = drop 6 input
      case parseProgram prog of
        Left err -> putStrLn $ "Program parse error: " ++ err
        Right stats -> putStrLn $ "Statistics: " ++ show stats
      interactiveLoop
    _ -> do
      putStrLn "Unknown command. Use 'expr: <expression>', 'prog: <program>', or 'quit'"
      interactiveLoop
