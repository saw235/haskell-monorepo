module Main where

-- Import generated BNFC modules
import AbsCalc
import ErrMCalc
import LexCalc
import ParCalc
import PrintCalc
import Test.QuickCheck
import Test.QuickCheck.Property (property)

-- Re-export evaluation function from Main for testing
eval :: Exp -> Integer
eval e = case e of
  EAdd e1 e2 -> eval e1 + eval e2
  ESub e1 e2 -> eval e1 - eval e2
  EMul e1 e2 -> eval e1 * eval e2
  EDiv e1 e2 -> eval e1 `div` eval e2
  EInt n -> n

-- Parse function for testing
parseExpr :: String -> Either String Exp
parseExpr s = case pExp (myLexer s) of
  Bad err -> Left err
  Ok expr -> Right expr

-- Arbitrary instance for generating test expressions
instance Arbitrary Exp where
  arbitrary = sized arbExp
    where
      arbExp 0 = EInt <$> arbitrary
      arbExp n =
        oneof
          [ EInt <$> arbitrary,
            EAdd <$> subExp <*> subExp,
            ESub <$> subExp <*> subExp,
            EMul <$> subExp <*> subExp,
            EDiv <$> subExp <*> nonZeroExp -- Avoid division by zero
          ]
        where
          subExp = arbExp (n `div` 2)
          nonZeroExp = arbExp (n `div` 2) `suchThat` (\e -> eval e /= 0)

  shrink (EAdd e1 e2) = [e1, e2] ++ [EAdd e1' e2 | e1' <- shrink e1] ++ [EAdd e1 e2' | e2' <- shrink e2]
  shrink (ESub e1 e2) = [e1, e2] ++ [ESub e1' e2 | e1' <- shrink e1] ++ [ESub e1 e2' | e2' <- shrink e2]
  shrink (EMul e1 e2) = [e1, e2] ++ [EMul e1' e2 | e1' <- shrink e1] ++ [EMul e1 e2' | e2' <- shrink e2]
  shrink (EDiv e1 e2) = [e1, e2] ++ [EDiv e1' e2 | e1' <- shrink e1] ++ [EDiv e1 e2' | e2' <- shrink e2]
  shrink (EInt n) = [EInt n' | n' <- shrink n]

-- Property: Parser round-trip (parse . print = identity)
-- Note: Only test with non-negative integers since BNFC printer generates
-- negative numbers as "-1" but our parser doesn't handle unary minus at top level
prop_parseRoundTrip :: Exp -> Property
prop_parseRoundTrip expr =
  hasOnlyNonNegativeInts expr
    ==> case parseExpr (printTree expr) of
      Left err -> counterexample ("Parse failed: " ++ err ++ " for: " ++ printTree expr) False
      Right parsed -> parsed === expr
  where
    hasOnlyNonNegativeInts (EInt n) = n >= 0
    hasOnlyNonNegativeInts (EAdd e1 e2) = hasOnlyNonNegativeInts e1 && hasOnlyNonNegativeInts e2
    hasOnlyNonNegativeInts (ESub e1 e2) = hasOnlyNonNegativeInts e1 && hasOnlyNonNegativeInts e2
    hasOnlyNonNegativeInts (EMul e1 e2) = hasOnlyNonNegativeInts e1 && hasOnlyNonNegativeInts e2
    hasOnlyNonNegativeInts (EDiv e1 e2) = hasOnlyNonNegativeInts e1 && hasOnlyNonNegativeInts e2

-- Property: Evaluator produces finite results for valid expressions
prop_evalFinite :: Exp -> Property
prop_evalFinite expr =
  property $
    let result = eval expr
     in result `seq` True -- Force evaluation, should not crash

-- Property: Addition is commutative
prop_addCommutative :: Exp -> Exp -> Property
prop_addCommutative e1 e2 = eval (EAdd e1 e2) === eval (EAdd e2 e1)

-- Property: Addition is associative
prop_addAssociative :: Exp -> Exp -> Exp -> Property
prop_addAssociative e1 e2 e3 =
  eval (EAdd (EAdd e1 e2) e3) === eval (EAdd e1 (EAdd e2 e3))

-- Property: Multiplication is commutative
prop_mulCommutative :: Exp -> Exp -> Property
prop_mulCommutative e1 e2 = eval (EMul e1 e2) === eval (EMul e2 e1)

-- Property: Multiplication is associative
prop_mulAssociative :: Exp -> Exp -> Exp -> Property
prop_mulAssociative e1 e2 e3 =
  eval (EMul (EMul e1 e2) e3) === eval (EMul e1 (EMul e2 e3))

-- Property: Multiplication distributes over addition
prop_mulDistributive :: Exp -> Exp -> Exp -> Property
prop_mulDistributive e1 e2 e3 =
  eval (EMul e1 (EAdd e2 e3)) === eval (EAdd (EMul e1 e2) (EMul e1 e3))

-- Property: Adding zero is identity
prop_addZeroIdentity :: Exp -> Property
prop_addZeroIdentity expr =
  eval (EAdd expr (EInt 0))
    === eval expr
    .&&. eval (EAdd (EInt 0) expr)
    === eval expr

-- Property: Multiplying by one is identity
prop_mulOneIdentity :: Exp -> Property
prop_mulOneIdentity expr =
  eval (EMul expr (EInt 1))
    === eval expr
    .&&. eval (EMul (EInt 1) expr)
    === eval expr

-- Property: Multiplying by zero gives zero
prop_mulZero :: Exp -> Property
prop_mulZero expr =
  eval (EMul expr (EInt 0))
    === 0
    .&&. eval (EMul (EInt 0) expr)
    === 0

-- Property: Subtracting from itself gives zero
prop_subSelf :: Exp -> Property
prop_subSelf expr = eval (ESub expr expr) === 0

-- Property: Dividing by itself gives one (for non-zero expressions)
prop_divSelf :: Exp -> Property
prop_divSelf expr =
  eval expr /= 0 ==> eval (EDiv expr expr) === 1

-- Property: Division is inverse of multiplication (for non-zero divisors)
prop_divMulInverse :: Exp -> Exp -> Property
prop_divMulInverse e1 e2 =
  eval e2 /= 0 ==> eval (EDiv (EMul e1 e2) e2) === eval e1

-- Property: Simple arithmetic consistency checks
prop_simpleArithmetic :: Integer -> Integer -> Property
prop_simpleArithmetic n m =
  m
    /= 0
      ==> conjoin
        [ eval (EAdd (EInt n) (EInt m)) === n + m,
          eval (ESub (EInt n) (EInt m)) === n - m,
          eval (EMul (EInt n) (EInt m)) === n * m,
          eval (EDiv (EInt n) (EInt m)) === n `div` m
        ]

-- Property: Parser handles basic expressions correctly
prop_parseBasicExpressions :: Property
prop_parseBasicExpressions =
  conjoin
    [ parseExpr "42" === Right (EInt 42),
      parseExpr "1 + 2" === Right (EAdd (EInt 1) (EInt 2)),
      parseExpr "3 * 4" === Right (EMul (EInt 3) (EInt 4)),
      parseExpr "10 - 5" === Right (ESub (EInt 10) (EInt 5)),
      parseExpr "8 / 2" === Right (EDiv (EInt 8) (EInt 2))
    ]

-- Property: Parser respects operator precedence
prop_operatorPrecedence :: Property
prop_operatorPrecedence =
  conjoin
    [ parseExpr "2 + 3 * 4" === Right (EAdd (EInt 2) (EMul (EInt 3) (EInt 4))),
      parseExpr "2 * 3 + 4" === Right (EAdd (EMul (EInt 2) (EInt 3)) (EInt 4)),
      parseExpr "10 - 6 / 2" === Right (ESub (EInt 10) (EDiv (EInt 6) (EInt 2)))
    ]

-- Main function to run all tests
main :: IO ()
main = do
  putStrLn "Running QuickCheck tests for BNFC Calculator..."
  putStrLn ""

  putStrLn "Testing parser round-trip property..."
  quickCheck prop_parseRoundTrip

  putStrLn "Testing evaluator produces finite results..."
  quickCheck prop_evalFinite

  putStrLn "Testing addition commutativity..."
  quickCheck prop_addCommutative

  putStrLn "Testing addition associativity..."
  quickCheck prop_addAssociative

  putStrLn "Testing multiplication commutativity..."
  quickCheck prop_mulCommutative

  putStrLn "Testing multiplication associativity..."
  quickCheck prop_mulAssociative

  putStrLn "Testing multiplication distributivity..."
  quickCheck prop_mulDistributive

  putStrLn "Testing addition zero identity..."
  quickCheck prop_addZeroIdentity

  putStrLn "Testing multiplication one identity..."
  quickCheck prop_mulOneIdentity

  putStrLn "Testing multiplication by zero..."
  quickCheck prop_mulZero

  putStrLn "Testing subtraction self..."
  quickCheck prop_subSelf

  putStrLn "Testing division self..."
  quickCheck prop_divSelf

  putStrLn "Testing division-multiplication inverse..."
  quickCheck prop_divMulInverse

  putStrLn "Testing simple arithmetic..."
  quickCheck prop_simpleArithmetic

  putStrLn "Testing basic expression parsing..."
  quickCheck prop_parseBasicExpressions

  putStrLn "Testing operator precedence..."
  quickCheck prop_operatorPrecedence

  putStrLn ""
  putStrLn "All QuickCheck tests completed!"
