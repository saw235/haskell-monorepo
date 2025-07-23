{-# LANGUAGE OverloadedStrings #-}

module Calculator (calculateExpression) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

calculateExpression :: Text -> Text
calculateExpression expr =
  case parseWithSpaces expr of
    Just result -> result
    Nothing -> parseWithoutSpaces expr
  where
    parseWithSpaces :: Text -> Maybe Text
    parseWithSpaces expr =
      case T.words expr of
        [a, "+", b] -> performOperation (+) a b
        [a, "-", b] -> performOperation (-) a b
        [a, "*", b] -> performOperation (*) a b
        [a, "/", b] -> performDivision a b
        _ -> Nothing

    parseWithoutSpaces :: Text -> Text
    parseWithoutSpaces expr =
      let exprStr = T.unpack expr
       in case parseSimpleExpr exprStr of
            Just result -> T.pack $ show result
            Nothing -> "Invalid expression format. Use: number operator number (e.g., '10 + 5' or '10+5')"

    performOperation :: (Double -> Double -> Double) -> Text -> Text -> Maybe Text
    performOperation op a b =
      case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
        (Just x, Just y) -> Just $ T.pack $ show (op x y)
        _ -> Just "Invalid numbers"

    performDivision :: Text -> Text -> Maybe Text
    performDivision a b =
      case (readMaybe (T.unpack a), readMaybe (T.unpack b)) of
        (Just x, Just y) ->
          if y /= 0
            then Just $ T.pack $ show (x / y :: Double)
            else Just "Division by zero"
        _ -> Just "Invalid numbers"

    parseSimpleExpr :: String -> Maybe Double
    parseSimpleExpr s =
      case break (`elem` ("+-*/" :: String)) s of
        (num1Str, op : num2Str) -> do
          num1 <- readMaybe num1Str
          num2 <- readMaybe num2Str
          case op of
            '+' -> Just (num1 + num2)
            '-' -> Just (num1 - num2)
            '*' -> Just (num1 * num2)
            '/' -> if num2 /= 0 then Just (num1 / num2) else Nothing
            _ -> Nothing
        _ -> Nothing
