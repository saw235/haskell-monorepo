{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : AgenticFramework.Workflow.Tools
-- Description : LangChain tool abstraction layer
-- Copyright   : (c) 2025
-- License     : MIT
--
-- Provides an abstraction layer for tool invocation that bridges
-- the workflow system with LangChain tools. Implements FR-008.
module AgenticFramework.Workflow.Tools
  ( -- * Tool Abstraction
    ToolExecutor (..),
    ToolInput (..),
    ToolOutput (..),

    -- * Tool Invocation
    invokeTool,
    invokeToolSafe,
    invokeToolWithTimeout,

    -- * Tool Registration
    ToolRegistry,
    emptyRegistry,
    registerTool,
    lookupTool,
    listTools,

    -- * Tool Builders
    makeTool,
    makeToolFromFunction,

    -- * Standard Tools
    echoTool,
    calculatorTool,
  )
where

import AgenticFramework.Types (Tool (..), ToolSchema (..))
import qualified AgenticFramework.Types as AgenticTypes
import AgenticFramework.Workflow.Types (Workflow, WorkflowError (..))
import Control.Exception (SomeException, catch, try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Input to a tool execution
data ToolInput = ToolInput
  { tiName :: Text, -- Tool name
    tiArgs :: Value, -- Tool arguments as JSON
    tiContext :: Maybe Value -- Optional context
  }
  deriving (Show, Eq)

-- | Output from a tool execution
data ToolOutput = ToolOutput
  { toResult :: Value, -- Result as JSON
    toMetadata :: Maybe Value -- Optional metadata
  }
  deriving (Show, Eq)

-- | Type class for tool executors
class ToolExecutor m where
  executeTool :: Tool -> ToolInput -> m (Either AgenticTypes.ToolError ToolOutput)

-- | Direct IO executor
instance ToolExecutor IO where
  executeTool tool input = do
    let toolInput = AgenticTypes.ToolInput (tiArgs input)
    result <- try $ (toolExecute tool) toolInput
    case result of
      Left (e :: SomeException) ->
        return $ Left $ AgenticTypes.ToolExecutionError (T.pack $ show e)
      Right (Left err) ->
        return $ Left err
      Right (Right output) ->
        return $ Right $ ToolOutput (AgenticTypes.unToolOutput output) Nothing

-- | Tool registry for managing available tools
newtype ToolRegistry = ToolRegistry
  { unRegistry :: Map Text Tool
  }
  deriving (Show)

-- | Create an empty tool registry
emptyRegistry :: ToolRegistry
emptyRegistry = ToolRegistry Map.empty

-- | Register a tool in the registry
registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool tool (ToolRegistry reg) =
  ToolRegistry $ Map.insert (toolName tool) tool reg

-- | Look up a tool by name
lookupTool :: Text -> ToolRegistry -> Maybe Tool
lookupTool name (ToolRegistry reg) = Map.lookup name reg

-- | List all registered tools
listTools :: ToolRegistry -> [Tool]
listTools (ToolRegistry reg) = Map.elems reg

-- | Invoke a tool within a workflow
invokeTool :: Tool -> Value -> Workflow Value
invokeTool tool args = do
  let input = AgenticTypes.ToolInput args
  result <- liftIO $ (toolExecute tool) input
  case result of
    Left _err -> return $ Aeson.String "Tool execution failed"
    Right output -> return $ AgenticTypes.unToolOutput output

-- | Invoke a tool safely, returning an Either
invokeToolSafe :: Tool -> Value -> Workflow (Either WorkflowError Value)
invokeToolSafe tool args = do
  let input = AgenticTypes.ToolInput args
  result <- liftIO $ try $ (toolExecute tool) input
  case result of
    Left (e :: SomeException) ->
      return $ Left $ ExecutionError $ T.pack $ show e
    Right (Left _err) ->
      return $ Left $ ExecutionError "Tool execution failed"
    Right (Right output) ->
      return $ Right $ AgenticTypes.unToolOutput output

-- | Invoke a tool with a timeout (in microseconds)
invokeToolWithTimeout :: Int -> Tool -> Value -> Workflow (Maybe Value)
invokeToolWithTimeout _timeoutMicros tool args = do
  -- For now, just invoke without timeout
  -- A proper implementation would use race from async
  let input = AgenticTypes.ToolInput args
  result <- liftIO $ (toolExecute tool) input
  case result of
    Left _err -> return Nothing
    Right output -> return $ Just $ AgenticTypes.unToolOutput output

-- | Create a tool from basic parameters
makeTool :: Text -> Text -> (Value -> IO Value) -> Tool
makeTool name desc fn =
  Tool
    { toolName = name,
      toolDescription = desc,
      toolSchema = ToolSchema Aeson.Null Aeson.Null,
      toolExecute = \input -> do
        result <- fn (AgenticTypes.unToolInput input)
        return $ Right $ AgenticTypes.ToolOutput result,
      toolTimeout = Nothing,
      toolRetryable = True
    }

-- | Create a tool from a pure function
makeToolFromFunction :: (FromJSON a, ToJSON b) => Text -> Text -> (a -> b) -> Tool
makeToolFromFunction name desc fn =
  makeTool name desc $ \input ->
    case Aeson.fromJSON input of
      Aeson.Error err -> return $ Aeson.String $ "Parse error: " <> T.pack err
      Aeson.Success a -> return $ Aeson.toJSON (fn a)

-- | A simple echo tool for testing
echoTool :: Tool
echoTool =
  makeTool
    "echo"
    "Echoes back the input"
    (\input -> return input)

-- | A simple calculator tool for testing
calculatorTool :: Tool
calculatorTool =
  makeTool
    "calculator"
    "Performs basic arithmetic operations"
    $ \input -> do
      case Aeson.fromJSON input :: Aeson.Result CalcInput of
        Aeson.Error err -> return $ Aeson.String $ "Error: " <> T.pack err
        Aeson.Success calc -> return $ Aeson.toJSON $ evalCalc calc

-- | Calculator input type
data CalcInput = CalcInput
  { operation :: Text,
    operand1 :: Double,
    operand2 :: Double
  }
  deriving (Show)

instance FromJSON CalcInput where
  parseJSON = Aeson.withObject "CalcInput" $ \v ->
    CalcInput
      <$> v Aeson..: "operation"
      <*> v Aeson..: "operand1"
      <*> v Aeson..: "operand2"

-- | Evaluate a calculator operation
evalCalc :: CalcInput -> Double
evalCalc calc = case operation calc of
  "add" -> operand1 calc + operand2 calc
  "sub" -> operand1 calc - operand2 calc
  "mul" -> operand1 calc * operand2 calc
  "div" ->
    if operand2 calc == 0
      then 0 / 0 -- NaN for division by zero
      else operand1 calc / operand2 calc
  _ -> 0 / 0 -- NaN for unknown operation
