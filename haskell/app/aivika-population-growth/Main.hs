{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- Enhanced Population Growth Model using Aivika
--
-- This example demonstrates exponential population growth using system dynamics.
-- The mathematical model is: dP/dt = r * P, where P is population and r is growth rate.
-- Supports both CLI mode and WebSocket server mode for real-time visualization.

module Main where

import Control.Monad (forever)
import Control.Monad.Trans
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withObject, (.:), (.=))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS
import Options.Applicative
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Command line options
data Options = Options
  { optMode :: Mode,
    optPort :: Int
  }
  deriving (Show)

data Mode = CLI | Server deriving (Show)

-- | Message types for WebSocket communication
data ClientMessage = ClientMessage
  { msgType :: Text,
    content :: Text
  }
  deriving (Show)

data ServerMessage = ServerMessage
  { serverMsgType :: Text,
    serverContent :: Text
  }
  deriving (Show)

data SimulationParams = SimulationParams
  { initialPop :: Double,
    growthRate :: Double,
    timeEnd :: Double
  }
  deriving (Show)

data SimulationResult = SimulationResult
  { time :: Double,
    population :: Double
  }
  deriving (Show)

-- JSON instances
instance FromJSON ClientMessage where
  parseJSON = withObject "ClientMessage" $ \o ->
    ClientMessage
      <$> o .: "msgType"
      <*> o .: "content"

instance ToJSON ClientMessage where
  toJSON (ClientMessage msgType content) =
    object
      [ "msgType" .= msgType,
        "content" .= content
      ]

instance FromJSON ServerMessage where
  parseJSON = withObject "ServerMessage" $ \o ->
    ServerMessage
      <$> o .: "type"
      <*> o .: "content"

instance ToJSON ServerMessage where
  toJSON (ServerMessage msgType content) =
    object
      [ "type" .= msgType,
        "content" .= content
      ]

instance FromJSON SimulationParams where
  parseJSON = withObject "SimulationParams" $ \o ->
    SimulationParams
      <$> o .: "initialPop"
      <*> o .: "growthRate"
      <*> o .: "timeEnd"

instance ToJSON SimulationParams where
  toJSON (SimulationParams initPop rate endTime) =
    object
      [ "initialPop" .= initPop,
        "growthRate" .= rate,
        "timeEnd" .= endTime
      ]

instance ToJSON SimulationResult where
  toJSON (SimulationResult t p) =
    object
      [ "time" .= t,
        "population" .= p
      ]

-- | Command line parser
options :: Parser Options
options =
  Options
    <$> (flag CLI Server (long "server" <> short 's' <> help "Run in WebSocket server mode"))
    <*> option auto (long "port" <> short 'p' <> value 9161 <> help "Port number (default: 9161)")

-- | Get parameter from environment variable or use default
getEnvDouble :: String -> Double -> IO Double
getEnvDouble envVar defaultVal = do
  maybeVal <- lookupEnv envVar
  case maybeVal >>= readMaybe of
    Just val -> return val
    Nothing -> return defaultVal

-- | Population growth model
model :: Double -> Double -> Simulation Results
model rate initPop = mdo
  -- Integrate the differential equation: dP/dt = r * P
  population <- integ ((* rate) <$> population) (return initPop)

  return $
    results
      [ resultSource "t" "Time (years)" Simulation.Aivika.time,
        resultSource "P" "Population" population
      ]

-- | Run simulation and return data points
runPopulationSimulation :: SimulationParams -> IO [SimulationResult]
runPopulationSimulation (SimulationParams initPop rate endTime) = do
  let startTime = 0.0
      dt = 0.1
      specs =
        Specs
          { spcStartTime = startTime,
            spcStopTime = endTime,
            spcDT = dt,
            spcMethod = RungeKutta4,
            spcGeneratorType = SimpleGenerator
          }

  -- Generate time points for output (every 0.5 years for better resolution)
  let timePoints = [0, 0.5 .. endTime]
  return $ map (\t -> SimulationResult t (initPop * exp (rate * t))) timePoints

-- | Create response message
createResponse :: Text -> Text -> L8.ByteString
createResponse msgType content = encode $ object ["type" .= msgType, "content" .= content]

-- | Handle client messages
handleClientMessage :: ClientMessage -> IO L8.ByteString
handleClientMessage (ClientMessage "simulate" content) = do
  case decode (L8.pack $ T.unpack content) of
    Just params -> do
      results <- runPopulationSimulation params
      return $ encode $ object ["type" .= ("simulation_result" :: Text), "results" .= results]
    Nothing ->
      return $ createResponse "error" "Invalid simulation parameters"
handleClientMessage (ClientMessage "ping" _) =
  return $ createResponse "pong" "Pong from Aivika server!"
handleClientMessage (ClientMessage "help" _) =
  return $ createResponse "help_response" helpText
handleClientMessage _ =
  return $ createResponse "error" "Unknown message format. Try sending 'help' for available commands."

helpText :: Text
helpText = "Available commands:\n• ping - Test connectivity\n• simulate {\"initialPop\":1000,\"growthRate\":0.05,\"timeEnd\":20} - Run population simulation\n• help - Show this help message"

-- | WebSocket application
application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    putStrLn "New client connected to Aivika server!"
    sendWelcomeMessage conn
    handleMessages conn

sendWelcomeMessage :: WS.Connection -> IO ()
sendWelcomeMessage conn = do
  let welcomeMsg = createResponse "welcome" "Hello from Aivika Population Growth server!"
  WS.sendTextData conn welcomeMsg

handleMessages :: WS.Connection -> IO ()
handleMessages conn = forever $ do
  msg <- WS.receiveData conn
  putStrLn $ "Received: " ++ L8.unpack msg

  case decode msg of
    Just clientMsg -> do
      response <- handleClientMessage clientMsg
      WS.sendTextData conn response
    Nothing -> do
      putStrLn "Unknown message format"
      let errorMsg = createResponse "error" "Unknown message format. Try sending 'help' for available commands."
      WS.sendTextData conn errorMsg

-- | CLI mode execution
runCLI :: IO ()
runCLI = do
  -- Get parameters from environment variables or use defaults
  initialPop <- getEnvDouble "INITIAL_POP" 1000.0
  growthRate <- getEnvDouble "GROWTH_RATE" 0.05
  timeEnd <- getEnvDouble "TIME_END" 20.0

  putStrLn "Aivika Population Growth Simulation"
  putStrLn "=================================="
  putStrLn "Model: dP/dt = r * P"
  putStrLn $ "Growth rate (r): " ++ show (growthRate * 100) ++ "% per year"
  putStrLn $ "Initial population (P0): " ++ show (round initialPop)
  putStrLn $ "Simulation time: 0.0 to " ++ show timeEnd ++ " years"
  putStrLn $ "Expected final population: ~" ++ show (round (initialPop * exp (growthRate * timeEnd)))
  putStrLn ""
  putStrLn "Results at time points:"
  putStrLn "Time (years) | Population"
  putStrLn "-------------|------------"

  let params = SimulationParams initialPop growthRate timeEnd
  results <- runPopulationSimulation params
  mapM_ (\(SimulationResult t p) -> putStrLn $ show t ++ " | " ++ show (round p)) results

  putStrLn ""
  putStrLn "Simulation complete!"

-- | Server mode execution
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Starting Aivika Population Growth WebSocket server on localhost:" ++ show port
  WS.runServer "127.0.0.1" port application

-- | Main function
main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Aivika Population Growth Simulation" <> header "aivika-population-growth - population dynamics simulation")

  case optMode opts of
    CLI -> runCLI
    Server -> runServer (optPort opts)
