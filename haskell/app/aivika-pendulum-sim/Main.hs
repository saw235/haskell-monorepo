{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

-- Simple Pendulum Simulation using Aivika
--
-- This example demonstrates a simple pendulum swing using system dynamics.
-- The mathematical model is: d²θ/dt² = -(g/L)sin(θ) - c*(dθ/dt), where:
-- θ is angle, L is length, g is gravity, c is damping coefficient.
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
  { pendulumLength :: Double,
    dampingCoeff :: Double,
    initialAngle :: Double,
    initialVelocity :: Double,
    timeEnd :: Double
  }
  deriving (Show)

data SimulationResult = SimulationResult
  { time :: Double,
    angle :: Double,
    angularVelocity :: Double,
    xPosition :: Double,
    yPosition :: Double
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
      <$> o .: "pendulumLength"
      <*> o .: "dampingCoeff"
      <*> o .: "initialAngle"
      <*> o .: "initialVelocity"
      <*> o .: "timeEnd"

instance ToJSON SimulationParams where
  toJSON (SimulationParams len damp initAngle initVel endTime) =
    object
      [ "pendulumLength" .= len,
        "dampingCoeff" .= damp,
        "initialAngle" .= initAngle,
        "initialVelocity" .= initVel,
        "timeEnd" .= endTime
      ]

instance ToJSON SimulationResult where
  toJSON (SimulationResult t theta omega x y) =
    object
      [ "time" .= t,
        "angle" .= theta,
        "angularVelocity" .= omega,
        "xPosition" .= x,
        "yPosition" .= y
      ]

-- | Command line parser
options :: Parser Options
options =
  Options
    <$> (flag CLI Server (long "server" <> short 's' <> help "Run in WebSocket server mode"))
    <*> option auto (long "port" <> short 'p' <> value 9162 <> help "Port number (default: 9162)")

-- | Get parameter from environment variable or use default
getEnvDouble :: String -> Double -> IO Double
getEnvDouble envVar defaultVal = do
  maybeVal <- lookupEnv envVar
  case maybeVal >>= readMaybe of
    Just val -> return val
    Nothing -> return defaultVal

-- | Pendulum model
model :: Double -> Double -> Double -> Double -> Simulation Results
model pendulumLen dampingC initAngle initVel = mdo
  let g = 9.81  -- gravitational acceleration (m/s²)

  -- Angular velocity differential equation: dω/dt = -(g/L)sin(θ) - c*ω
  let dOmega = do
        theta' <- theta
        omega' <- omega
        return $ -(g / pendulumLen) * sin theta' - dampingC * omega'

  -- Angular position differential equation: dθ/dt = ω
  let dTheta = do
        omega' <- omega
        return omega'

  -- Integrate the equations
  omega <- integ dOmega (return initVel)
  theta <- integ dTheta (return initAngle)

  -- Calculate Cartesian coordinates for visualization
  let xPos = do
        theta' <- theta
        return $ pendulumLen * sin theta'
  
  let yPos = do
        theta' <- theta
        return $ -pendulumLen * cos theta'  -- Negative because y=0 is at the top

  return $
    results
      [ resultSource "t" "Time (seconds)" Simulation.Aivika.time,
        resultSource "theta" "Angle (radians)" theta,
        resultSource "omega" "Angular Velocity (rad/s)" omega,
        resultSource "x" "X Position (m)" xPos,
        resultSource "y" "Y Position (m)" yPos
      ]

-- | Run simulation and return data points using analytical approximation
runPendulumSimulation :: SimulationParams -> IO [SimulationResult]
runPendulumSimulation (SimulationParams len damp initAngle initVel endTime) = do
  let dt = 0.05  -- Time step for output
      timePoints = [0, dt .. endTime]
      
  return $ map (calculatePendulumState len damp initAngle initVel) timePoints
  where
    calculatePendulumState l c theta0 omega0 t =
      -- For small angles, approximate with harmonic oscillator with damping
      let omega_n = sqrt (9.81 / l)  -- Natural frequency
          zeta = c / (2 * sqrt (9.81 / l))  -- Damping ratio
          omega_d = omega_n * sqrt (1 - zeta^2)  -- Damped frequency
          
          -- Damped oscillation solution
          theta = if zeta < 1
                 then exp (-zeta * omega_n * t) * 
                      (theta0 * cos (omega_d * t) + 
                       ((omega0 + zeta * omega_n * theta0) / omega_d) * sin (omega_d * t))
                 else theta0 * exp (-omega_n * t)  -- Overdamped case simplified
          
          omega = if zeta < 1
                 then exp (-zeta * omega_n * t) * 
                      (-theta0 * omega_d * sin (omega_d * t) + 
                       (omega0 + zeta * omega_n * theta0) * cos (omega_d * t)) -
                      zeta * omega_n * theta
                 else -omega_n * theta  -- Overdamped case simplified
          
          x = l * sin theta
          y = -l * cos theta
          
      in SimulationResult t theta omega x y

-- | Create response message
createResponse :: Text -> Text -> L8.ByteString
createResponse msgType content = encode $ object ["type" .= msgType, "content" .= content]

-- | Handle client messages
handleClientMessage :: ClientMessage -> IO L8.ByteString
handleClientMessage (ClientMessage "simulate" content) = do
  case decode (L8.pack $ T.unpack content) of
    Just params -> do
      results <- runPendulumSimulation params
      return $ encode $ object ["type" .= ("simulation_result" :: Text), "results" .= results]
    Nothing ->
      return $ createResponse "error" "Invalid simulation parameters"
handleClientMessage (ClientMessage "ping" _) =
  return $ createResponse "pong" "Pong from Aivika pendulum server!"
handleClientMessage (ClientMessage "help" _) =
  return $ createResponse "help_response" helpText
handleClientMessage _ =
  return $ createResponse "error" "Unknown message format. Try sending 'help' for available commands."

helpText :: Text
helpText = "Available commands:\n• ping - Test connectivity\n• simulate {\"pendulumLength\":1.0,\"dampingCoeff\":0.1,\"initialAngle\":0.785,\"initialVelocity\":0.0,\"timeEnd\":10} - Run pendulum simulation\n• help - Show this help message"

-- | WebSocket application
application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    putStrLn "New client connected to Aivika pendulum server!"
    sendWelcomeMessage conn
    handleMessages conn

sendWelcomeMessage :: WS.Connection -> IO ()
sendWelcomeMessage conn = do
  let welcomeMsg = createResponse "welcome" "Hello from Aivika Pendulum Simulation server!"
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
  pendulumLength <- getEnvDouble "PENDULUM_LENGTH" 1.0
  dampingCoeff <- getEnvDouble "DAMPING_COEFF" 0.1
  initialAngle <- getEnvDouble "INITIAL_ANGLE" 0.785  -- 45 degrees in radians
  initialVelocity <- getEnvDouble "INITIAL_VELOCITY" 0.0
  timeEnd <- getEnvDouble "TIME_END" 10.0

  putStrLn "Aivika Simple Pendulum Simulation"
  putStrLn "================================="
  putStrLn "Model: d²θ/dt² = -(g/L)sin(θ) - c*(dθ/dt)"
  putStrLn $ "Pendulum length (L): " ++ show pendulumLength ++ " m"
  putStrLn $ "Damping coefficient (c): " ++ show dampingCoeff
  putStrLn $ "Initial angle (θ₀): " ++ show initialAngle ++ " rad (" ++ show (initialAngle * 180 / pi) ++ "°)"
  putStrLn $ "Initial angular velocity (ω₀): " ++ show initialVelocity ++ " rad/s"
  putStrLn $ "Simulation time: 0.0 to " ++ show timeEnd ++ " seconds"
  putStrLn ""
  putStrLn "Results at time points:"
  putStrLn "Time (s) | Angle (rad) | Angle (°) | Angular Velocity (rad/s) | X (m) | Y (m)"
  putStrLn "---------|-------------|----------|--------------------------|-------|-------"

  let params = SimulationParams pendulumLength dampingCoeff initialAngle initialVelocity timeEnd
  results <- runPendulumSimulation params
  
  -- Sample every 10th point to avoid too much output
  let sampledResults = [r | (i, r) <- zip [0..] results, i `mod` 20 == 0]
  
  mapM_ printResult sampledResults

  putStrLn ""
  putStrLn "Simulation complete!"
  where
    printResult (SimulationResult t theta omega x y) =
      putStrLn $ show t ++ " | " ++ 
                 show theta ++ " | " ++
                 show (theta * 180 / pi) ++ " | " ++
                 show omega ++ " | " ++
                 show x ++ " | " ++
                 show y

-- | Server mode execution
runServer :: Int -> IO ()
runServer port = do
  putStrLn $ "Starting Aivika Pendulum Simulation WebSocket server on localhost:" ++ show port
  WS.runServer "127.0.0.1" port application

-- | Main function
main :: IO ()
main = do
  opts <-
    execParser $
      info
        (options <**> helper)
        (fullDesc <> progDesc "Aivika Simple Pendulum Simulation" <> header "aivika-pendulum-sim - pendulum swing simulation")

  case optMode opts of
    CLI -> runCLI
    Server -> runServer (optPort opts)