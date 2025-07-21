{-# LANGUAGE RecursiveDo #-}

-- Simple Population Growth Model using Aivika
--
-- This example demonstrates exponential population growth using system dynamics.
-- The mathematical model is: dP/dt = r * P, where P is population and r is growth rate.

import Control.Monad.Trans
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics

-- | Simulation specifications
specs :: Specs
specs =
  Specs
    { spcStartTime = 0.0,
      spcStopTime = 20.0,
      spcDT = 0.1,
      spcMethod = RungeKutta4,
      spcGeneratorType = SimpleGenerator
    }

-- | Population growth rate (5% per year)
growthRate :: Double
growthRate = 0.05

-- | Initial population
initialPopulation :: Double
initialPopulation = 1000.0

-- | Population growth model
model :: Simulation Results
model = mdo
  -- Integrate the differential equation: dP/dt = r * P
  population <- integ ((* growthRate) <$> population) initialPopulation

  return $
    results
      [ resultSource "t" "Time (years)" time,
        resultSource "P" "Population" population
      ]

-- | Main function to run the simulation
main :: IO ()
main = do
  putStrLn "Aivika Population Growth Simulation"
  putStrLn "=================================="
  putStrLn "Model: dP/dt = r * P"
  putStrLn $ "Growth rate (r): " ++ show (growthRate * 100) ++ "% per year"
  putStrLn $ "Initial population (P0): " ++ show (round initialPopulation)
  putStrLn $ "Simulation time: " ++ show (spcStartTime specs) ++ " to " ++ show (spcStopTime specs) ++ " years"
  putStrLn $ "Expected final population: ~" ++ show (round (initialPopulation * exp (growthRate * spcStopTime specs)))
  putStrLn ""
  putStrLn "Results at key time points:"
  putStrLn "Time (years) | Population"
  putStrLn "-------------|------------"

  -- Print results at years 0, 5, 10, 15, 20
  let timePoints = [0, 5, 10, 15, 20]
  printSimulationResultsInTimes timePoints printResultSourceInEnglish model specs

  putStrLn ""
  putStrLn "Simulation complete!"
