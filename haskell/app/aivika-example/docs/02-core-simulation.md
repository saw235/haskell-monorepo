# Core Simulation Module: Simulation.Aivika

## Overview

The `Simulation.Aivika` module is the foundation of the Aivika library, providing the core simulation framework, event handling, and basic simulation primitives.

## Key Types and Data Structures

### **Simulation Monad**
```haskell
type Simulation = EventIO
type EventIO = Event (IO a)
```

The `Simulation` monad is the primary type for building simulations. It combines event handling with IO operations.

### **Simulation Specs**
```haskell
data Specs = Specs
  { spcStartTime :: Double      -- Start time of simulation
  , spcStopTime :: Double       -- End time of simulation
  , spcDT :: Double            -- Time step for continuous processes
  , spcMethod :: Method        -- Integration method
  , spcGeneratorType :: GeneratorType  -- Random number generator type
  }
```

### **Integration Methods**
```haskell
data Method
  = Euler                    -- Simple Euler method
  | RungeKutta2             -- Second-order Runge-Kutta
  | RungeKutta4             -- Fourth-order Runge-Kutta (recommended)
  | RungeKuttaFehlberg45    -- Adaptive step size method
```

### **Generator Types**
```haskell
data GeneratorType
  = SimpleGenerator         -- Simple random number generator
  | CustomGenerator         -- Custom generator with seed
```

## Core Functions

### **Simulation Execution**

#### `runSimulation`
```haskell
runSimulation :: Specs -> Simulation a -> IO a
```
Runs a simulation with the given specifications.

**Usage:**
```haskell
main = do
  let specs = Specs { spcStartTime = 0.0, spcStopTime = 10.0, spcDT = 0.1, spcMethod = RungeKutta4, spcGeneratorType = SimpleGenerator }
  result <- runSimulation specs myModel
  print result
```

#### `runSimulationWithResults`
```haskell
runSimulationWithResults :: Specs -> Simulation Results -> IO Results
```
Runs a simulation and returns structured results.

### **Time Management**

#### `time`
```haskell
time :: Simulation Double
```
Returns the current simulation time.

**Usage:**
```haskell
model = do
  t <- time
  -- Use current time in simulation
```

#### `timeStep`
```haskell
timeStep :: Simulation Double
```
Returns the current time step (DT).

### **Event Scheduling**

#### `scheduleEvent`
```haskell
scheduleEvent :: Double -> Simulation a -> Simulation ()
```
Schedules an event to occur at a specific time.

**Usage:**
```haskell
model = do
  scheduleEvent 5.0 $ do
    putStrLn "Event occurred at time 5.0"
```

#### `scheduleEventWithDelay`
```haskell
scheduleEventWithDelay :: Double -> Simulation a -> Simulation ()
```
Schedules an event to occur after a delay.

**Usage:**
```haskell
model = do
  scheduleEventWithDelay 2.0 $ do
    putStrLn "Event occurred 2.0 time units later"
```

### **Random Number Generation**

#### `randomUniform`
```haskell
randomUniform :: Double -> Double -> Simulation Double
```
Generates a uniform random number between two values.

**Usage:**
```haskell
model = do
  r <- randomUniform 0.0 1.0
  -- r is a random number between 0.0 and 1.0
```

#### `randomExponential`
```haskell
randomExponential :: Double -> Simulation Double
```
Generates an exponentially distributed random number.

**Usage:**
```haskell
model = do
  arrivalTime <- randomExponential 1.0  -- Mean = 1.0
```

#### `randomNormal`
```haskell
randomNormal :: Double -> Double -> Simulation Double
```
Generates a normally distributed random number.

**Usage:**
```haskell
model = do
  value <- randomNormal 0.0 1.0  -- Mean = 0.0, StdDev = 1.0
```

## Advanced Functions

### **Conditional Events**

#### `scheduleEventIf`
```haskell
scheduleEventIf :: Simulation Bool -> Simulation a -> Simulation ()
```
Schedules an event that only occurs if a condition is met.

**Usage:**
```haskell
model = do
  scheduleEventIf (condition >>= return) $ do
    putStrLn "Conditional event occurred"
```

### **Event Cancellation**

#### `cancelEvent`
```haskell
cancelEvent :: EventId -> Simulation Bool
```
Cancels a previously scheduled event.

**Usage:**
```haskell
model = do
  eventId <- scheduleEvent 10.0 someAction
  -- Later...
  cancelled <- cancelEvent eventId
```

### **Simulation State**

#### `getSimulationState`
```haskell
getSimulationState :: Simulation SimulationState
```
Retrieves the current simulation state.

#### `setSimulationState`
```haskell
setSimulationState :: SimulationState -> Simulation ()
```
Sets the simulation state.

## Common Patterns

### **Periodic Events**
```haskell
periodicEvent :: Double -> Simulation a -> Simulation ()
periodicEvent interval action = do
  scheduleEventWithDelay interval $ do
    action
    periodicEvent interval action  -- Schedule next occurrence
```

### **Conditional Looping**
```haskell
whileM :: Simulation Bool -> Simulation a -> Simulation ()
whileM condition action = do
  shouldContinue <- condition
  if shouldContinue
    then do
      action
      whileM condition action
    else return ()
```

### **State Tracking**
```haskell
trackState :: (a -> Simulation ()) -> Simulation a -> Simulation a
trackState tracker action = do
  result <- action
  tracker result
  return result
```

## Error Handling

### **Exception Handling**
```haskell
catchSimulation :: Simulation a -> (SomeException -> Simulation a) -> Simulation a
```
Catches exceptions in simulation code.

**Usage:**
```haskell
model = do
  catchSimulation riskyOperation $ \e -> do
    putStrLn $ "Error occurred: " ++ show e
    return defaultValue
```

### **Error Recovery**
```haskell
recoverSimulation :: Simulation a -> Simulation a -> Simulation a
```
Provides a fallback for failed simulations.

## Performance Considerations

### **Event Optimization**
- Minimize the number of scheduled events
- Use appropriate time steps for continuous processes
- Batch related events when possible

### **Memory Management**
- Use lazy evaluation for large result sets
- Avoid accumulating large data structures in memory
- Profile memory usage for long-running simulations

### **Random Number Generation**
- Use appropriate random number generators for your use case
- Consider using custom generators for reproducible results
- Cache random numbers when possible

## Integration with Other Modules

### **System Dynamics**
```haskell
import Simulation.Aivika.SystemDynamics

model = do
  -- Use integration functions from SystemDynamics
  x <- integ dx x0
```

### **Results Collection**
```haskell
import Simulation.Aivika.Results

model = do
  -- Collect simulation results
  return $ results [resultSource "x" "position" x]
```

## Best Practices

### **Simulation Design**
1. **Start Simple**: Begin with basic models and add complexity gradually
2. **Validate Assumptions**: Test model assumptions and parameters
3. **Document Parameters**: Clearly document all simulation parameters
4. **Handle Edge Cases**: Consider boundary conditions and error cases
5. **Test Thoroughly**: Test with various parameter combinations

### **Code Organization**
1. **Separate Concerns**: Keep simulation logic separate from setup
2. **Use Type Safety**: Leverage Haskell's type system for safety
3. **Modular Design**: Create reusable simulation components
4. **Clear Naming**: Use descriptive names for variables and functions
5. **Documentation**: Add Haddock comments for all functions

### **Performance Optimization**
1. **Profile First**: Measure performance before optimizing
2. **Choose Appropriate Methods**: Use suitable integration methods
3. **Optimize Events**: Minimize event scheduling overhead
4. **Memory Management**: Be mindful of memory usage patterns
5. **Parallel Processing**: Consider parallel simulation when appropriate

## Example: Complete Simulation

```haskell
import Simulation.Aivika
import Simulation.Aivika.SystemDynamics
import Simulation.Aivika.Results

-- Simulation parameters
specs = Specs 
  { spcStartTime = 0.0
  , spcStopTime = 100.0
  , spcDT = 0.1
  , spcMethod = RungeKutta4
  , spcGeneratorType = SimpleGenerator
  }

-- Simulation model
model :: Simulation Results
model = do
  -- Initialize state variables
  let x0 = 10.0
      v0 = 0.0
  
  -- Define differential equations
  let dx = return v0  -- Position derivative
      dv = return (-9.81)  -- Velocity derivative (gravity)
  
  -- Integrate equations
  x <- integ dx x0
  v <- integ dv v0
  
  -- Schedule periodic output
  scheduleEvent 1.0 $ do
    t <- time
    xVal <- x
    vVal <- v
    putStrLn $ "Time: " ++ show t ++ ", Position: " ++ show xVal ++ ", Velocity: " ++ show vVal
  
  -- Return results
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]

-- Main function
main = do
  putStrLn "Starting simulation..."
  results <- runSimulationWithResults specs model
  putStrLn "Simulation complete."
  return results
```

This example demonstrates a complete simulation with differential equations, event scheduling, and result collection. 