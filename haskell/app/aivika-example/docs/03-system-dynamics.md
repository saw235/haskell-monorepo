# System Dynamics Module: Simulation.Aivika.SystemDynamics

## Overview

The `Simulation.Aivika.SystemDynamics` module provides tools for solving differential equations and modeling continuous dynamic systems. It's essential for simulations involving physical systems, chemical reactions, population dynamics, and other continuous processes.

## Key Concepts

### **Differential Equations**
Aivika can solve systems of ordinary differential equations (ODEs) of the form:
```
dx/dt = f(t, x, y, z, ...)
dy/dt = g(t, x, y, z, ...)
dz/dt = h(t, x, y, z, ...)
```

### **Integration Methods**
The module supports several numerical integration methods:
- **Euler**: Simple but less accurate
- **Runge-Kutta 2**: Second-order accuracy
- **Runge-Kutta 4**: Fourth-order accuracy (recommended)
- **Runge-Kutta-Fehlberg 45**: Adaptive step size

## Core Functions

### **Basic Integration**

#### `integ`
```haskell
integ :: Simulation Double -> Double -> Simulation (Simulation Double)
```
Integrates a differential equation with a given initial value.

**Parameters:**
- `Simulation Double`: The derivative function
- `Double`: Initial value

**Returns:** A simulation that produces the integrated value

**Usage:**
```haskell
model = do
  -- Define derivative: dx/dt = v
  let dx = return 5.0  -- Constant velocity
  
  -- Integrate with initial value x0 = 10.0
  x <- integ dx 10.0
  
  -- Use the integrated value
  xVal <- x
  putStrLn $ "Position: " ++ show xVal
```

#### `integEither`
```haskell
integEither :: Simulation (Either Double Double) -> Double -> Simulation (Simulation Double)
```
Integrates differential equations with discrete events.

**Parameters:**
- `Simulation (Either Double Double)`: Derivative function that can return discrete events
- `Double`: Initial value

**Returns:** A simulation that produces the integrated value

**Usage:**
```haskell
model = do
  -- Define derivative with discrete events
  let dx = do
        x' <- x
        if x' < 0
          then return $ Left 0  -- Discrete event: stop at boundary
          else return $ Right v -- Continuous: normal integration
  
  -- Integrate with initial value
  x <- integEither dx 10.0
```

### **Advanced Integration**

#### `integWithMethod`
```haskell
integWithMethod :: Method -> Simulation Double -> Double -> Simulation (Simulation Double)
```
Integrates with a specific integration method.

**Usage:**
```haskell
model = do
  x <- integWithMethod RungeKutta4 dx x0
```

#### `integWithTolerance`
```haskell
integWithTolerance :: Double -> Simulation Double -> Double -> Simulation (Simulation Double)
```
Integrates with adaptive step size based on tolerance.

**Usage:**
```haskell
model = do
  x <- integWithTolerance 1e-6 dx x0  -- Tolerance of 1e-6
```

## Common Patterns

### **Coupled Differential Equations**

For systems with multiple coupled equations:

```haskell
model = mdo
  -- Define coupled equations
  let dx = do
        y' <- y
        return y'  -- dx/dt = y
  
  let dy = do
        x' <- x
        return (-x')  -- dy/dt = -x (simple harmonic oscillator)
  
  -- Integrate both equations
  x <- integ dx x0
  y <- integ dy y0
  
  return $ results [resultSource "x" "position" x, resultSource "y" "velocity" y]
```

### **Time-Dependent Derivatives**

```haskell
model = do
  let dx = do
        t <- time
        return $ sin t  -- dx/dt = sin(t)
  
  x <- integ dx 0.0
  return $ results [resultSource "x" "position" x]
```

### **State-Dependent Derivatives**

```haskell
model = do
  let dx = do
        x' <- x
        return $ -0.1 * x'  -- dx/dt = -0.1*x (exponential decay)
  
  x <- integ dx 100.0
  return $ results [resultSource "x" "quantity" x]
```

## Physical System Examples

### **Simple Harmonic Oscillator**

```haskell
harmonicOscillator :: Simulation Results
harmonicOscillator = mdo
  let k = 1.0  -- Spring constant
      m = 1.0  -- Mass
      omega = sqrt (k / m)  -- Natural frequency
  
  let dx = do
        v' <- v
        return v'  -- dx/dt = v
  
  let dv = do
        x' <- x
        return $ -omega^2 * x'  -- dv/dt = -ω²x
  
  x <- integ dx 1.0   -- Initial position
  v <- integ dv 0.0   -- Initial velocity
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]
```

### **Damped Oscillator**

```haskell
dampedOscillator :: Simulation Results
dampedOscillator = mdo
  let k = 1.0    -- Spring constant
      m = 1.0    -- Mass
      c = 0.1    -- Damping coefficient
      omega = sqrt (k / m)
      zeta = c / (2 * sqrt (k * m))  -- Damping ratio
  
  let dx = do
        v' <- v
        return v'  -- dx/dt = v
  
  let dv = do
        x' <- x
        v' <- v
        return $ -omega^2 * x' - 2 * zeta * omega * v'  -- dv/dt = -ω²x - 2ζωv
  
  x <- integ dx 1.0
  v <- integ dv 0.0
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]
```

### **Predator-Prey System (Lotka-Volterra)**

```haskell
predatorPrey :: Simulation Results
predatorPrey = mdo
  let alpha = 1.0  -- Prey growth rate
      beta = 0.1   -- Predation rate
      gamma = 0.1  -- Predator death rate
      delta = 0.02 -- Predator growth rate
  
  let dx = do
        x' <- x
        y' <- y
        return $ alpha * x' - beta * x' * y'  -- dx/dt = αx - βxy
  
  let dy = do
        x' <- x
        y' <- y
        return $ delta * x' * y' - gamma * y'  -- dy/dt = δxy - γy
  
  x <- integ dx 100.0  -- Initial prey population
  y <- integ dy 10.0   -- Initial predator population
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "prey" "prey_population" x
    , resultSource "predator" "predator_population" y
    ]
```

## Chemical Reaction Examples

### **First-Order Reaction**

```haskell
firstOrderReaction :: Simulation Results
firstOrderReaction = do
  let k = 0.1  -- Rate constant
  
  let dA = do
        A' <- A
        return $ -k * A'  -- dA/dt = -kA
  
  A <- integ dA 1.0  -- Initial concentration
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "A" "concentration" A
    ]
```

### **Second-Order Reaction**

```haskell
secondOrderReaction :: Simulation Results
secondOrderReaction = mdo
  let k = 0.01  -- Rate constant
  
  let dA = do
        A' <- A
        B' <- B
        return $ -k * A' * B'  -- dA/dt = -kAB
  
  let dB = do
        A' <- A
        B' <- B
        return $ -k * A' * B'  -- dB/dt = -kAB
  
  A <- integ dA 1.0  -- Initial concentration of A
  B <- integ dB 1.0  -- Initial concentration of B
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "A" "concentration_A" A
    , resultSource "B" "concentration_B" B
    ]
```

## Boundary Conditions and Constraints

### **Bouncing Ball with Constraints**

```haskell
bouncingBall :: Simulation Results
bouncingBall = mdo
  let g = 9.81    -- Gravitational acceleration
      k = 0.8     -- Coefficient of restitution
  
  let dv = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left (-k * v')  -- Bounce: reverse velocity with energy loss
          else return $ Right (-g)      -- Free fall: constant acceleration
  
  let dx = do
        x' <- x
        v' <- v
        if x' < 0
          then return $ Left 0          -- Stop at ground
          else return $ Right v'        -- Normal motion
  
  v <- integEither dv 15.0  -- Initial upward velocity
  x <- integEither dx 10.0  -- Initial height
  
  return $ results 
    [ resultSource "t" "time" time
    , resultSource "x" "position" x
    , resultSource "v" "velocity" v
    ]
```

## Error Handling and Stability

### **Stiff Systems**

For stiff differential equations, use appropriate integration methods:

```haskell
stiffSystem :: Simulation Results
stiffSystem = do
  let dx = do
        x' <- x
        return $ -1000 * x'  -- Very fast dynamics
  
  x <- integWithMethod RungeKutta4 dx 1.0
  return $ results [resultSource "x" "value" x]
```

### **Adaptive Step Size**

For systems with varying time scales:

```haskell
adaptiveSystem :: Simulation Results
adaptiveSystem = do
  let dx = do
        t <- time
        x' <- x
        return $ if t < 5.0 
                 then -0.1 * x'  -- Slow dynamics
                 else -10.0 * x' -- Fast dynamics
  
  x <- integWithTolerance 1e-6 dx 1.0
  return $ results [resultSource "x" "value" x]
```

## Performance Considerations

### **Integration Method Selection**

- **Euler**: Fast but less accurate, good for simple systems
- **Runge-Kutta 2**: Better accuracy, moderate speed
- **Runge-Kutta 4**: High accuracy, recommended for most applications
- **Runge-Kutta-Fehlberg 45**: Adaptive, best for varying dynamics

### **Step Size Optimization**

- Smaller step sizes increase accuracy but reduce performance
- Larger step sizes improve performance but may cause instability
- Use adaptive methods for systems with varying time scales

### **Memory Usage**

- Integration methods store intermediate values
- Long simulations may accumulate significant memory usage
- Consider using lazy evaluation for result processing

## Best Practices

### **Equation Formulation**
1. **Use Physical Units**: Keep units consistent throughout
2. **Normalize Variables**: Scale variables to avoid numerical issues
3. **Check Dimensionality**: Ensure equations are dimensionally correct
4. **Validate Initial Conditions**: Ensure initial values are reasonable

### **Numerical Stability**
1. **Choose Appropriate Methods**: Use RK4 for most applications
2. **Monitor Step Size**: Watch for very small or large step sizes
3. **Check Conservation Laws**: Verify energy/mass conservation
4. **Test Parameter Sensitivity**: Vary parameters to check stability

### **Code Organization**
1. **Separate Physics**: Keep physical equations separate from simulation logic
2. **Use Meaningful Names**: Name variables and parameters clearly
3. **Document Assumptions**: Comment on physical assumptions
4. **Test Components**: Test individual equations before coupling

## Integration with Other Modules

### **Results Collection**
```haskell
import Simulation.Aivika.Results

model = do
  x <- integ dx x0
  return $ results [resultSource "x" "position" x]
```

### **Event Scheduling**
```haskell
model = do
  x <- integ dx x0
  
  -- Schedule events based on integrated values
  scheduleEvent 1.0 $ do
    xVal <- x
    putStrLn $ "Position at t=1: " ++ show xVal
```

### **Random Processes**
```haskell
model = do
  let dx = do
        noise <- randomNormal 0.0 0.1
        return $ -0.1 * x + noise  -- Stochastic differential equation
  
  x <- integ dx x0
```

This module provides the foundation for modeling continuous dynamic systems in Aivika, enabling complex simulations of physical, chemical, and biological processes. 