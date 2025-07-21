# Aivika Bouncing Ball Simulation Example

This directory contains a working example of a bouncing ball simulation using the [Aivika](https://hackage.haskell.org/package/aivika) Haskell simulation library.

## Overview

The simulation models a ball bouncing under gravity with realistic physics, including:
- Gravitational acceleration (g = 9.81 m/s²)
- Coefficient of restitution (k = 0.8) for energy loss on bounces
- Initial conditions: 10m height, 15 m/s upward velocity
- Step-by-step output showing position and velocity over time

## Files

- `Main.hs` - The main simulation code
- `BUILD.bazel` - Bazel build configuration
- `README.md` - This documentation file

## Building and Running

### Prerequisites

- Bazel build system
- Haskell toolchain (managed by Bazel)

### Build

```bash
bazel build //haskell/app/aivika-example:aivika-example
```

### Run

```bash
bazel run //haskell/app/aivika-example:aivika-example
```

## Code Explanation

### Key Components

1. **Simulation Specs**
   ```haskell
   specs = Specs { spcStartTime = 0.0,
                   spcStopTime = 5.0,
                   spcDT = 0.1,
                   spcMethod = RungeKutta4,
                   spcGeneratorType = SimpleGenerator }
   ```

2. **Physics Parameters**
   ```haskell
   g = 9.81    -- gravitational acceleration
   v0 = 15     -- initial velocity (upward)
   x0 = 10     -- initial position (height)
   k = 0.8     -- coefficient of restitution
   ```

3. **Differential Equations**
   - `dv`: Velocity change (acceleration due to gravity or bounce)
   - `dx`: Position change (velocity)

4. **Bounce Logic**
   ```haskell
   if x' < 0
     then return $ Left (- k * v')  -- bounce with energy loss
     else return $ Right (- g)      -- normal gravity
   ```

### Output Functions

The simulation uses `printSimulationResultsInTimes` from the `Simulation.Aivika.Results.IO` module to output results at specific time points:

```haskell
let timePoints = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]
printSimulationResultsInTimes timePoints printResultSourceInEnglish model specs
```

## Sample Output

```
Bouncing Ball Simulation - Step by Step Output
==============================================
Printing results every 0.5 seconds:

----------
-- simulation time
t = 0.0
-- time
t = 0.0
-- position
x = 10.0
-- velocity
v = 15.0

----------
-- simulation time
t = 0.5
-- time
t = 0.5
-- position
x = 16.519000000000002
-- velocity
v = 10.095
...
```

## Physics Analysis

### Ball Trajectory

1. **Rising Phase (0.0-1.5s)**: Ball rises to maximum height
2. **Falling Phase (2.0-3.5s)**: Ball falls under gravity
3. **First Bounce (4.0s)**: Ball hits ground, bounces with 80% energy retention
4. **Second Bounce (4.5-5.0s)**: Ball continues bouncing

### Energy Conservation

- Initial kinetic energy: ~112.5 J (½ × mass × 15²)
- After first bounce: ~90 J (80% retention)
- Demonstrates realistic energy loss in bouncing

## Troubleshooting History

### Original Issues

The initial implementation faced several type mismatch errors:

1. **Resource Creation Issues**
   - `newResource` function signature problems
   - Queue strategy type annotations needed
   - Parameter type mismatches

2. **Function Signature Problems**
   - `traceEvent` usage errors
   - `runSimulation` parameter order issues

3. **Output Function Confusion**
   - `printSimulationResultsInStopTime` vs other variants
   - Step-by-step output not working initially

### Solutions Applied

1. **Used Official Example**: Replaced custom code with verified bouncing ball example from [Aivika repository](https://gitflic.ru/project/dsorokin/aivika/blob/raw?file=examples%2FBouncingBall.hs&commit=340421faefb6d66e32f267487d86a520400b8844)

2. **Correct Output Function**: Used `printSimulationResultsInTimes` from [Simulation.Aivika.Results.IO](https://hackage.haskell.org/package/aivika-6.1.1/docs/Simulation-Aivika-Results-IO.html) for step-by-step output

3. **Proper Type Annotations**: Used `RecursiveDo` extension and correct type signatures

## Aivika Library Information

### Key Modules Used

- `Simulation.Aivika` - Core simulation functionality
- `Simulation.Aivika.SystemDynamics` - Differential equation solvers
- `Simulation.Aivika.Results.IO` - Output and printing functions

### Important Functions

- `integEither` - Integrate differential equations with discrete events
- `printSimulationResultsInTimes` - Print results at specific time points
- `printResultSourceInEnglish` - English language output formatter

### Function Origins

The `printSimulationResultsInStopTime` function comes from the `Simulation.Aivika.Results.IO` module, as documented in the [Aivika Hackage package](https://hackage.haskell.org/package/aivika-6.1.1/docs/Simulation-Aivika-Results-IO.html).

## Extending the Example

### Adding More Physics

- Air resistance
- Variable coefficient of restitution
- Multiple balls
- Collisions between balls

### Different Output Formats

- CSV export
- Real-time plotting
- Statistical analysis
- Animation generation

### Simulation Parameters

- Different initial conditions
- Various gravity values
- Multiple bounce surfaces
- Time-varying parameters

## References

- [Aivika Hackage Package](https://hackage.haskell.org/package/aivika)
- [Aivika Results.IO Documentation](https://hackage.haskell.org/package/aivika-6.1.1/docs/Simulation-Aivika-Results-IO.html)
- [Original Bouncing Ball Example](https://gitflic.ru/project/dsorokin/aivika/blob/raw?file=examples%2FBouncingBall.hs&commit=340421faefb6d66e32f267487d86a520400b8844)
- [MATLAB Simulink Reference](http://www.mathworks.com/help/simulink/examples/simulation-of-a-bouncing-ball.html)

## License

This example is based on the Aivika library which is licensed under BSD3. 