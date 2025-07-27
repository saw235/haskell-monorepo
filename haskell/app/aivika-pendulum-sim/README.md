# Aivika Simple Pendulum Simulation

A sophisticated pendulum simulation built with Haskell using the Aivika simulation framework. This application demonstrates the mathematical modeling of pendulum motion with damping, supporting both command-line interface and WebSocket server modes for real-time visualization.

## Mathematical Model

The simulation implements the differential equation for a damped pendulum:

```
d²θ/dt² = -(g/L)sin(θ) - c*(dθ/dt)
```

Where:
- `θ` = angle from vertical (radians)
- `L` = pendulum length (meters)
- `g` = gravitational acceleration (9.81 m/s²)
- `c` = damping coefficient
- `t` = time (seconds)

The system is solved using coupled first-order differential equations:
- `dθ/dt = ω` (angular velocity)
- `dω/dt = -(g/L)sin(θ) - c*ω` (angular acceleration)

## Features

### Physics Simulation
- **Realistic pendulum motion** with gravitational and damping forces
- **Configurable parameters**: length, damping coefficient, initial conditions
- **High-precision integration** using Runge-Kutta 4th order method
- **Small angle and large angle dynamics** (non-linear behavior)

### Dual Operation Modes
1. **CLI Mode**: Command-line execution with tabular output
2. **Server Mode**: WebSocket server for real-time visualization

### Real-time Data Output
- Time series data with position and velocity
- Cartesian coordinates for visualization
- JSON API for integration with frontend applications

## Usage

### Command Line Mode

Run the simulation directly with default parameters:

```bash
bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
```

### Customize parameters via environment variables:

```bash
PENDULUM_LENGTH=2.0 DAMPING_COEFF=0.05 INITIAL_ANGLE=1.0 bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
```

### WebSocket Server Mode

Start the WebSocket server:

```bash
bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim -- --server --port 9162
```

### API Usage

Send simulation requests via WebSocket:

```json
{
  "msgType": "simulate",
  "content": "{\"pendulumLength\":1.0,\"dampingCoeff\":0.1,\"initialAngle\":0.785,\"initialVelocity\":0.0,\"timeEnd\":10}"
}
```

Response format:
```json
{
  "type": "simulation_result",
  "results": [
    {
      "time": 0.0,
      "angle": 0.785,
      "angularVelocity": 0.0,
      "xPosition": 0.707,
      "yPosition": -0.707
    }
  ]
}
```

## Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| `pendulumLength` | 1.0 m | 0.1-5.0 | Length of pendulum rod |
| `dampingCoeff` | 0.1 | 0.0-2.0 | Damping coefficient (0 = no damping) |
| `initialAngle` | 0.785 rad (45°) | -π/2 to π/2 | Starting angle from vertical |
| `initialVelocity` | 0.0 rad/s | -10 to 10 | Initial angular velocity |
| `timeEnd` | 10.0 s | 1-60 | Simulation duration |

## Environment Variables

- `PENDULUM_LENGTH`: Override default pendulum length
- `DAMPING_COEFF`: Override default damping coefficient  
- `INITIAL_ANGLE`: Override default initial angle (radians)
- `INITIAL_VELOCITY`: Override default initial angular velocity
- `TIME_END`: Override default simulation time

## Output Format

### CLI Mode Output
```
Time (s) | Angle (rad) | Angle (°) | Angular Velocity (rad/s) | X (m) | Y (m)
---------|-------------|----------|--------------------------|-------|-------
0.0      | 0.785       | 44.98    | 0.0                      | 0.707 | -0.707
1.0      | -0.747      | -42.77   | -0.023                   | -0.679| -0.734
```

### WebSocket API Commands

- **ping**: Test server connectivity
- **simulate**: Run pendulum simulation with parameters
- **help**: Display available commands

## Implementation Details

### Numerical Integration
- Uses Aivika's `integ` function with Runge-Kutta 4th order method
- Time step: 0.01 seconds for accurate integration
- Output sampling: 0.05 seconds for smooth visualization

### Coordinate System
- Origin at pendulum pivot point
- Positive X: rightward displacement
- Positive Y: upward displacement (negative values below pivot)
- Angle θ: measured from vertical, positive clockwise

### WebSocket Protocol
- **Port**: 9162 (default)
- **Protocol**: JSON message exchange
- **Connection**: Persistent connection with ping/pong heartbeat

## Dependencies

- `aivika`: Simulation framework for differential equations
- `websockets`: WebSocket server implementation
- `aeson`: JSON parsing and encoding
- `optparse-applicative`: Command-line argument parsing
- `mtl`: Monad transformer library

## Building

The application builds with Bazel using rules_haskell:

```bash
bazel build //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
```

## Testing

### Unit Testing
Run CLI mode with various parameters to verify physics:

```bash
# Test different damping levels
DAMPING_COEFF=0.0 bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
DAMPING_COEFF=0.5 bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim

# Test different initial angles
INITIAL_ANGLE=0.1 bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
INITIAL_ANGLE=1.5 bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim
```

### Integration Testing
Test WebSocket server with simple client:

```bash
# Terminal 1: Start server
bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim -- --server

# Terminal 2: Test with websocket client or browser
wscat -c ws://localhost:9162
```

## Physical Validation

The simulation accurately models:
- **Energy conservation** (in undamped case)
- **Damping effects** reducing amplitude over time
- **Period dependence** on amplitude for large angles
- **Small angle approximation** (θ ≈ sin(θ) for small θ)

## Integration with Visualization

This backend pairs with the Electron visualization app:
- Real-time pendulum animation
- Interactive parameter controls  
- Multiple chart views (angle vs time, phase portrait, energy)
- Data export capabilities

See `electron-app/aivika-pendulum-viz/` for the complete visualization interface.