# Aivika Pendulum Visualization

An interactive Electron application for visualizing pendulum simulations from the Aivika Haskell backend. Features real-time pendulum animation, multiple chart views, and interactive parameter controls.

## Features

### Real-time Pendulum Animation

- **SVG-based visualization** with smooth pendulum motion
- **Interactive time control** with play/pause and scrubbing
- **Motion trail** showing pendulum path history
- **Accurate physics rendering** matching backend calculations

### Multiple Chart Views

1. **Angle vs Time**: Classical pendulum oscillation plot
2. **Angular Velocity**: Rate of change visualization
3. **Phase Portrait**: State space diagram (angle vs velocity)
4. **Energy Analysis**: Kinetic, potential, and total energy tracking

### Interactive Controls

- **Pendulum Parameters**:
  - Length (0.1-5.0 m)
  - Damping coefficient (0.0-2.0)
  - Initial angle (-90° to 90°)
  - Initial angular velocity (-10 to 10 rad/s)
  - Simulation time (1-60 seconds)

### Data Management

- **Real-time data display** in tabular format
- **Export capabilities**: CSV and JSON formats
- **Connection status** monitoring for Haskell backend
- **Fallback simulation** when backend unavailable

## Architecture

### Frontend Stack

- **Electron**: Cross-platform desktop application framework
- **Chart.js**: Interactive charting library
- **WebSocket Client**: Real-time communication with Haskell backend
- **SVG Animation**: Smooth pendulum motion rendering

### Backend Integration

- **WebSocket Protocol**: JSON message exchange on port 9162
- **Automatic Reconnection**: Handles backend server restarts
- **Fallback Mode**: JavaScript implementation when Haskell unavailable
- **Parameter Validation**: Client-side input checking

## Usage

### Standalone Mode

Run the Electron app alone (uses JavaScript fallback):

```bash
bazel run //electron-app/aivika-pendulum-viz:aivika-pendulum-viz
```

### Development Mode

Run with developer tools enabled:

```bash
bazel run //electron-app/aivika-pendulum-viz:aivika-pendulum-viz-dev
```

### Complete Demo

Run with Haskell backend automatically started:

```bash
bazel run //electron-app/aivika-pendulum-viz:aivika-pendulum-demo
```

### Manual Setup

1. Start Haskell backend:

   ```bash
   bazel run //haskell/app/aivika-pendulum-sim:aivika-pendulum-sim -- --server --port 9162
   ```

2. Start Electron app:
   ```bash
   bazel run //electron-app/aivika-pendulum-viz:aivika-pendulum-viz
   ```

## User Interface

### Parameter Controls

- **Intuitive sliders and inputs** for all pendulum parameters
- **Real-time validation** with visual feedback
- **Reset functionality** to restore default values
- **Parameter persistence** during session

### Animation Panel

- **Centered pendulum visualization** with proper scaling
- **Time display** showing current simulation time
- **Playback controls**: play, pause, and timeline scrubbing
- **Visual reference grid** for position awareness

### Chart Tabs

- **Responsive layouts** adapting to window size
- **Interactive charts** with zoom and pan capabilities
- **Legend and axis labeling** for clear data interpretation
- **Smooth transitions** between different views

### Data Export

- **Sampling options** to manage large datasets
- **Standard formats** (CSV, JSON) for external analysis
- **Filename conventions** with timestamps
- **Progress indicators** for export operations

## Technical Implementation

### WebSocket Communication

```javascript
// Connection to Haskell backend
const ws = new WebSocket("ws://127.0.0.1:9162");

// Simulation request format
const request = {
  msgType: "simulate",
  content: JSON.stringify({
    pendulumLength: 1.0,
    dampingCoeff: 0.1,
    initialAngle: 0.785, // radians
    initialVelocity: 0.0,
    timeEnd: 10.0,
  }),
};
```

### Animation Engine

```javascript
// SVG pendulum rendering
function updatePendulumPosition(timeIndex) {
  const data = simulationData[timeIndex];
  const x = centerX + data.xPosition * scale;
  const y = centerY - data.yPosition * scale;

  pendulumRod.setAttribute("x2", x);
  pendulumRod.setAttribute("y2", y);
  pendulumBob.setAttribute("cx", x);
  pendulumBob.setAttribute("cy", y);
}
```

### Fallback Simulation

When the Haskell backend is unavailable, the app uses a JavaScript implementation:

```javascript
// Simplified pendulum physics
function generateFallbackSimulation(params) {
  const {
    pendulumLength,
    dampingCoeff,
    initialAngle,
    initialVelocity,
    timeEnd,
  } = params;

  for (let t = 0; t <= timeEnd; t += dt) {
    const alpha =
      -(g / pendulumLength) * Math.sin(theta) - dampingCoeff * omega;
    omega += alpha * dt;
    theta += omega * dt;

    // Store results...
  }
}
```

## Chart Configurations

### Angle vs Time Chart

- **X-axis**: Time (seconds)
- **Y-axis**: Angle (degrees)
- **Style**: Smooth line with fill area
- **Color**: Blue gradient (#667eea)

### Phase Portrait Chart

- **X-axis**: Angle (degrees)
- **Y-axis**: Angular velocity (rad/s)
- **Style**: Scatter plot with connected points
- **Color**: Pink (#ed64a6)
- **Shows**: System trajectory in state space

### Energy Chart

- **Multiple datasets**: Kinetic, potential, total energy
- **Physics validation**: Energy conservation visualization
- **Color coding**: Red (kinetic), Blue (potential), Teal (total)

## File Structure

```
electron-app/aivika-pendulum-viz/
├── package.json          # Node.js dependencies
├── main.js              # Electron main process
├── preload.js           # Security context bridge
├── index.html           # Application UI structure
├── styles.css           # UI styling and responsive design
├── renderer.js          # Frontend logic and animation
├── BUILD.bazel          # Build configuration with demo targets
└── README.md            # This documentation
```

## Dependencies

### Production Dependencies

- **chart.js**: ^4.4.0 - Interactive charting
- **ws**: ^8.14.0 - WebSocket client implementation

### Development Dependencies

- **electron**: ^28.0.0 - Desktop application framework
- **electron-builder**: ^24.6.4 - Application packaging

## Building and Packaging

### Development Build

```bash
cd electron-app/aivika-pendulum-viz
pnpm install
pnpm run dev
```

### Production Package

```bash
pnpm run build
```

### Cross-platform Targets

- **macOS**: .dmg installer
- **Windows**: NSIS installer
- **Linux**: AppImage

## Performance Considerations

### Animation Optimization

- **RequestAnimationFrame**: Smooth 60fps animation
- **Data sampling**: Efficient memory usage for large datasets
- **SVG optimization**: Minimal DOM manipulations

### Chart Rendering

- **Canvas-based charts**: Hardware-accelerated rendering
- **Responsive updates**: Efficient data binding
- **Memory management**: Cleanup on data reset

### WebSocket Efficiency

- **Connection pooling**: Single persistent connection
- **Message batching**: Reduced network overhead
- **Automatic reconnection**: Resilient connectivity

## Customization

### Styling

Modify `styles.css` for visual customization:

- Color schemes and themes
- Layout and spacing adjustments
- Responsive breakpoints

### Chart Options

Extend chart configurations in `renderer.js`:

- Additional chart types
- Custom axis formatting
- Animation parameters

### Physics Parameters

Adjust simulation bounds and defaults:

- Parameter ranges and validation
- Default initial conditions
- Simulation time limits

## Integration with Backend

The visualization seamlessly integrates with the Haskell simulation:

- **Protocol compatibility**: JSON message format
- **Data synchronization**: Real-time parameter updates
- **Error handling**: Graceful fallback mechanisms
- **Performance matching**: Coordinated timing and precision

For backend details, see `haskell/app/aivika-pendulum-sim/README.md`.
