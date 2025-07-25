# 2D Sprite Animation Demo

A demonstration of 2D sprite animation using Babylon.js and Electron, featuring programmatically generated sprites and interactive animation controls.

## Features

- **Programmatic Sprite Generation**: Creates sprites using basic geometric shapes (squares, circles, triangles)
- **Multiple Animation Types**: Rotation, movement, and scaling animations
- **Interactive Controls**: Play/pause, speed adjustment, and sprite management
- **Real-time Feedback**: FPS counter and sprite count display
- **Cross-platform**: Runs on Windows, macOS, and Linux

## Building and Running

### Prerequisites
- Bazel 7.4.0+
- PNPM (for dependency management)

### Build Commands

```bash
# Build the application
bazel build //electron-app/sprite-animation:sprite-animation

# Run in normal mode
bazel run //electron-app/sprite-animation:sprite-animation

# Run in development mode (with DevTools)
bazel run //electron-app/sprite-animation:sprite-animation-dev
```

### Install Dependencies
```bash
bazel run -- @pnpm//:pnpm --dir $PWD/electron-app/sprite-animation/ install
```

## Usage

### Controls

- **Play/Pause**: Toggle animation playback
- **Reset**: Reset all sprites to their initial positions
- **Speed Slider**: Adjust animation speed from 0.1x to 3.0x
- **Add Sprites**: Create new squares, circles, or triangles
- **Clear All**: Remove all sprites from the scene
- **Animation Toggles**: Enable/disable rotation, movement, and scaling

### Sprite Types

1. **Squares**: Rotate around their Z-axis with solid colors
2. **Circles**: Scale up and down with smooth transitions  
3. **Triangles**: Rotate around their Y-axis as 3-sided cylinders

### Animation System

- **Built-in Animations**: Each sprite type has its own animation pattern using Babylon.js AnimationGroups
- **Movement System**: Sinusoidal movement patterns applied to all sprites independently
- **Speed Control**: Global speed multiplier affects all animations simultaneously
- **Individual Phases**: Each sprite starts with a random phase to create variety

## Technical Details

### Architecture

- **Frontend**: Electron with TypeScript and Babylon.js
- **Build System**: Bazel with aspect_rules_js for modern JavaScript tooling
- **Bundling**: ESBuild for fast TypeScript compilation
- **Styling**: CSS Grid and Flexbox for responsive layout

### Key Components

- `SpriteAnimationDemo`: Main application class managing the 3D scene
- `Sprite Interface`: Type definition for sprite objects with mesh and animation data
- `Animation System`: Babylon.js AnimationGroups for frame-based animations
- `Control System`: DOM event handlers for interactive controls

### Performance

- 60 FPS target with real-time FPS monitoring
- Efficient sprite management with proper disposal
- Babylon.js rendering optimizations for 2D-style scenes

## Development

### Adding New Sprite Types

1. Create a new method in `SpriteAnimationDemo` (e.g., `createStarSprite`)
2. Add the sprite type to the `Sprite` interface
3. Update the `addSprite` method with the new case
4. Add a button in `index.html` and wire it up in `setupControls`

### Modifying Animations

- Edit the `Animation.CreateAndStartAnimation` calls in sprite creation methods
- Adjust the `updateSpriteMovement` method for custom movement patterns
- Modify animation speeds by changing the frame counts and key values

## Dependencies

- **@babylonjs/core**: 3D graphics engine
- **@babylonjs/gui**: UI components (future use)
- **@babylonjs/materials**: Advanced materials (future use)
- **electron**: Desktop application framework
- **typescript**: Type safety and modern JavaScript features
- **esbuild**: Fast JavaScript bundler

## License

MIT License - see the project root for details.