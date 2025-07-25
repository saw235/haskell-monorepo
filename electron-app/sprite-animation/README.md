# 2D Pixel Art Side Scroller

A demonstration of 2D pixel art animation and side-scrolling using Babylon.js and Electron, featuring hand-crafted pixel art sprites with frame-based animation.

## Features

- **Pixel Art Animation**: Hand-crafted 8x8 and 6x6 pixel art sprites with multiple animation frames
- **Side-Scrolling Gameplay**: True 2D orthographic view with scrolling background
- **Interactive Character**: Keyboard controls for jumping and movement with gravity physics
- **Multiple Sprite Types**: Animated character, spinning coins, and enemy sprites
- **Dynamic Spawning**: Add new sprites during runtime
- **Real-time Controls**: Play/pause, scroll speed adjustment, and sprite management
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

#### UI Controls
- **Play/Pause**: Toggle animation playback and movement
- **Reset**: Reset all sprite animations to first frame
- **Scroll Speed Slider**: Adjust background and sprite scroll speed from 0.1x to 3.0x
- **Add Character**: Spawn a new player character
- **Add Coin**: Spawn a spinning collectible coin
- **Add Enemy**: Spawn an animated enemy sprite  
- **Clear All**: Remove all sprites from the scene

#### Keyboard Controls
- **Arrow Keys**: Move the character left and right
- **Space Bar / Up Arrow**: Make the character jump
- **Left/Right Arrows**: Walk left and right

### Sprite Types

1. **Character**: 8x8 pixel art character with standing and walking animation frames
   - Skin tone head, brown hair, red shirt with yellow belt, blue pants, brown shoes
   - 2-frame walking animation at 2 FPS
   - Responds to keyboard input and gravity

2. **Coin**: 6x6 pixel art spinning coin with gold/yellow colors
   - 2-frame spinning animation at 4 FPS  
   - Scrolls from right to left automatically
   - Simulates collectible game items

3. **Enemy**: 8x8 pixel art enemy with purple/magenta colors
   - 2-frame animation showing normal and angry states at 3 FPS
   - Scrolls from right to left at 80% of scroll speed
   - Red eyes and intimidating appearance

### Animation System

- **Frame-Based Animation**: Each sprite type has hand-crafted pixel art frames
- **Dynamic Texture Generation**: Pixel data is converted to Babylon.js DynamicTextures in real-time
- **Individual Timing**: Each sprite animates at its own frame rate (2-4 FPS for pixel art feel)
- **Automatic Cleanup**: Sprites that scroll off-screen are automatically disposed
- **Physics Integration**: Character has gravity, jumping, and ground collision

## Technical Details

### Architecture

- **Frontend**: Electron with TypeScript and Babylon.js
- **Build System**: Bazel with aspect_rules_js for modern JavaScript tooling
- **Bundling**: ESBuild for fast TypeScript compilation
- **Styling**: CSS Grid and Flexbox for responsive layout

### Key Components

- `SpriteAnimationDemo`: Main application class managing the 2D orthographic scene
- `PixelArtFrame`: Interface defining pixel art frame data as 2D color arrays
- `AnimatedSprite`: Interface for sprites with frame-based animation and physics
- `Dynamic Texture System`: Real-time conversion of pixel data to Babylon.js textures
- `Physics System`: Gravity, jumping, and collision detection for character
- `Keyboard Controls`: Event handlers for character movement and jumping

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
