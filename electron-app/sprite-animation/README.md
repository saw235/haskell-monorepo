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

## How Sprites Are Generated

This demo uses a programmatic approach to create pixel art sprites entirely in code, without requiring external image files.

### Pixel Art Definition

Each sprite is defined as a 2D array of hex color strings representing individual pixels:

```typescript
interface PixelArtFrame {
  width: number; // Width in pixels (e.g., 8)
  height: number; // Height in pixels (e.g., 8)
  pixels: string[][]; // 2D array of hex colors like "#FF0000"
}
```

### Example: Character Sprite

The character sprite is defined as an 8x8 pixel grid:

```typescript
{
  width: 8,
  height: 8,
  pixels: [
    ['#000000', '#FFDBAC', '#FFDBAC', '#000000', '#000000', '#000000', '#000000', '#000000'], // Head
    ['#000000', '#FFDBAC', '#FFDBAC', '#FFDBAC', '#000000', '#000000', '#000000', '#000000'], // Head
    ['#000000', '#8B4513', '#8B4513', '#8B4513', '#8B4513', '#000000', '#000000', '#000000'], // Hair
    ['#000000', '#FF0000', '#FF0000', '#FF0000', '#FF0000', '#000000', '#000000', '#000000'], // Shirt
    ['#000000', '#FF0000', '#FFFF00', '#FF0000', '#FF0000', '#000000', '#000000', '#000000'], // Belt
    ['#000000', '#0000FF', '#0000FF', '#0000FF', '#0000FF', '#000000', '#000000', '#000000'], // Pants
    ['#000000', '#8B4513', '#000000', '#8B4513', '#000000', '#8B4513', '#000000', '#000000'], // Feet
    ['#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000', '#000000']  // Ground
  ]
}
```

### Color Palette

The demo uses a carefully chosen retro pixel art color palette:

- **`#FFDBAC`** - Skin tone (peach)
- **`#8B4513`** - Brown (hair, shoes)
- **`#FF0000`** - Red (shirt)
- **`#FFFF00`** - Yellow (belt)
- **`#0000FF`** - Blue (pants)
- **`#FFD700`** - Gold (coins)
- **`#8B008B`** - Dark magenta (enemy outline)
- **`#FF1493`** - Hot pink (enemy body)
- **`#000000`** - Black (transparent pixels)

### Dynamic Texture Creation Process

1. **Canvas Creation**: A `DynamicTexture` canvas is created at 8x scale for better visibility
2. **Pixel Rendering**: Each pixel in the array is drawn as a square on the canvas
3. **Transparency**: Black pixels (`#000000`) are treated as transparent
4. **Texture Update**: The canvas is converted to a Babylon.js texture
5. **Material Assignment**: The texture is applied to a plane mesh with alpha support

### Code Example

```typescript
private createPixelTexture(frame: PixelArtFrame): DynamicTexture {
  const textureSize = Math.max(frame.width, frame.height) * 8; // 8x scale
  const texture = new DynamicTexture("pixelTexture", textureSize, this.scene, false);

  const ctx = texture.getContext();
  const pixelSize = textureSize / Math.max(frame.width, frame.height);

  // Draw each pixel as a colored square
  for (let y = 0; y < frame.height; y++) {
    for (let x = 0; x < frame.width; x++) {
      const color = frame.pixels[y][x];
      if (color !== '#000000') { // Skip transparent pixels
        ctx.fillStyle = color;
        ctx.fillRect(x * pixelSize, y * pixelSize, pixelSize, pixelSize);
      }
    }
  }

  texture.update();
  return texture;
}
```

### Benefits of This Approach

- **No External Dependencies**: All sprites are defined in code
- **Easy Customization**: Colors and patterns can be modified programmatically
- **Version Control Friendly**: Pixel data is stored as readable text
- **Real-time Generation**: Textures are created dynamically at runtime
- **Memory Efficient**: Only active sprites consume texture memory
- **Scalable**: Pixel size can be adjusted for different screen resolutions

### Creating New Sprites

To add new sprites, simply define new `PixelArtFrame` arrays and add them to the sprite creation methods. Each sprite can have multiple frames for animation, and the system will automatically cycle through them at the specified frame rate.

### Code Locations and Implementation

The sprite generation system is implemented across several key methods in `renderer.ts`:

#### 1. Sprite Frame Definitions (`renderer.ts:104-503`)

Each sprite type has its own method that returns an array of `PixelArtFrame` objects:

```typescript
// Character sprite frames (lines 104-293)
private createCharacterFrames(): PixelArtFrame[] {
  return [
    // Frame 1 - Standing pose
    { width: 8, height: 8, pixels: [...] },
    // Frame 2 - Walking pose
    { width: 8, height: 8, pixels: [...] }
  ];
}

// Coin sprite frames (lines 295-324)
private createCoinFrames(): PixelArtFrame[] {
  return [
    // Frame 1 - Full coin
    { width: 6, height: 6, pixels: [...] },
    // Frame 2 - Thin coin (spinning effect)
    { width: 6, height: 6, pixels: [...] }
  ];
}

// Enemy sprite frames (lines 326-503)
private createEnemyFrames(): PixelArtFrame[] {
  return [
    // Frame 1 - Normal state
    { width: 8, height: 8, pixels: [...] },
    // Frame 2 - Angry state
    { width: 8, height: 8, pixels: [...] }
  ];
}
```

#### 2. Dynamic Texture Creation (`renderer.ts:505-535`)

The `createPixelTexture` method converts pixel arrays into Babylon.js textures:

```typescript
private createPixelTexture(frame: PixelArtFrame): DynamicTexture {
  const textureSize = Math.max(frame.width, frame.height) * 8; // 8x upscale
  const texture = new DynamicTexture("pixelTexture", textureSize, this.scene, false);

  const ctx = texture.getContext();
  const pixelSize = textureSize / Math.max(frame.width, frame.height);

  // Clear canvas with transparent background
  ctx.fillStyle = "transparent";
  ctx.fillRect(0, 0, textureSize, textureSize);

  // Render each pixel as a colored rectangle
  for (let y = 0; y < frame.height; y++) {
    for (let x = 0; x < frame.width; x++) {
      const color = frame.pixels[y][x];
      if (color !== "#000000") { // Skip black (transparent) pixels
        ctx.fillStyle = color;
        ctx.fillRect(x * pixelSize, y * pixelSize, pixelSize, pixelSize);
      }
    }
  }

  texture.update();
  return texture;
}
```

#### 3. Sprite Creation (`renderer.ts:537-590`)

The `createAnimatedSprite` method combines frames, textures, and meshes:

```typescript
private createAnimatedSprite(
  type: "character" | "coin" | "enemy",
  position: Vector3
): AnimatedSprite {
  let frames: PixelArtFrame[];
  let animationSpeed: number;
  let velocity: Vector3;

  // Select appropriate frames and properties based on sprite type
  switch (type) {
    case "character":
      frames = this.createCharacterFrames();
      animationSpeed = 2; // 2 FPS for walking animation
      velocity = new Vector3(0, 0, 0); // Player controlled
      break;
    case "coin":
      frames = this.createCoinFrames();
      animationSpeed = 4; // 4 FPS for spinning effect
      velocity = new Vector3(-this.scrollSpeed, 0, 0); // Scrolls left
      break;
    case "enemy":
      frames = this.createEnemyFrames();
      animationSpeed = 3; // 3 FPS for mood changes
      velocity = new Vector3(-this.scrollSpeed * 0.8, 0, 0); // Slower scroll
      break;
  }

  // Create 3D plane mesh for the sprite
  const sprite = MeshBuilder.CreatePlane(`${type}_sprite`, { size: 1 }, this.scene);
  sprite.position = position.clone();

  // Create material with the first frame texture
  const material = new StandardMaterial(`${type}_material`, this.scene);
  const texture = this.createPixelTexture(frames[0]);
  material.diffuseTexture = texture;
  material.hasAlpha = true; // Enable transparency
  sprite.material = material;

  return { mesh: sprite, material, texture, frames, currentFrame: 0, /* ... */ };
}
```

#### 4. Frame Animation (`renderer.ts:670-689`)

The animation system updates textures in real-time:

```typescript
private updateSprites(deltaTime: number): void {
  // ... other update logic ...

  this.sprites.forEach((sprite, index) => {
    // Update animation timer
    sprite.animationTimer += deltaSeconds;
    const frameTime = 1 / sprite.animationSpeed;

    // Check if it's time for next frame
    if (sprite.animationTimer >= frameTime) {
      sprite.animationTimer = 0;
      sprite.currentFrame = (sprite.currentFrame + 1) % sprite.frames.length;

      // Dispose old texture and create new one
      sprite.texture.dispose();
      sprite.texture = this.createPixelTexture(sprite.frames[sprite.currentFrame]);
      sprite.material.diffuseTexture = sprite.texture;
    }

    // ... position updates and cleanup ...
  });
}
```

#### 5. Initial Sprite Setup (`renderer.ts:592-619`)

Sprites are initially created and positioned:

```typescript
private createInitialSprites(): void {
  // Create player character at left side
  const character = this.createAnimatedSprite(
    "character",
    new Vector3(-5, this.groundLevel + 1, 0)
  );
  this.sprites.push(character);

  // Create coins at regular intervals
  for (let i = 0; i < 5; i++) {
    const coin = this.createAnimatedSprite(
      "coin",
      new Vector3(i * 3 + 2, this.groundLevel + 2, 0)
    );
    this.sprites.push(coin);
  }

  // Create enemies spread across the level
  for (let i = 0; i < 3; i++) {
    const enemy = this.createAnimatedSprite(
      "enemy",
      new Vector3(i * 4 + 6, this.groundLevel + 1, 0)
    );
    this.sprites.push(enemy);
  }
}
```

### Adding Your Own Sprites

To create new sprite types:

1. **Define frames** - Add a new method like `createMyNewSpriteFrames()`
2. **Add to switch statement** - Include your sprite type in `createAnimatedSprite()`
3. **Update UI** - Add buttons and controls in `index.html` and `setupControls()`
4. **Set properties** - Define animation speed, velocity, and positioning rules

The system automatically handles texture creation, animation cycling, and cleanup for any sprite type you define!

## Using External Sprite Images

While this demo uses programmatic pixel art, you can easily modify it to support external sprite images (PNG, JPG, etc.). Here are the approaches:

### Option 1: Modify `createPixelTexture` for External Images

Replace or extend the `createPixelTexture` method to load external images:

```typescript
// Add to imports
import { Texture } from "@babylonjs/core";

// New method for external sprites
private createExternalTexture(imageUrl: string): Promise<Texture> {
  return new Promise((resolve, reject) => {
    const texture = new Texture(imageUrl, this.scene, false, false, undefined,
      () => resolve(texture),  // onLoad
      () => reject(new Error(`Failed to load ${imageUrl}`)) // onError
    );
    texture.hasAlpha = true; // Enable transparency
    texture.wrapU = Texture.CLAMP_ADDRESSMODE;
    texture.wrapV = Texture.CLAMP_ADDRESSMODE;
  });
}

// Modified sprite creation for external images
private async createExternalSprite(
  imageUrls: string[], // Array of frame images
  type: string,
  position: Vector3
): Promise<AnimatedSprite> {
  const textures: Texture[] = [];

  // Load all frame textures
  for (const url of imageUrls) {
    const texture = await this.createExternalTexture(url);
    textures.push(texture);
  }

  // Create sprite mesh
  const sprite = MeshBuilder.CreatePlane(`${type}_sprite`, { size: 1 }, this.scene);
  sprite.position = position.clone();

  // Create material with first frame
  const material = new StandardMaterial(`${type}_material`, this.scene);
  material.diffuseTexture = textures[0];
  material.hasAlpha = true;
  sprite.material = material;

  return {
    mesh: sprite,
    material,
    textures, // Store all textures instead of generating them
    currentFrame: 0,
    animationTimer: 0,
    animationSpeed: 4,
    position: position.clone(),
    velocity: new Vector3(-this.scrollSpeed, 0, 0),
    type
  };
}
```

### Option 2: Sprite Sheet Support

For sprite sheets (multiple frames in one image), add sprite sheet functionality:

```typescript
interface SpriteSheetFrame {
  x: number;      // X position in sprite sheet
  y: number;      // Y position in sprite sheet
  width: number;  // Frame width
  height: number; // Frame height
}

interface SpriteSheetData {
  imageUrl: string;
  frameWidth: number;
  frameHeight: number;
  frames: SpriteSheetFrame[];
}

private createSpriteSheetTexture(
  spriteSheetData: SpriteSheetData,
  frameIndex: number
): Promise<Texture> {
  return new Promise((resolve, reject) => {
    const baseTexture = new Texture(spriteSheetData.imageUrl, this.scene, false, false);

    baseTexture.onLoadObservable.add(() => {
      const frame = spriteSheetData.frames[frameIndex];

      // Create dynamic texture for the specific frame
      const frameTexture = new DynamicTexture(
        `frame_${frameIndex}`,
        { width: frame.width, height: frame.height },
        this.scene
      );

      const ctx = frameTexture.getContext();
      const img = baseTexture.getInternalTexture()?.getWebGLTexture();

      // Draw the specific frame from the sprite sheet
      ctx.drawImage(
        img as any,
        frame.x, frame.y, frame.width, frame.height,  // Source
        0, 0, frame.width, frame.height               // Destination
      );

      frameTexture.update();
      resolve(frameTexture);
    });
  });
}
```

### Option 3: Asset Management Setup

For a production setup, organize your external sprites:

```
electron-app/sprite-animation/
├── assets/
│   ├── sprites/
│   │   ├── character/
│   │   │   ├── walk_01.png
│   │   │   ├── walk_02.png
│   │   │   └── idle.png
│   │   ├── enemies/
│   │   │   ├── goblin_01.png
│   │   │   └── goblin_02.png
│   │   └── items/
│   │       ├── coin_spin_01.png
│   │       └── coin_spin_02.png
│   └── spritesheets/
│       ├── character_sheet.png
│       └── character_data.json
├── renderer.ts
└── index.html
```

### Option 4: Hybrid Approach

Combine both approaches in your sprite creation:

```typescript
// Updated AnimatedSprite interface
interface AnimatedSprite {
  mesh: Mesh;
  material: StandardMaterial;
  textures?: Texture[];        // For external images
  frames?: PixelArtFrame[];    // For programmatic sprites
  currentFrame: number;
  animationTimer: number;
  animationSpeed: number;
  position: Vector3;
  velocity: Vector3;
  type: string;
  isExternal: boolean;         // Flag to determine rendering method
}

// Updated animation method
private updateSprites(deltaTime: number): void {
  this.sprites.forEach((sprite, index) => {
    if (sprite.animationTimer >= frameTime) {
      sprite.animationTimer = 0;
      sprite.currentFrame = (sprite.currentFrame + 1) %
        (sprite.isExternal ? sprite.textures!.length : sprite.frames!.length);

      if (sprite.isExternal) {
        // Use pre-loaded texture
        sprite.material.diffuseTexture = sprite.textures![sprite.currentFrame];
      } else {
        // Generate texture from pixel data
        sprite.texture?.dispose();
        sprite.texture = this.createPixelTexture(sprite.frames![sprite.currentFrame]);
        sprite.material.diffuseTexture = sprite.texture;
      }
    }
  });
}
```

### Performance Considerations

- **Pre-load textures**: Load all external images at startup to avoid runtime delays
- **Texture atlasing**: Combine multiple sprites into texture atlases for better performance
- **Image optimization**: Use compressed formats (WebP) and appropriate sizes
- **Memory management**: Dispose of unused textures properly

### Example Usage

```typescript
// Add external sprite to your game
async addExternalSprite() {
  const dragonFrames = [
    '/assets/sprites/dragon/fly_01.png',
    '/assets/sprites/dragon/fly_02.png',
    '/assets/sprites/dragon/fly_03.png'
  ];

  const dragon = await this.createExternalSprite(
    dragonFrames,
    'dragon',
    new Vector3(8, this.groundLevel + 3, 0)
  );

  this.sprites.push(dragon);
}
```

This approach gives you the flexibility to use both programmatic pixel art (for simple sprites) and external artwork (for complex sprites) in the same application!

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
