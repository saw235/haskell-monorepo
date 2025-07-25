import {
  Engine,
  Scene,
  FreeCamera,
  HemisphericLight,
  MeshBuilder,
  StandardMaterial,
  DynamicTexture,
  Color3,
  Vector3,
  Mesh,
} from "@babylonjs/core";

interface PixelArtFrame {
  width: number;
  height: number;
  pixels: string[][]; // 2D array of color hex strings
}

interface AnimatedSprite {
  mesh: Mesh;
  material: StandardMaterial;
  texture: DynamicTexture;
  frames: PixelArtFrame[];
  currentFrame: number;
  animationTimer: number;
  animationSpeed: number; // frames per second
  position: Vector3;
  velocity: Vector3;
  type: "character" | "coin" | "enemy";
}

class SpriteAnimationDemo {
  private engine: Engine;
  private scene: Scene;
  private camera: FreeCamera;
  private sprites: AnimatedSprite[] = [];
  private isPlaying: boolean = true;
  private scrollSpeed: number = 1.0;
  private groundLevel: number = -3;
  private backgroundMeshes: Mesh[] = [];

  constructor(canvas: HTMLCanvasElement) {
    this.engine = new Engine(canvas, true);
    this.scene = new Scene(this.engine);
    this.setupScene();
    this.setupControls();
    this.createInitialSprites();
    this.startRenderLoop();
  }

  private setupScene(): void {
    // Setup orthographic camera for true 2D view
    this.camera = new FreeCamera("camera", new Vector3(0, 0, -10), this.scene);
    this.camera.setTarget(Vector3.Zero());

    // Set orthographic projection for 2D
    const canvas = this.engine.getRenderingCanvas()!;
    const aspectRatio = canvas.width / canvas.height;
    const orthoSize = 8;
    this.camera.mode = 1; // Orthographic mode
    this.camera.orthoLeft = -orthoSize * aspectRatio;
    this.camera.orthoRight = orthoSize * aspectRatio;
    this.camera.orthoTop = orthoSize;
    this.camera.orthoBottom = -orthoSize;

    // Simple lighting for 2D
    const light = new HemisphericLight(
      "hemi",
      new Vector3(0, 0, -1),
      this.scene,
    );
    light.intensity = 1.0;

    // Create scrolling background
    this.createBackground();
  }

  private createBackground(): void {
    // Create simple background tiles that scroll
    const tileSize = 2;
    const tilesCount = 20;

    for (let i = 0; i < tilesCount; i++) {
      // Ground tiles
      const groundTile = MeshBuilder.CreatePlane(
        "ground",
        { size: tileSize },
        this.scene,
      );
      groundTile.position = new Vector3(i * tileSize - 10, this.groundLevel, 1);

      const groundMaterial = new StandardMaterial("groundMat", this.scene);
      groundMaterial.diffuseColor = new Color3(0.3, 0.7, 0.3); // Green ground
      groundTile.material = groundMaterial;

      this.backgroundMeshes.push(groundTile);

      // Sky background
      const skyTile = MeshBuilder.CreatePlane(
        "sky",
        { size: tileSize },
        this.scene,
      );
      skyTile.position = new Vector3(i * tileSize - 10, 2, 2);

      const skyMaterial = new StandardMaterial("skyMat", this.scene);
      skyMaterial.diffuseColor = new Color3(0.5, 0.8, 1.0); // Light blue sky
      skyTile.material = skyMaterial;

      this.backgroundMeshes.push(skyTile);
    }
  }

  // Define pixel art frames for different sprite types
  private createCharacterFrames(): PixelArtFrame[] {
    return [
      // Frame 1 - Standing
      {
        width: 8,
        height: 8,
        pixels: [
          [
            "#000000",
            "#FFDBAC",
            "#FFDBAC",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FFDBAC",
            "#FFDBAC",
            "#FFDBAC",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B4513",
            "#8B4513",
            "#8B4513",
            "#8B4513",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FF0000",
            "#FF0000",
            "#FF0000",
            "#FF0000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FF0000",
            "#FFFF00",
            "#FF0000",
            "#FF0000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#0000FF",
            "#0000FF",
            "#0000FF",
            "#0000FF",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B4513",
            "#000000",
            "#8B4513",
            "#000000",
            "#8B4513",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
        ],
      },
      // Frame 2 - Walking
      {
        width: 8,
        height: 8,
        pixels: [
          [
            "#000000",
            "#FFDBAC",
            "#FFDBAC",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FFDBAC",
            "#FFDBAC",
            "#FFDBAC",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B4513",
            "#8B4513",
            "#8B4513",
            "#8B4513",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FF0000",
            "#FF0000",
            "#FF0000",
            "#FF0000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#FF0000",
            "#FFFF00",
            "#FF0000",
            "#FF0000",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#0000FF",
            "#0000FF",
            "#0000FF",
            "#0000FF",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#8B4513",
            "#000000",
            "#8B4513",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B4513",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
        ],
      },
    ];
  }

  private createCoinFrames(): PixelArtFrame[] {
    return [
      // Frame 1 - Full coin
      {
        width: 6,
        height: 6,
        pixels: [
          ["#000000", "#000000", "#FFD700", "#FFD700", "#000000", "#000000"],
          ["#000000", "#FFD700", "#FFFF00", "#FFFF00", "#FFD700", "#000000"],
          ["#FFD700", "#FFFF00", "#FFA500", "#FFA500", "#FFFF00", "#FFD700"],
          ["#FFD700", "#FFFF00", "#FFA500", "#FFA500", "#FFFF00", "#FFD700"],
          ["#000000", "#FFD700", "#FFFF00", "#FFFF00", "#FFD700", "#000000"],
          ["#000000", "#000000", "#FFD700", "#FFD700", "#000000", "#000000"],
        ],
      },
      // Frame 2 - Thin coin (spinning)
      {
        width: 6,
        height: 6,
        pixels: [
          ["#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
          ["#000000", "#000000", "#FFD700", "#FFD700", "#000000", "#000000"],
          ["#000000", "#FFD700", "#FFFF00", "#FFFF00", "#FFD700", "#000000"],
          ["#000000", "#FFD700", "#FFFF00", "#FFFF00", "#FFD700", "#000000"],
          ["#000000", "#000000", "#FFD700", "#FFD700", "#000000", "#000000"],
          ["#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
        ],
      },
    ];
  }

  private createEnemyFrames(): PixelArtFrame[] {
    return [
      // Frame 1 - Enemy normal
      {
        width: 8,
        height: 8,
        pixels: [
          [
            "#000000",
            "#000000",
            "#8B008B",
            "#8B008B",
            "#8B008B",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B008B",
            "#FF1493",
            "#FF1493",
            "#FF1493",
            "#8B008B",
            "#000000",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#FF0000",
            "#000000",
            "#FF0000",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#FF1493",
            "#FF1493",
            "#FF1493",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#000000",
            "#8B008B",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#8B008B",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#8B008B",
            "#8B008B",
            "#8B008B",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
        ],
      },
      // Frame 2 - Enemy angry
      {
        width: 8,
        height: 8,
        pixels: [
          [
            "#000000",
            "#000000",
            "#8B008B",
            "#8B008B",
            "#8B008B",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#8B008B",
            "#FF1493",
            "#FF1493",
            "#FF1493",
            "#8B008B",
            "#000000",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#FF0000",
            "#000000",
            "#FF0000",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#FF1493",
            "#FF0000",
            "#FF1493",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#8B008B",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#8B008B",
            "#000000",
          ],
          [
            "#000000",
            "#8B008B",
            "#FF1493",
            "#000000",
            "#FF1493",
            "#8B008B",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#8B008B",
            "#8B008B",
            "#8B008B",
            "#000000",
            "#000000",
            "#000000",
          ],
          [
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
            "#000000",
          ],
        ],
      },
    ];
  }

  private createPixelTexture(frame: PixelArtFrame): DynamicTexture {
    const textureSize = Math.max(frame.width, frame.height) * 8; // Scale up for better visibility
    const texture = new DynamicTexture(
      "pixelTexture",
      textureSize,
      this.scene,
      false,
    );

    const ctx = texture.getContext();
    const pixelSize = textureSize / Math.max(frame.width, frame.height);

    // Clear the texture
    ctx.fillStyle = "transparent";
    ctx.fillRect(0, 0, textureSize, textureSize);

    // Draw each pixel
    for (let y = 0; y < frame.height; y++) {
      for (let x = 0; x < frame.width; x++) {
        const color = frame.pixels[y][x];
        if (color !== "#000000") {
          // Don't draw black pixels (transparent)
          ctx.fillStyle = color;
          ctx.fillRect(x * pixelSize, y * pixelSize, pixelSize, pixelSize);
        }
      }
    }

    texture.update();
    return texture;
  }

  private createAnimatedSprite(
    type: "character" | "coin" | "enemy",
    position: Vector3,
  ): AnimatedSprite {
    let frames: PixelArtFrame[];
    let animationSpeed: number;
    let velocity: Vector3;

    switch (type) {
      case "character":
        frames = this.createCharacterFrames();
        animationSpeed = 2; // 2 FPS
        velocity = new Vector3(0, 0, 0); // Player doesn't move automatically
        break;
      case "coin":
        frames = this.createCoinFrames();
        animationSpeed = 4; // 4 FPS for spinning effect
        velocity = new Vector3(-this.scrollSpeed, 0, 0); // Moves left with scroll
        break;
      case "enemy":
        frames = this.createEnemyFrames();
        animationSpeed = 3; // 3 FPS
        velocity = new Vector3(-this.scrollSpeed * 0.8, 0, 0); // Moves slightly slower
        break;
    }

    // Create sprite mesh
    const sprite = MeshBuilder.CreatePlane(
      `${type}_sprite`,
      { size: 1 },
      this.scene,
    );
    sprite.position = position.clone();

    // Create material and texture
    const material = new StandardMaterial(`${type}_material`, this.scene);
    const texture = this.createPixelTexture(frames[0]);
    material.diffuseTexture = texture;
    material.hasAlpha = true;
    sprite.material = material;

    return {
      mesh: sprite,
      material,
      texture,
      frames,
      currentFrame: 0,
      animationTimer: 0,
      animationSpeed,
      position: position.clone(),
      velocity,
      type,
    };
  }

  private createInitialSprites(): void {
    // Create a player character
    const character = this.createAnimatedSprite(
      "character",
      new Vector3(-5, this.groundLevel + 1, 0),
    );
    this.sprites.push(character);

    // Create some coins
    for (let i = 0; i < 5; i++) {
      const coin = this.createAnimatedSprite(
        "coin",
        new Vector3(i * 3 + 2, this.groundLevel + 2, 0),
      );
      this.sprites.push(coin);
    }

    // Create some enemies
    for (let i = 0; i < 3; i++) {
      const enemy = this.createAnimatedSprite(
        "enemy",
        new Vector3(i * 4 + 6, this.groundLevel + 1, 0),
      );
      this.sprites.push(enemy);
    }

    this.updateSpriteCount();
  }

  public addSprite(type: "character" | "coin" | "enemy"): void {
    const position = new Vector3(
      8, // Start from right side of screen
      this.groundLevel + (type === "coin" ? 2 : 1),
      0,
    );

    const sprite = this.createAnimatedSprite(type, position);
    this.sprites.push(sprite);
    this.updateSpriteCount();
  }

  public clearAllSprites(): void {
    this.sprites.forEach((sprite) => {
      sprite.texture.dispose();
      sprite.material.dispose();
      sprite.mesh.dispose();
    });
    this.sprites = [];
    this.updateSpriteCount();
  }

  public setScrollSpeed(speed: number): void {
    this.scrollSpeed = speed;
    // Update velocities for moving sprites
    this.sprites.forEach((sprite) => {
      if (sprite.type === "coin") {
        sprite.velocity.x = -this.scrollSpeed;
      } else if (sprite.type === "enemy") {
        sprite.velocity.x = -this.scrollSpeed * 0.8;
      }
    });
  }

  public togglePlayPause(): void {
    this.isPlaying = !this.isPlaying;
  }

  public resetAnimations(): void {
    this.sprites.forEach((sprite) => {
      sprite.currentFrame = 0;
      sprite.animationTimer = 0;
      // Reset texture to first frame
      sprite.texture.dispose();
      sprite.texture = this.createPixelTexture(sprite.frames[0]);
      sprite.material.diffuseTexture = sprite.texture;
    });
  }

  private updateSprites(deltaTime: number): void {
    if (!this.isPlaying) return;

    const deltaSeconds = deltaTime / 1000;

    this.sprites.forEach((sprite, index) => {
      // Update animation
      sprite.animationTimer += deltaSeconds;
      const frameTime = 1 / sprite.animationSpeed;

      if (sprite.animationTimer >= frameTime) {
        sprite.animationTimer = 0;
        sprite.currentFrame = (sprite.currentFrame + 1) % sprite.frames.length;

        // Update texture
        sprite.texture.dispose();
        sprite.texture = this.createPixelTexture(
          sprite.frames[sprite.currentFrame],
        );
        sprite.material.diffuseTexture = sprite.texture;
      }

      // Update position
      sprite.mesh.position.addInPlace(sprite.velocity.scale(deltaSeconds));

      // Remove sprites that have moved off screen
      if (sprite.mesh.position.x < -12) {
        sprite.texture.dispose();
        sprite.material.dispose();
        sprite.mesh.dispose();
        this.sprites.splice(index, 1);
      }
    });

    // Update background scrolling
    this.backgroundMeshes.forEach((mesh) => {
      mesh.position.x -= this.scrollSpeed * deltaSeconds;
      if (mesh.position.x < -12) {
        mesh.position.x += 40; // Reset to right side
      }
    });
  }

  private updateSpriteCount(): void {
    const spriteCountEl = document.getElementById("sprite-count")!;
    spriteCountEl.textContent = `Sprites: ${this.sprites.length}`;
  }

  private setupControls(): void {
    // Play/Pause button
    const playPauseBtn = document.getElementById(
      "play-pause-btn",
    ) as HTMLButtonElement;
    playPauseBtn.onclick = () => {
      this.togglePlayPause();
      playPauseBtn.textContent = this.isPlaying ? "Pause" : "Play";
    };

    // Reset button
    const resetBtn = document.getElementById("reset-btn") as HTMLButtonElement;
    resetBtn.onclick = () => this.resetAnimations();

    // Speed slider (now controls scroll speed)
    const speedSlider = document.getElementById(
      "speed-slider",
    ) as HTMLInputElement;
    const speedValue = document.getElementById("speed-value")!;
    speedSlider.oninput = () => {
      const speed = parseFloat(speedSlider.value);
      this.setScrollSpeed(speed);
      speedValue.textContent = `${speed.toFixed(1)}x`;
    };

    // Add sprite buttons
    document.getElementById("add-square-btn")!.onclick = () =>
      this.addSprite("character");
    document.getElementById("add-circle-btn")!.onclick = () =>
      this.addSprite("coin");
    document.getElementById("add-triangle-btn")!.onclick = () =>
      this.addSprite("enemy");
    document.getElementById("clear-sprites-btn")!.onclick = () =>
      this.clearAllSprites();

    // Keyboard controls for character movement
    window.addEventListener("keydown", (event) => {
      if (!this.isPlaying) return;

      const character = this.sprites.find((s) => s.type === "character");
      if (!character) return;

      switch (event.key) {
        case "ArrowUp":
        case " ":
          // Jump
          character.velocity.y = 3;
          break;
        case "ArrowLeft":
          character.velocity.x = -2;
          break;
        case "ArrowRight":
          character.velocity.x = 2;
          break;
      }
    });

    window.addEventListener("keyup", (event) => {
      const character = this.sprites.find((s) => s.type === "character");
      if (!character) return;

      switch (event.key) {
        case "ArrowLeft":
        case "ArrowRight":
          character.velocity.x = 0;
          break;
      }
    });
  }

  private startRenderLoop(): void {
    let lastTime = performance.now();

    this.engine.runRenderLoop(() => {
      const currentTime = performance.now();
      const deltaTime = currentTime - lastTime;
      lastTime = currentTime;

      // Update sprites and animations
      this.updateSprites(deltaTime);

      // Apply gravity to character
      const character = this.sprites.find((s) => s.type === "character");
      if (character) {
        character.velocity.y -= 9.8 * (deltaTime / 1000); // Gravity
        if (character.mesh.position.y <= this.groundLevel + 1) {
          character.mesh.position.y = this.groundLevel + 1;
          character.velocity.y = 0;
        }
      }

      // Update UI
      const fpsEl = document.getElementById("fps")!;
      fpsEl.textContent = `FPS: ${this.engine.getFps().toFixed(1)}`;

      const cameraInfoEl = document.getElementById("camera-info")!;
      cameraInfoEl.textContent = `Scroll Speed: ${this.scrollSpeed.toFixed(1)}x`;

      this.scene.render();
    });

    // Handle window resize
    window.addEventListener("resize", () => {
      this.engine.resize();
      // Update camera orthographic bounds
      const canvas = this.engine.getRenderingCanvas()!;
      const aspectRatio = canvas.width / canvas.height;
      const orthoSize = 8;
      this.camera.orthoLeft = -orthoSize * aspectRatio;
      this.camera.orthoRight = orthoSize * aspectRatio;
    });
  }
}

// Initialize the demo when DOM is loaded
window.addEventListener("DOMContentLoaded", () => {
  const canvas = document.getElementById("renderCanvas") as HTMLCanvasElement;
  new SpriteAnimationDemo(canvas);
});
