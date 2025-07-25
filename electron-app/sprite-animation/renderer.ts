import {
  Engine,
  Scene,
  ArcRotateCamera,
  HemisphericLight,
  DirectionalLight,
  MeshBuilder,
  StandardMaterial,
  Color3,
  Vector3,
  TransformNode,
  Animation,
  AnimationGroup,
  Mesh,
} from "@babylonjs/core";

interface Sprite {
  mesh: Mesh;
  animationGroup: AnimationGroup;
  type: "square" | "circle" | "triangle";
  basePosition: Vector3;
  animationPhase: number;
}

class SpriteAnimationDemo {
  private engine: Engine;
  private scene: Scene;
  private camera: ArcRotateCamera;
  private sprites: Sprite[] = [];
  private animationSpeed: number = 1.0;
  private isPlaying: boolean = true;
  private rotationEnabled: boolean = true;
  private movementEnabled: boolean = true;
  private scalingEnabled: boolean = true;

  constructor(canvas: HTMLCanvasElement) {
    this.engine = new Engine(canvas, true);
    this.scene = new Scene(this.engine);
    this.setupScene();
    this.setupControls();
    this.createInitialSprites();
    this.startRenderLoop();
  }

  private setupScene(): void {
    // Camera setup for 2D-like view
    this.camera = new ArcRotateCamera(
      "camera",
      0,
      Math.PI / 2,
      15,
      new Vector3(0, 0, 0),
      this.scene,
    );
    this.camera.attachControl(this.engine.getRenderingCanvas()!, true);
    this.camera.wheelDeltaPercentage = 0.01;
    this.camera.setTarget(Vector3.Zero());

    // Lighting
    const hemiLight = new HemisphericLight(
      "hemi",
      new Vector3(0, 1, 0),
      this.scene,
    );
    hemiLight.intensity = 0.8;
    const dirLight = new DirectionalLight(
      "dir",
      new Vector3(0, -1, 0),
      this.scene,
    );
    dirLight.intensity = 0.4;
  }

  private createSpriteMaterial(color: Color3): StandardMaterial {
    const material = new StandardMaterial(
      `spriteMat_${Math.random()}`,
      this.scene,
    );
    material.diffuseColor = color;
    material.specularColor = new Color3(0.2, 0.2, 0.2);
    return material;
  }

  private createSquareSprite(position: Vector3, color: Color3): Sprite {
    const mesh = MeshBuilder.CreateBox("square", { size: 1 }, this.scene);
    mesh.position = position.clone();
    mesh.material = this.createSpriteMaterial(color);

    const animationGroup = new AnimationGroup(
      `squareAnim_${Math.random()}`,
      this.scene,
    );

    // Rotation animation
    const rotationAnim = Animation.CreateAndStartAnimation(
      "squareRotation",
      mesh,
      "rotation.z",
      30,
      120,
      0,
      Math.PI * 2,
      Animation.ANIMATIONLOOPMODE_CYCLE,
    );
    if (rotationAnim) animationGroup.addTargetedAnimation(rotationAnim, mesh);

    return {
      mesh,
      animationGroup,
      type: "square",
      basePosition: position.clone(),
      animationPhase: Math.random() * Math.PI * 2,
    };
  }

  private createCircleSprite(position: Vector3, color: Color3): Sprite {
    const mesh = MeshBuilder.CreateSphere(
      "circle",
      { diameter: 1 },
      this.scene,
    );
    mesh.position = position.clone();
    mesh.material = this.createSpriteMaterial(color);

    const animationGroup = new AnimationGroup(
      `circleAnim_${Math.random()}`,
      this.scene,
    );

    // Scale animation
    const scaleKeys = [
      { frame: 0, value: 1 },
      { frame: 30, value: 1.3 },
      { frame: 60, value: 1 },
    ];
    const scaleAnim = Animation.CreateAndStartAnimation(
      "circleScale",
      mesh,
      "scaling",
      30,
      60,
      new Vector3(1, 1, 1),
      new Vector3(1.3, 1.3, 1.3),
      Animation.ANIMATIONLOOPMODE_CYCLE,
    );
    if (scaleAnim) animationGroup.addTargetedAnimation(scaleAnim, mesh);

    return {
      mesh,
      animationGroup,
      type: "circle",
      basePosition: position.clone(),
      animationPhase: Math.random() * Math.PI * 2,
    };
  }

  private createTriangleSprite(position: Vector3, color: Color3): Sprite {
    const mesh = MeshBuilder.CreateCylinder(
      "triangle",
      {
        height: 0.1,
        diameterTop: 0,
        diameterBottom: 1,
        tessellation: 3,
      },
      this.scene,
    );
    mesh.position = position.clone();
    mesh.rotation.x = Math.PI / 2;
    mesh.material = this.createSpriteMaterial(color);

    const animationGroup = new AnimationGroup(
      `triangleAnim_${Math.random()}`,
      this.scene,
    );

    // Rotation around Y axis
    const rotationAnim = Animation.CreateAndStartAnimation(
      "triangleRotation",
      mesh,
      "rotation.y",
      30,
      180,
      0,
      Math.PI * 2,
      Animation.ANIMATIONLOOPMODE_CYCLE,
    );
    if (rotationAnim) animationGroup.addTargetedAnimation(rotationAnim, mesh);

    return {
      mesh,
      animationGroup,
      type: "triangle",
      basePosition: position.clone(),
      animationPhase: Math.random() * Math.PI * 2,
    };
  }

  private getRandomColor(): Color3 {
    const colors = [
      new Color3(1, 0.2, 0.2), // Red
      new Color3(0.2, 1, 0.2), // Green
      new Color3(0.2, 0.2, 1), // Blue
      new Color3(1, 1, 0.2), // Yellow
      new Color3(1, 0.2, 1), // Magenta
      new Color3(0.2, 1, 1), // Cyan
      new Color3(1, 0.6, 0.2), // Orange
      new Color3(0.6, 0.2, 1), // Purple
    ];
    return colors[Math.floor(Math.random() * colors.length)];
  }

  private getRandomPosition(): Vector3 {
    return new Vector3(
      (Math.random() - 0.5) * 10,
      (Math.random() - 0.5) * 6,
      0,
    );
  }

  private createInitialSprites(): void {
    // Create a few initial sprites
    for (let i = 0; i < 6; i++) {
      const type = ["square", "circle", "triangle"][i % 3] as
        | "square"
        | "circle"
        | "triangle";
      this.addSprite(type);
    }
  }

  public addSprite(type: "square" | "circle" | "triangle"): void {
    const position = this.getRandomPosition();
    const color = this.getRandomColor();

    let sprite: Sprite;
    switch (type) {
      case "square":
        sprite = this.createSquareSprite(position, color);
        break;
      case "circle":
        sprite = this.createCircleSprite(position, color);
        break;
      case "triangle":
        sprite = this.createTriangleSprite(position, color);
        break;
    }

    this.sprites.push(sprite);
    this.updateSpriteCount();
  }

  public clearAllSprites(): void {
    this.sprites.forEach((sprite) => {
      sprite.animationGroup.dispose();
      sprite.mesh.dispose();
    });
    this.sprites = [];
    this.updateSpriteCount();
  }

  public setAnimationSpeed(speed: number): void {
    this.animationSpeed = speed;
    this.sprites.forEach((sprite) => {
      sprite.animationGroup.speedRatio = speed;
    });
  }

  public togglePlayPause(): void {
    this.isPlaying = !this.isPlaying;
    this.sprites.forEach((sprite) => {
      if (this.isPlaying) {
        sprite.animationGroup.play();
      } else {
        sprite.animationGroup.pause();
      }
    });
  }

  public resetAnimations(): void {
    this.sprites.forEach((sprite) => {
      sprite.animationGroup.reset();
      sprite.mesh.position = sprite.basePosition.clone();
      sprite.animationPhase = Math.random() * Math.PI * 2;
    });
  }

  public setRotationEnabled(enabled: boolean): void {
    this.rotationEnabled = enabled;
  }

  public setMovementEnabled(enabled: boolean): void {
    this.movementEnabled = enabled;
  }

  public setScalingEnabled(enabled: boolean): void {
    this.scalingEnabled = enabled;
  }

  private updateSpriteMovement(deltaTime: number): void {
    if (!this.movementEnabled) return;

    this.sprites.forEach((sprite) => {
      sprite.animationPhase += deltaTime * this.animationSpeed * 0.001;

      // Sinusoidal movement pattern
      const offsetX = Math.sin(sprite.animationPhase) * 0.5;
      const offsetY = Math.cos(sprite.animationPhase * 0.7) * 0.3;

      sprite.mesh.position.x = sprite.basePosition.x + offsetX;
      sprite.mesh.position.y = sprite.basePosition.y + offsetY;
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

    // Speed slider
    const speedSlider = document.getElementById(
      "speed-slider",
    ) as HTMLInputElement;
    const speedValue = document.getElementById("speed-value")!;
    speedSlider.oninput = () => {
      const speed = parseFloat(speedSlider.value);
      this.setAnimationSpeed(speed);
      speedValue.textContent = `${speed.toFixed(1)}x`;
    };

    // Add sprite buttons
    document.getElementById("add-square-btn")!.onclick = () =>
      this.addSprite("square");
    document.getElementById("add-circle-btn")!.onclick = () =>
      this.addSprite("circle");
    document.getElementById("add-triangle-btn")!.onclick = () =>
      this.addSprite("triangle");
    document.getElementById("clear-sprites-btn")!.onclick = () =>
      this.clearAllSprites();

    // Animation toggles
    const rotationCheck = document.getElementById(
      "rotation-enabled",
    ) as HTMLInputElement;
    rotationCheck.onchange = () =>
      this.setRotationEnabled(rotationCheck.checked);

    const movementCheck = document.getElementById(
      "movement-enabled",
    ) as HTMLInputElement;
    movementCheck.onchange = () =>
      this.setMovementEnabled(movementCheck.checked);

    const scalingCheck = document.getElementById(
      "scaling-enabled",
    ) as HTMLInputElement;
    scalingCheck.onchange = () => this.setScalingEnabled(scalingCheck.checked);
  }

  private startRenderLoop(): void {
    let lastTime = performance.now();

    this.engine.runRenderLoop(() => {
      const currentTime = performance.now();
      const deltaTime = currentTime - lastTime;
      lastTime = currentTime;

      // Update custom movement animations
      this.updateSpriteMovement(deltaTime);

      // Update UI
      const fpsEl = document.getElementById("fps")!;
      fpsEl.textContent = `FPS: ${this.engine.getFps().toFixed(1)}`;

      const cameraInfoEl = document.getElementById("camera-info")!;
      cameraInfoEl.textContent = `Camera: α=${this.camera.alpha.toFixed(2)}, β=${this.camera.beta.toFixed(2)}`;

      this.scene.render();
    });

    // Handle window resize
    window.addEventListener("resize", () => this.engine.resize());
  }
}

// Initialize the demo when DOM is loaded
window.addEventListener("DOMContentLoaded", () => {
  const canvas = document.getElementById("renderCanvas") as HTMLCanvasElement;
  new SpriteAnimationDemo(canvas);
});
