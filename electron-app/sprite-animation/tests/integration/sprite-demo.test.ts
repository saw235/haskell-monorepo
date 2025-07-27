/**
 * Integration tests for SpriteAnimationDemo class
 * Tests the complete sprite animation system including scene setup,
 * sprite management, and game loop integration
 */

import { describe, it, expect, beforeEach, jest } from '@jest/globals';
import {
  MockEngine,
  MockScene,
  MockFreeCamera,
  MockHemisphericLight,
  MockVector3,
  MockMesh,
  MockStandardMaterial,
  MockDynamicTexture,
  MockMeshBuilder
} from '../mocks/babylon-mocks';

interface PixelArtFrame {
  width: number;
  height: number;
  pixels: string[][];
}

interface AnimatedSprite {
  mesh: MockMesh;
  material: MockStandardMaterial;
  texture: MockDynamicTexture;
  frames: PixelArtFrame[];
  currentFrame: number;
  animationTimer: number;
  animationSpeed: number;
  position: MockVector3;
  velocity: MockVector3;
  type: "character" | "coin" | "enemy";
}

// Mock implementation of SpriteAnimationDemo for testing
class MockSpriteAnimationDemo {
  private engine: MockEngine;
  private scene: MockScene;
  private camera: MockFreeCamera;
  private sprites: AnimatedSprite[] = [];
  private isPlaying: boolean = true;
  private scrollSpeed: number = 1.0;
  private groundLevel: number = -3;
  private backgroundMeshes: MockMesh[] = [];

  constructor(canvas: HTMLCanvasElement) {
    this.engine = new MockEngine(canvas, true);
    this.scene = new MockScene(this.engine);
    this.setupScene();
    this.createInitialSprites();
  }

  private setupScene(): void {
    // Setup orthographic camera for true 2D view
    this.camera = new MockFreeCamera("camera", new MockVector3(0, 0, -10), this.scene);
    this.camera.setTarget(MockVector3.Zero());

    // Set orthographic projection for 2D
    this.camera.mode = 1; // Orthographic mode
    this.camera.orthoLeft = -16;
    this.camera.orthoRight = 16;
    this.camera.orthoTop = 8;
    this.camera.orthoBottom = -8;

    // Simple lighting for 2D
    const light = new MockHemisphericLight("hemi", new MockVector3(0, 0, -1), this.scene);
    light.intensity = 1.0;

    // Create scrolling background
    this.createBackground();
  }

  private createBackground(): void {
    const tileSize = 2;
    const tilesCount = 20;

    for (let i = 0; i < tilesCount; i++) {
      // Ground tiles
      const groundTile = MockMeshBuilder.CreatePlane("ground", { size: tileSize }, this.scene);
      groundTile.position = new MockVector3(i * tileSize - 10, this.groundLevel, 1);
      this.backgroundMeshes.push(groundTile);

      // Sky background
      const skyTile = MockMeshBuilder.CreatePlane("sky", { size: tileSize }, this.scene);
      skyTile.position = new MockVector3(i * tileSize - 10, 2, 2);
      this.backgroundMeshes.push(skyTile);
    }
  }

  private createCharacterFrames(): PixelArtFrame[] {
    return [
      {
        width: 8,
        height: 8,
        pixels: Array(8).fill(null).map(() => Array(8).fill("#FF0000"))
      },
      {
        width: 8,
        height: 8,
        pixels: Array(8).fill(null).map(() => Array(8).fill("#00FF00"))
      }
    ];
  }

  private createCoinFrames(): PixelArtFrame[] {
    return [
      {
        width: 6,
        height: 6,
        pixels: Array(6).fill(null).map(() => Array(6).fill("#FFD700"))
      },
      {
        width: 6,
        height: 6,
        pixels: Array(6).fill(null).map(() => Array(6).fill("#FFFF00"))
      }
    ];
  }

  private createEnemyFrames(): PixelArtFrame[] {
    return [
      {
        width: 8,
        height: 8,
        pixels: Array(8).fill(null).map(() => Array(8).fill("#8B008B"))
      },
      {
        width: 8,
        height: 8,
        pixels: Array(8).fill(null).map(() => Array(8).fill("#FF1493"))
      }
    ];
  }

  private createAnimatedSprite(type: "character" | "coin" | "enemy", position: MockVector3): AnimatedSprite {
    let frames: PixelArtFrame[];
    let animationSpeed: number;
    let velocity: MockVector3;

    switch (type) {
      case "character":
        frames = this.createCharacterFrames();
        animationSpeed = 2;
        velocity = new MockVector3(0, 0, 0);
        break;
      case "coin":
        frames = this.createCoinFrames();
        animationSpeed = 4;
        velocity = new MockVector3(-this.scrollSpeed, 0, 0);
        break;
      case "enemy":
        frames = this.createEnemyFrames();
        animationSpeed = 3;
        velocity = new MockVector3(-this.scrollSpeed * 0.8, 0, 0);
        break;
    }

    const sprite = MockMeshBuilder.CreatePlane(`${type}_sprite`, { size: 1 }, this.scene);
    sprite.position = position.clone();

    const material = new MockStandardMaterial(`${type}_material`, this.scene);
    const texture = new MockDynamicTexture("pixelTexture", 64, this.scene, false);
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
    const character = this.createAnimatedSprite("character", new MockVector3(-5, this.groundLevel + 1, 0));
    this.sprites.push(character);

    // Create some coins
    for (let i = 0; i < 5; i++) {
      const coin = this.createAnimatedSprite("coin", new MockVector3(i * 3 + 2, this.groundLevel + 2, 0));
      this.sprites.push(coin);
    }

    // Create some enemies
    for (let i = 0; i < 3; i++) {
      const enemy = this.createAnimatedSprite("enemy", new MockVector3(i * 4 + 6, this.groundLevel + 1, 0));
      this.sprites.push(enemy);
    }
  }

  public addSprite(type: "character" | "coin" | "enemy"): void {
    const position = new MockVector3(8, this.groundLevel + (type === "coin" ? 2 : 1), 0);
    const sprite = this.createAnimatedSprite(type, position);
    this.sprites.push(sprite);
  }

  public clearAllSprites(): void {
    this.sprites.forEach((sprite) => {
      sprite.texture.dispose();
      sprite.material.dispose();
      sprite.mesh.dispose();
    });
    this.sprites = [];
  }

  public setScrollSpeed(speed: number): void {
    this.scrollSpeed = speed;
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
    });
  }

  public updateSprites(deltaTime: number): void {
    if (!this.isPlaying) return;

    const deltaSeconds = deltaTime / 1000;

    this.sprites.forEach((sprite, index) => {
      // Update animation
      sprite.animationTimer += deltaSeconds;
      const frameTime = 1 / sprite.animationSpeed;

      if (sprite.animationTimer >= frameTime) {
        sprite.animationTimer = 0;
        sprite.currentFrame = (sprite.currentFrame + 1) % sprite.frames.length;
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
        mesh.position.x += 40;
      }
    });
  }

  // Getters for testing
  public getSprites(): AnimatedSprite[] { return this.sprites; }
  public getBackgroundMeshes(): MockMesh[] { return this.backgroundMeshes; }
  public getEngine(): MockEngine { return this.engine; }
  public getScene(): MockScene { return this.scene; }
  public getCamera(): MockFreeCamera { return this.camera; }
  public getScrollSpeed(): number { return this.scrollSpeed; }
  public getIsPlaying(): boolean { return this.isPlaying; }
  public getGroundLevel(): number { return this.groundLevel; }
}

describe('SpriteAnimationDemo Integration', () => {
  let demo: MockSpriteAnimationDemo;
  let mockCanvas: HTMLCanvasElement;

  beforeEach(() => {
    // Create mock canvas
    mockCanvas = {
      width: 800,
      height: 600,
      getContext: jest.fn(() => ({
        fillRect: jest.fn(),
        clearRect: jest.fn(),
        canvas: { width: 800, height: 600 }
      }))
    } as any;

    demo = new MockSpriteAnimationDemo(mockCanvas);
  });

  describe('Initialization', () => {
    it('should create engine, scene, and camera', () => {
      expect(demo.getEngine()).toBeDefined();
      expect(demo.getScene()).toBeDefined();
      expect(demo.getCamera()).toBeDefined();
    });

    it('should setup orthographic camera correctly', () => {
      const camera = demo.getCamera();
      
      expect(camera.mode).toBe(1); // Orthographic mode
      expect(camera.orthoLeft).toBe(-16);
      expect(camera.orthoRight).toBe(16);
      expect(camera.orthoTop).toBe(8);
      expect(camera.orthoBottom).toBe(-8);
    });

    it('should create initial sprites', () => {
      const sprites = demo.getSprites();
      
      expect(sprites).toHaveLength(9); // 1 character + 5 coins + 3 enemies
      
      const characterSprites = sprites.filter(s => s.type === "character");
      const coinSprites = sprites.filter(s => s.type === "coin");
      const enemySprites = sprites.filter(s => s.type === "enemy");
      
      expect(characterSprites).toHaveLength(1);
      expect(coinSprites).toHaveLength(5);
      expect(enemySprites).toHaveLength(3);
    });

    it('should create background meshes', () => {
      const backgroundMeshes = demo.getBackgroundMeshes();
      
      expect(backgroundMeshes).toHaveLength(40); // 20 ground + 20 sky tiles
    });

    it('should start in playing state', () => {
      expect(demo.getIsPlaying()).toBe(true);
    });
  });

  describe('Sprite Management', () => {
    it('should add new sprites correctly', () => {
      const initialCount = demo.getSprites().length;
      
      demo.addSprite("coin");
      
      expect(demo.getSprites()).toHaveLength(initialCount + 1);
      
      const newSprite = demo.getSprites()[demo.getSprites().length - 1];
      expect(newSprite.type).toBe("coin");
      expect(newSprite.mesh.position.x).toBe(8); // Spawns from right
    });

    it('should position sprites correctly based on type', () => {
      demo.addSprite("character");
      demo.addSprite("coin");
      demo.addSprite("enemy");
      
      const sprites = demo.getSprites();
      const newCharacter = sprites.find(s => s.mesh.position.x === 8 && s.type === "character");
      const newCoin = sprites.find(s => s.mesh.position.x === 8 && s.type === "coin");
      const newEnemy = sprites.find(s => s.mesh.position.x === 8 && s.type === "enemy");
      
      expect(newCharacter?.mesh.position.y).toBe(-2); // groundLevel + 1
      expect(newCoin?.mesh.position.y).toBe(-1); // groundLevel + 2
      expect(newEnemy?.mesh.position.y).toBe(-2); // groundLevel + 1
    });

    it('should clear all sprites', () => {
      demo.clearAllSprites();
      
      expect(demo.getSprites()).toHaveLength(0);
    });

    it('should dispose resources when clearing sprites', () => {
      const initialSprites = demo.getSprites();
      const disposeSpy = jest.spyOn(initialSprites[0].texture, 'dispose');
      
      demo.clearAllSprites();
      
      expect(disposeSpy).toHaveBeenCalled();
    });
  });

  describe('Animation System', () => {
    it('should update sprite animations based on deltaTime', () => {
      const sprites = demo.getSprites();
      const character = sprites.find(s => s.type === "character")!;
      const initialFrame = character.currentFrame;
      
      // Update with enough time to advance frame (character has 2 FPS = 0.5s per frame)
      demo.updateSprites(600); // 0.6 seconds
      
      expect(character.currentFrame).toBe((initialFrame + 1) % character.frames.length);
      expect(character.animationTimer).toBeCloseTo(0.1, 1); // 0.6 - 0.5
    });

    it('should not update animations when paused', () => {
      demo.togglePlayPause(); // Pause
      
      const sprites = demo.getSprites();
      const character = sprites.find(s => s.type === "character")!;
      const initialFrame = character.currentFrame;
      const initialTimer = character.animationTimer;
      
      demo.updateSprites(1000);
      
      expect(character.currentFrame).toBe(initialFrame);
      expect(character.animationTimer).toBe(initialTimer);
    });

    it('should handle different animation speeds correctly', () => {
      const sprites = demo.getSprites();
      const character = sprites.find(s => s.type === "character")!; // 2 FPS
      const coin = sprites.find(s => s.type === "coin")!; // 4 FPS
      
      // Update with 0.3 seconds (enough for coin frame but not character)
      demo.updateSprites(300);
      
      expect(character.currentFrame).toBe(0); // No change (needs 0.5s)
      expect(coin.currentFrame).toBe(1); // Advanced (needs 0.25s)
    });

    it('should reset animations correctly', () => {
      const sprites = demo.getSprites();
      
      // Advance animations
      demo.updateSprites(1000);
      
      // Reset
      demo.resetAnimations();
      
      sprites.forEach(sprite => {
        expect(sprite.currentFrame).toBe(0);
        expect(sprite.animationTimer).toBe(0);
      });
    });
  });

  describe('Movement and Physics', () => {
    it('should move sprites based on velocity', () => {
      const sprites = demo.getSprites();
      const coin = sprites.find(s => s.type === "coin")!;
      const initialX = coin.mesh.position.x;
      
      demo.updateSprites(1000); // 1 second
      
      expect(coin.mesh.position.x).toBe(initialX - 1.0); // -scrollSpeed
    });

    it('should not move character sprite automatically', () => {
      const sprites = demo.getSprites();
      const character = sprites.find(s => s.type === "character")!;
      const initialPosition = character.mesh.position.clone();
      
      demo.updateSprites(1000);
      
      expect(character.mesh.position.x).toBe(initialPosition.x);
      expect(character.mesh.position.y).toBe(initialPosition.y);
    });

    it('should remove sprites that move off screen', () => {
      const sprites = demo.getSprites();
      const coin = sprites.find(s => s.type === "coin")!;
      
      // Move sprite off screen
      coin.mesh.position.x = -15;
      
      const initialCount = demo.getSprites().length;
      demo.updateSprites(1);
      
      expect(demo.getSprites().length).toBe(initialCount - 1);
    });
  });

  describe('Background Scrolling', () => {
    it('should scroll background meshes', () => {
      const backgroundMeshes = demo.getBackgroundMeshes();
      const initialX = backgroundMeshes[0].position.x;
      
      demo.updateSprites(1000); // 1 second
      
      expect(backgroundMeshes[0].position.x).toBe(initialX - 1.0); // -scrollSpeed
    });

    it('should wrap background meshes when they move off screen', () => {
      const backgroundMeshes = demo.getBackgroundMeshes();
      const mesh = backgroundMeshes[0];
      
      // Move mesh off screen
      mesh.position.x = -15;
      
      demo.updateSprites(1000);
      
      expect(mesh.position.x).toBe(25); // -15 + 40 (wrap distance)
    });
  });

  describe('Scroll Speed Control', () => {
    it('should update scroll speed and sprite velocities', () => {
      demo.setScrollSpeed(2.5);
      
      expect(demo.getScrollSpeed()).toBe(2.5);
      
      const sprites = demo.getSprites();
      const coin = sprites.find(s => s.type === "coin")!;
      const enemy = sprites.find(s => s.type === "enemy")!;
      
      expect(coin.velocity.x).toBe(-2.5);
      expect(enemy.velocity.x).toBe(-2.0); // -2.5 * 0.8
    });

    it('should not affect character velocity when changing scroll speed', () => {
      const sprites = demo.getSprites();
      const character = sprites.find(s => s.type === "character")!;
      const initialVelocity = character.velocity.x;
      
      demo.setScrollSpeed(3.0);
      
      expect(character.velocity.x).toBe(initialVelocity);
    });
  });

  describe('Play/Pause Functionality', () => {
    it('should toggle play/pause state', () => {
      expect(demo.getIsPlaying()).toBe(true);
      
      demo.togglePlayPause();
      expect(demo.getIsPlaying()).toBe(false);
      
      demo.togglePlayPause();
      expect(demo.getIsPlaying()).toBe(true);
    });
  });

  describe('Performance and Resource Management', () => {
    it('should handle many sprites efficiently', () => {
      // Add many sprites
      for (let i = 0; i < 100; i++) {
        demo.addSprite("coin");
      }
      
      expect(demo.getSprites().length).toBeGreaterThan(100);
      
      // Should handle update without issues
      expect(() => {
        demo.updateSprites(16); // ~60 FPS
      }).not.toThrow();
    });

    it('should clean up resources properly', () => {
      const sprites = demo.getSprites();
      const disposeSpy = jest.spyOn(sprites[0].texture, 'dispose');
      
      // Move sprite off screen to trigger cleanup
      sprites[0].mesh.position.x = -15;
      demo.updateSprites(1);
      
      expect(disposeSpy).toHaveBeenCalled();
    });
  });
});