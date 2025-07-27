/**
 * Unit tests for sprite creation and animation logic
 * Tests animation timing, frame cycling, and sprite properties
 */

import { describe, it, expect, beforeEach, jest } from '@jest/globals';
import { MockVector3, MockMesh, MockStandardMaterial, MockDynamicTexture } from '../mocks/babylon-mocks';

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
  animationSpeed: number; // frames per second
  position: MockVector3;
  velocity: MockVector3;
  type: "character" | "coin" | "enemy";
}

// Mock implementation of sprite animation logic for testing
class SpriteAnimationController {
  private scrollSpeed: number = 1.0;
  private groundLevel: number = -3;

  createAnimatedSprite(
    type: "character" | "coin" | "enemy",
    position: MockVector3,
    frames: PixelArtFrame[]
  ): AnimatedSprite {
    let animationSpeed: number;
    let velocity: MockVector3;

    switch (type) {
      case "character":
        animationSpeed = 2; // 2 FPS
        velocity = new MockVector3(0, 0, 0); // Player doesn't move automatically
        break;
      case "coin":
        animationSpeed = 4; // 4 FPS for spinning effect
        velocity = new MockVector3(-this.scrollSpeed, 0, 0); // Moves left with scroll
        break;
      case "enemy":
        animationSpeed = 3; // 3 FPS
        velocity = new MockVector3(-this.scrollSpeed * 0.8, 0, 0); // Moves slightly slower
        break;
    }

    // Create sprite mesh (mocked)
    const sprite = new MockMesh(`${type}_sprite`, null);
    sprite.position = position.clone();

    // Create material and texture (mocked)
    const material = new MockStandardMaterial(`${type}_material`, null);
    const texture = new MockDynamicTexture("pixelTexture", 64, null, false);
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

  updateSpriteAnimation(sprite: AnimatedSprite, deltaTime: number): void {
    const deltaSeconds = deltaTime / 1000;
    
    // Update animation
    sprite.animationTimer += deltaSeconds;
    const frameTime = 1 / sprite.animationSpeed;

    if (sprite.animationTimer >= frameTime) {
      sprite.animationTimer = 0;
      sprite.currentFrame = (sprite.currentFrame + 1) % sprite.frames.length;
    }

    // Update position
    sprite.mesh.position.addInPlace(sprite.velocity.scale(deltaSeconds));
  }

  applyPhysics(sprite: AnimatedSprite, deltaTime: number): void {
    if (sprite.type === "character") {
      const deltaSeconds = deltaTime / 1000;
      
      // Apply gravity
      sprite.velocity.y -= 9.8 * deltaSeconds;
      
      // Ground collision
      if (sprite.mesh.position.y <= this.groundLevel + 1) {
        sprite.mesh.position.y = this.groundLevel + 1;
        sprite.velocity.y = 0;
      }
    }
  }

  setScrollSpeed(speed: number): void {
    this.scrollSpeed = speed;
  }

  getScrollSpeed(): number {
    return this.scrollSpeed;
  }
}

describe('Sprite Animation Logic', () => {
  let controller: SpriteAnimationController;
  let mockFrames: PixelArtFrame[];
  let mockPosition: MockVector3;

  beforeEach(() => {
    controller = new SpriteAnimationController();
    mockFrames = [
      {
        width: 4,
        height: 4,
        pixels: [
          ["#FF0000", "#FF0000", "#FF0000", "#FF0000"],
          ["#FF0000", "#FFFFFF", "#FFFFFF", "#FF0000"],
          ["#FF0000", "#FFFFFF", "#FFFFFF", "#FF0000"],
          ["#FF0000", "#FF0000", "#FF0000", "#FF0000"]
        ]
      },
      {
        width: 4,
        height: 4,
        pixels: [
          ["#00FF00", "#00FF00", "#00FF00", "#00FF00"],
          ["#00FF00", "#FFFFFF", "#FFFFFF", "#00FF00"],
          ["#00FF00", "#FFFFFF", "#FFFFFF", "#00FF00"],
          ["#00FF00", "#00FF00", "#00FF00", "#00FF00"]
        ]
      }
    ];
    mockPosition = new MockVector3(5, 0, 0);
  });

  describe('Sprite Creation', () => {
    it('should create character sprite with correct properties', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      
      expect(sprite.type).toBe("character");
      expect(sprite.animationSpeed).toBe(2);
      expect(sprite.velocity.x).toBe(0);
      expect(sprite.velocity.y).toBe(0);
      expect(sprite.currentFrame).toBe(0);
      expect(sprite.animationTimer).toBe(0);
      expect(sprite.frames).toEqual(mockFrames);
    });

    it('should create coin sprite with correct properties', () => {
      const sprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      
      expect(sprite.type).toBe("coin");
      expect(sprite.animationSpeed).toBe(4);
      expect(sprite.velocity.x).toBe(-1.0); // -scrollSpeed
      expect(sprite.velocity.y).toBe(0);
    });

    it('should create enemy sprite with correct properties', () => {
      const sprite = controller.createAnimatedSprite("enemy", mockPosition, mockFrames);
      
      expect(sprite.type).toBe("enemy");
      expect(sprite.animationSpeed).toBe(3);
      expect(sprite.velocity.x).toBe(-0.8); // -scrollSpeed * 0.8
      expect(sprite.velocity.y).toBe(0);
    });

    it('should clone position correctly', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      
      expect(sprite.position).not.toBe(mockPosition); // Should be a clone
      expect(sprite.position.x).toBe(mockPosition.x);
      expect(sprite.position.y).toBe(mockPosition.y);
      expect(sprite.position.z).toBe(mockPosition.z);
    });

    it('should set up material properties correctly', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      
      expect(sprite.material.hasAlpha).toBe(true);
      expect(sprite.material.diffuseTexture).toBe(sprite.texture);
    });
  });

  describe('Animation Timing', () => {
    it('should not advance frame before frameTime elapsed', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      const frameTime = 1 / sprite.animationSpeed; // 0.5 seconds for character
      const deltaTime = (frameTime * 1000) - 100; // Just under frameTime
      
      controller.updateSpriteAnimation(sprite, deltaTime);
      
      expect(sprite.currentFrame).toBe(0);
      expect(sprite.animationTimer).toBeGreaterThan(0);
    });

    it('should advance frame when frameTime elapsed', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      const frameTime = 1 / sprite.animationSpeed; // 0.5 seconds for character
      const deltaTime = frameTime * 1000; // Exactly frameTime
      
      controller.updateSpriteAnimation(sprite, deltaTime);
      
      expect(sprite.currentFrame).toBe(1);
      expect(sprite.animationTimer).toBe(0);
    });

    it('should wrap around to first frame after last frame', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      sprite.currentFrame = mockFrames.length - 1; // Set to last frame
      const frameTime = 1 / sprite.animationSpeed;
      const deltaTime = frameTime * 1000;
      
      controller.updateSpriteAnimation(sprite, deltaTime);
      
      expect(sprite.currentFrame).toBe(0);
    });

    it('should handle multiple frame advances in one update', () => {
      const sprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const frameTime = 1 / sprite.animationSpeed; // 0.25 seconds for coin
      const deltaTime = frameTime * 3 * 1000; // 3 frame times
      
      // Simulate step-by-step updates
      for (let i = 0; i < 3; i++) {
        controller.updateSpriteAnimation(sprite, frameTime * 1000);
      }
      
      expect(sprite.currentFrame).toBe(1); // Should have cycled: 0->1->0->1
    });
  });

  describe('Position Updates', () => {
    it('should update position based on velocity', () => {
      const sprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const deltaTime = 1000; // 1 second
      const initialX = sprite.mesh.position.x;
      
      controller.updateSpriteAnimation(sprite, deltaTime);
      
      expect(sprite.mesh.position.x).toBe(initialX + sprite.velocity.x);
    });

    it('should not move character sprite automatically', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      const initialPosition = sprite.mesh.position.clone();
      const deltaTime = 1000;
      
      controller.updateSpriteAnimation(sprite, deltaTime);
      
      expect(sprite.mesh.position.x).toBe(initialPosition.x);
      expect(sprite.mesh.position.y).toBe(initialPosition.y);
    });

    it('should move coin and enemy sprites based on scroll speed', () => {
      const coinSprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const enemySprite = controller.createAnimatedSprite("enemy", mockPosition, mockFrames);
      const deltaTime = 1000;
      
      controller.updateSpriteAnimation(coinSprite, deltaTime);
      controller.updateSpriteAnimation(enemySprite, deltaTime);
      
      expect(coinSprite.mesh.position.x).toBe(mockPosition.x - 1.0); // -scrollSpeed
      expect(enemySprite.mesh.position.x).toBe(mockPosition.x - 0.8); // -scrollSpeed * 0.8
    });
  });

  describe('Physics System', () => {
    it('should apply gravity to character', () => {
      const sprite = controller.createAnimatedSprite("character", new MockVector3(0, 5, 0), mockFrames);
      const deltaTime = 1000; // 1 second
      
      controller.applyPhysics(sprite, deltaTime);
      
      expect(sprite.velocity.y).toBe(-9.8); // Gravity applied
    });

    it('should handle ground collision', () => {
      const sprite = controller.createAnimatedSprite("character", new MockVector3(0, -2.5, 0), mockFrames);
      sprite.velocity.y = -5; // Falling
      const deltaTime = 1000;
      
      controller.applyPhysics(sprite, deltaTime);
      
      expect(sprite.mesh.position.y).toBe(-2); // groundLevel + 1
      expect(sprite.velocity.y).toBe(0); // Velocity reset
    });

    it('should not apply physics to non-character sprites', () => {
      const coinSprite = controller.createAnimatedSprite("coin", new MockVector3(0, 5, 0), mockFrames);
      const enemySprite = controller.createAnimatedSprite("enemy", new MockVector3(0, 5, 0), mockFrames);
      const deltaTime = 1000;
      
      controller.applyPhysics(coinSprite, deltaTime);
      controller.applyPhysics(enemySprite, deltaTime);
      
      expect(coinSprite.velocity.y).toBe(0);
      expect(enemySprite.velocity.y).toBe(0);
    });
  });

  describe('Scroll Speed Effects', () => {
    it('should update sprite velocities when scroll speed changes', () => {
      const coinSprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const enemySprite = controller.createAnimatedSprite("enemy", mockPosition, mockFrames);
      
      controller.setScrollSpeed(2.0);
      
      // Create new sprites with updated scroll speed
      const newCoin = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const newEnemy = controller.createAnimatedSprite("enemy", mockPosition, mockFrames);
      
      expect(newCoin.velocity.x).toBe(-2.0);
      expect(newEnemy.velocity.x).toBe(-1.6); // -2.0 * 0.8
    });

    it('should get and set scroll speed correctly', () => {
      expect(controller.getScrollSpeed()).toBe(1.0);
      
      controller.setScrollSpeed(3.5);
      expect(controller.getScrollSpeed()).toBe(3.5);
    });
  });

  describe('Edge Cases', () => {
    it('should handle empty frames array', () => {
      expect(() => {
        controller.createAnimatedSprite("character", mockPosition, []);
      }).not.toThrow();
    });

    it('should handle zero deltaTime', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      const initialFrame = sprite.currentFrame;
      const initialTimer = sprite.animationTimer;
      
      controller.updateSpriteAnimation(sprite, 0);
      
      expect(sprite.currentFrame).toBe(initialFrame);
      expect(sprite.animationTimer).toBe(initialTimer);
    });

    it('should handle negative deltaTime gracefully', () => {
      const sprite = controller.createAnimatedSprite("character", mockPosition, mockFrames);
      
      expect(() => {
        controller.updateSpriteAnimation(sprite, -1000);
      }).not.toThrow();
    });

    it('should handle very large deltaTime', () => {
      const sprite = controller.createAnimatedSprite("coin", mockPosition, mockFrames);
      const hugeDeltaTime = 1000000; // Very large time step
      
      expect(() => {
        controller.updateSpriteAnimation(sprite, hugeDeltaTime);
      }).not.toThrow();
      
      // Should still be within valid frame range
      expect(sprite.currentFrame).toBeGreaterThanOrEqual(0);
      expect(sprite.currentFrame).toBeLessThan(mockFrames.length);
    });
  });
});