/**
 * Unit tests for movement physics and collision detection
 * Tests gravity, velocity updates, jumping, and boundary checks
 */

import { describe, it, expect, beforeEach, jest } from '@jest/globals';
import { MockVector3 } from '../mocks/babylon-mocks';

interface MovementState {
  position: MockVector3;
  velocity: MockVector3;
}

// Mock implementation of physics system for testing
class PhysicsEngine {
  private gravity: number = 9.8;
  private groundLevel: number = -3;
  private jumpForce: number = 3;
  private maxFallVelocity: number = -20;

  applyGravity(state: MovementState, deltaTime: number): void {
    const deltaSeconds = deltaTime / 1000;
    state.velocity.y -= this.gravity * deltaSeconds;
    
    // Clamp to maximum fall velocity
    if (state.velocity.y < this.maxFallVelocity) {
      state.velocity.y = this.maxFallVelocity;
    }
  }

  updatePosition(state: MovementState, deltaTime: number): void {
    const deltaSeconds = deltaTime / 1000;
    state.position.addInPlace(state.velocity.scale(deltaSeconds));
  }

  checkGroundCollision(state: MovementState): boolean {
    if (state.position.y <= this.groundLevel + 1) {
      state.position.y = this.groundLevel + 1;
      state.velocity.y = 0;
      return true;
    }
    return false;
  }

  jump(state: MovementState): boolean {
    // Can only jump if on ground
    if (state.position.y <= this.groundLevel + 1.1) {
      state.velocity.y = this.jumpForce;
      return true;
    }
    return false;
  }

  setHorizontalMovement(state: MovementState, direction: number, speed: number): void {
    state.velocity.x = direction * speed;
  }

  stopHorizontalMovement(state: MovementState): void {
    state.velocity.x = 0;
  }

  isOnGround(state: MovementState): boolean {
    return state.position.y <= this.groundLevel + 1.1;
  }

  checkOffScreenCollision(state: MovementState, screenBounds: { left: number; right: number }): boolean {
    return state.position.x < screenBounds.left || state.position.x > screenBounds.right;
  }

  // Getters and setters for testing
  getGravity(): number { return this.gravity; }
  setGravity(value: number): void { this.gravity = value; }
  
  getGroundLevel(): number { return this.groundLevel; }
  setGroundLevel(value: number): void { this.groundLevel = value; }
  
  getJumpForce(): number { return this.jumpForce; }
  setJumpForce(value: number): void { this.jumpForce = value; }
}

describe('Movement Physics', () => {
  let physics: PhysicsEngine;
  let movementState: MovementState;

  beforeEach(() => {
    physics = new PhysicsEngine();
    movementState = {
      position: new MockVector3(0, 0, 0),
      velocity: new MockVector3(0, 0, 0)
    };
  });

  describe('Gravity System', () => {
    it('should apply downward acceleration', () => {
      const deltaTime = 1000; // 1 second
      
      physics.applyGravity(movementState, deltaTime);
      
      expect(movementState.velocity.y).toBe(-9.8);
    });

    it('should accumulate gravity over time', () => {
      const deltaTime = 500; // 0.5 seconds
      
      physics.applyGravity(movementState, deltaTime);
      expect(movementState.velocity.y).toBe(-4.9);
      
      physics.applyGravity(movementState, deltaTime);
      expect(movementState.velocity.y).toBe(-9.8);
    });

    it('should clamp to maximum fall velocity', () => {
      movementState.velocity.y = -15; // Already falling fast
      const deltaTime = 2000; // 2 seconds of additional gravity
      
      physics.applyGravity(movementState, deltaTime);
      
      expect(movementState.velocity.y).toBe(-20); // Clamped to max
    });

    it('should work with custom gravity values', () => {
      physics.setGravity(5.0);
      const deltaTime = 1000;
      
      physics.applyGravity(movementState, deltaTime);
      
      expect(movementState.velocity.y).toBe(-5.0);
    });
  });

  describe('Position Updates', () => {
    it('should update position based on velocity', () => {
      movementState.velocity = new MockVector3(2, 3, 0);
      const deltaTime = 1000; // 1 second
      
      physics.updatePosition(movementState, deltaTime);
      
      expect(movementState.position.x).toBe(2);
      expect(movementState.position.y).toBe(3);
      expect(movementState.position.z).toBe(0);
    });

    it('should handle partial time steps', () => {
      movementState.velocity = new MockVector3(4, 2, 0);
      const deltaTime = 500; // 0.5 seconds
      
      physics.updatePosition(movementState, deltaTime);
      
      expect(movementState.position.x).toBe(2);
      expect(movementState.position.y).toBe(1);
    });

    it('should handle negative velocities', () => {
      movementState.velocity = new MockVector3(-3, -2, 0);
      const deltaTime = 1000;
      
      physics.updatePosition(movementState, deltaTime);
      
      expect(movementState.position.x).toBe(-3);
      expect(movementState.position.y).toBe(-2);
    });
  });

  describe('Ground Collision', () => {
    it('should detect ground collision', () => {
      movementState.position.y = -3.5; // Below ground
      
      const collision = physics.checkGroundCollision(movementState);
      
      expect(collision).toBe(true);
      expect(movementState.position.y).toBe(-2); // groundLevel + 1
      expect(movementState.velocity.y).toBe(0);
    });

    it('should not trigger collision when above ground', () => {
      movementState.position.y = 0; // Above ground
      movementState.velocity.y = -5;
      
      const collision = physics.checkGroundCollision(movementState);
      
      expect(collision).toBe(false);
      expect(movementState.velocity.y).toBe(-5); // Velocity unchanged
    });

    it('should handle exact ground level', () => {
      movementState.position.y = -2; // Exactly at ground level + 1
      
      const collision = physics.checkGroundCollision(movementState);
      
      expect(collision).toBe(true);
      expect(movementState.velocity.y).toBe(0);
    });

    it('should work with custom ground levels', () => {
      physics.setGroundLevel(0);
      movementState.position.y = 0.5; // Below new ground level
      
      const collision = physics.checkGroundCollision(movementState);
      
      expect(collision).toBe(true);
      expect(movementState.position.y).toBe(1); // newGroundLevel + 1
    });
  });

  describe('Jumping Mechanics', () => {
    it('should allow jumping when on ground', () => {
      movementState.position.y = -2; // On ground
      
      const jumpSuccess = physics.jump(movementState);
      
      expect(jumpSuccess).toBe(true);
      expect(movementState.velocity.y).toBe(3); // jumpForce
    });

    it('should not allow jumping when in air', () => {
      movementState.position.y = 2; // High in air
      
      const jumpSuccess = physics.jump(movementState);
      
      expect(jumpSuccess).toBe(false);
      expect(movementState.velocity.y).toBe(0); // No change
    });

    it('should allow jumping with small tolerance above ground', () => {
      movementState.position.y = -1.9; // Slightly above ground (within tolerance)
      
      const jumpSuccess = physics.jump(movementState);
      
      expect(jumpSuccess).toBe(true);
      expect(movementState.velocity.y).toBe(3);
    });

    it('should work with custom jump force', () => {
      physics.setJumpForce(5.0);
      movementState.position.y = -2;
      
      physics.jump(movementState);
      
      expect(movementState.velocity.y).toBe(5.0);
    });
  });

  describe('Horizontal Movement', () => {
    it('should set rightward movement', () => {
      physics.setHorizontalMovement(movementState, 1, 5);
      
      expect(movementState.velocity.x).toBe(5);
    });

    it('should set leftward movement', () => {
      physics.setHorizontalMovement(movementState, -1, 3);
      
      expect(movementState.velocity.x).toBe(-3);
    });

    it('should stop horizontal movement', () => {
      movementState.velocity.x = 10;
      
      physics.stopHorizontalMovement(movementState);
      
      expect(movementState.velocity.x).toBe(0);
    });

    it('should not affect vertical velocity', () => {
      movementState.velocity.y = 5;
      
      physics.setHorizontalMovement(movementState, 1, 3);
      
      expect(movementState.velocity.y).toBe(5); // Unchanged
    });
  });

  describe('Ground Detection', () => {
    it('should detect when on ground', () => {
      movementState.position.y = -2;
      
      expect(physics.isOnGround(movementState)).toBe(true);
    });

    it('should detect when not on ground', () => {
      movementState.position.y = 0;
      
      expect(physics.isOnGround(movementState)).toBe(false);
    });

    it('should use tolerance for ground detection', () => {
      movementState.position.y = -1.95; // Within tolerance
      
      expect(physics.isOnGround(movementState)).toBe(true);
    });
  });

  describe('Screen Boundary Collision', () => {
    it('should detect left boundary collision', () => {
      movementState.position.x = -15;
      const bounds = { left: -10, right: 10 };
      
      const collision = physics.checkOffScreenCollision(movementState, bounds);
      
      expect(collision).toBe(true);
    });

    it('should detect right boundary collision', () => {
      movementState.position.x = 15;
      const bounds = { left: -10, right: 10 };
      
      const collision = physics.checkOffScreenCollision(movementState, bounds);
      
      expect(collision).toBe(true);
    });

    it('should not detect collision when within bounds', () => {
      movementState.position.x = 5;
      const bounds = { left: -10, right: 10 };
      
      const collision = physics.checkOffScreenCollision(movementState, bounds);
      
      expect(collision).toBe(false);
    });

    it('should handle exact boundary values', () => {
      const bounds = { left: -10, right: 10 };
      
      movementState.position.x = -10;
      expect(physics.checkOffScreenCollision(movementState, bounds)).toBe(false);
      
      movementState.position.x = 10;
      expect(physics.checkOffScreenCollision(movementState, bounds)).toBe(false);
    });
  });

  describe('Integration Tests', () => {
    it('should handle complete jump cycle', () => {
      movementState.position.y = -2; // Start on ground
      
      // Jump
      const jumpSuccess = physics.jump(movementState);
      expect(jumpSuccess).toBe(true);
      expect(movementState.velocity.y).toBe(3);
      
      // Apply physics for upward motion
      physics.updatePosition(movementState, 500); // 0.5 seconds
      expect(movementState.position.y).toBe(-0.5); // -2 + (3 * 0.5)
      
      physics.applyGravity(movementState, 500);
      expect(movementState.velocity.y).toBe(-1.9); // 3 - (9.8 * 0.5)
      
      // Continue until ground collision
      physics.updatePosition(movementState, 500); // Another 0.5 seconds
      physics.applyGravity(movementState, 500);
      
      // Should be falling
      expect(movementState.velocity.y).toBeLessThan(0);
      
      // Eventually hits ground
      movementState.position.y = -3; // Simulate hitting ground
      const collision = physics.checkGroundCollision(movementState);
      expect(collision).toBe(true);
      expect(movementState.velocity.y).toBe(0);
    });

    it('should handle horizontal movement with physics', () => {
      movementState.position = new MockVector3(0, -2, 0); // Start on ground
      
      // Start moving right
      physics.setHorizontalMovement(movementState, 1, 4);
      
      // Update for 1 second
      physics.updatePosition(movementState, 1000);
      
      expect(movementState.position.x).toBe(4);
      expect(movementState.position.y).toBe(-2); // Still on ground
    });

    it('should handle complex movement scenario', () => {
      movementState.position = new MockVector3(-5, -2, 0);
      
      // Jump and move right
      physics.jump(movementState);
      physics.setHorizontalMovement(movementState, 1, 2);
      
      // Simulate physics for 0.5 seconds
      physics.updatePosition(movementState, 500);
      physics.applyGravity(movementState, 500);
      
      expect(movementState.position.x).toBe(-4); // -5 + (2 * 0.5)
      expect(movementState.position.y).toBeGreaterThan(-2); // Should be above ground
      expect(movementState.velocity.y).toBeLessThan(3); // Gravity affecting jump
    });
  });

  describe('Edge Cases', () => {
    it('should handle zero deltaTime', () => {
      const initialState = {
        position: movementState.position.clone(),
        velocity: movementState.velocity.clone()
      };
      
      physics.applyGravity(movementState, 0);
      physics.updatePosition(movementState, 0);
      
      expect(movementState.position.x).toBe(initialState.position.x);
      expect(movementState.position.y).toBe(initialState.position.y);
      expect(movementState.velocity.y).toBe(initialState.velocity.y);
    });

    it('should handle very large deltaTime', () => {
      const hugeDeltaTime = 1000000; // Very large time
      
      expect(() => {
        physics.applyGravity(movementState, hugeDeltaTime);
        physics.updatePosition(movementState, hugeDeltaTime);
      }).not.toThrow();
      
      // Should still clamp to maximum fall velocity
      expect(movementState.velocity.y).toBeGreaterThanOrEqual(-20);
    });

    it('should handle negative deltaTime gracefully', () => {
      expect(() => {
        physics.applyGravity(movementState, -1000);
        physics.updatePosition(movementState, -1000);
      }).not.toThrow();
    });
  });
});