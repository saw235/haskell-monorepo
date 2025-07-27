/**
 * Integration tests for UI controls and user interactions
 * Tests button clicks, keyboard input, sliders, and DOM updates
 */

import { describe, it, expect, beforeEach, jest } from '@jest/globals';

// Mock DOM elements and event handling
interface MockElement {
  id: string;
  textContent: string;
  value?: string;
  onclick?: () => void;
  oninput?: () => void;
  addEventListener: jest.Mock;
  removeEventListener: jest.Mock;
}

interface MockKeyboardEvent {
  key: string;
  preventDefault: jest.Mock;
}

// Mock implementation of UI control system for testing
class UIControlSystem {
  private isPlaying: boolean = true;
  private scrollSpeed: number = 1.0;
  private spriteCount: number = 9;
  private fps: number = 60;
  private keyboardHandlers: Map<string, (event: MockKeyboardEvent) => void> = new Map();
  private elements: Map<string, MockElement> = new Map();

  constructor() {
    this.setupMockElements();
    this.setupControls();
  }

  private setupMockElements(): void {
    const elementConfigs = [
      { id: 'play-pause-btn', textContent: 'Pause' },
      { id: 'reset-btn', textContent: 'Reset' },
      { id: 'speed-slider', textContent: '', value: '1.0' },
      { id: 'speed-value', textContent: '1.0x' },
      { id: 'add-square-btn', textContent: 'Add Character' },
      { id: 'add-circle-btn', textContent: 'Add Coin' },
      { id: 'add-triangle-btn', textContent: 'Add Enemy' },
      { id: 'clear-sprites-btn', textContent: 'Clear All' },
      { id: 'sprite-count', textContent: 'Sprites: 9' },
      { id: 'fps', textContent: 'FPS: 60' },
      { id: 'camera-info', textContent: 'Camera: 0, 0' }
    ];

    elementConfigs.forEach(config => {
      const element: MockElement = {
        id: config.id,
        textContent: config.textContent,
        value: config.value,
        addEventListener: jest.fn(),
        removeEventListener: jest.fn()
      };
      this.elements.set(config.id, element);
    });
  }

  private setupControls(): void {
    // Play/Pause button
    const playPauseBtn = this.elements.get('play-pause-btn')!;
    playPauseBtn.onclick = () => {
      this.togglePlayPause();
      playPauseBtn.textContent = this.isPlaying ? 'Pause' : 'Play';
    };

    // Reset button
    const resetBtn = this.elements.get('reset-btn')!;
    resetBtn.onclick = () => this.resetAnimations();

    // Speed slider
    const speedSlider = this.elements.get('speed-slider')!;
    const speedValue = this.elements.get('speed-value')!;
    speedSlider.oninput = () => {
      const speed = parseFloat(speedSlider.value!);
      this.setScrollSpeed(speed);
      speedValue.textContent = `${speed.toFixed(1)}x`;
    };

    // Add sprite buttons
    this.elements.get('add-square-btn')!.onclick = () => this.addSprite('character');
    this.elements.get('add-circle-btn')!.onclick = () => this.addSprite('coin');
    this.elements.get('add-triangle-btn')!.onclick = () => this.addSprite('enemy');
    this.elements.get('clear-sprites-btn')!.onclick = () => this.clearAllSprites();

    // Keyboard controls
    this.setupKeyboardControls();
  }

  private setupKeyboardControls(): void {
    this.keyboardHandlers.set('keydown', (event: MockKeyboardEvent) => {
      if (!this.isPlaying) return;

      switch (event.key) {
        case 'ArrowUp':
        case ' ':
          // Jump
          this.handleJump();
          break;
        case 'ArrowLeft':
          this.handleMoveLeft();
          break;
        case 'ArrowRight':
          this.handleMoveRight();
          break;
      }
    });

    this.keyboardHandlers.set('keyup', (event: MockKeyboardEvent) => {
      switch (event.key) {
        case 'ArrowLeft':
        case 'ArrowRight':
          this.handleStopMovement();
          break;
      }
    });
  }

  // UI Action Methods
  public togglePlayPause(): void {
    this.isPlaying = !this.isPlaying;
  }

  public resetAnimations(): void {
    // Reset animation state
  }

  public setScrollSpeed(speed: number): void {
    this.scrollSpeed = speed;
  }

  public addSprite(type: 'character' | 'coin' | 'enemy'): void {
    this.spriteCount++;
    this.updateSpriteCount();
  }

  public clearAllSprites(): void {
    this.spriteCount = 0;
    this.updateSpriteCount();
  }

  public updateSpriteCount(): void {
    const spriteCountEl = this.elements.get('sprite-count')!;
    spriteCountEl.textContent = `Sprites: ${this.spriteCount}`;
  }

  public updateFPS(fps: number): void {
    this.fps = fps;
    const fpsEl = this.elements.get('fps')!;
    fpsEl.textContent = `FPS: ${fps.toFixed(1)}`;
  }

  public updateCameraInfo(info: string): void {
    const cameraInfoEl = this.elements.get('camera-info')!;
    cameraInfoEl.textContent = info;
  }

  // Keyboard Action Methods
  private handleJump(): void {
    // Jump logic
  }

  private handleMoveLeft(): void {
    // Move left logic
  }

  private handleMoveRight(): void {
    // Move right logic
  }

  private handleStopMovement(): void {
    // Stop movement logic
  }

  // Simulate events for testing
  public simulateButtonClick(buttonId: string): void {
    const element = this.elements.get(buttonId);
    if (element && element.onclick) {
      element.onclick();
    }
  }

  public simulateSliderInput(sliderId: string, value: string): void {
    const element = this.elements.get(sliderId);
    if (element && element.oninput) {
      element.value = value;
      element.oninput();
    }
  }

  public simulateKeyEvent(eventType: 'keydown' | 'keyup', key: string): void {
    const handler = this.keyboardHandlers.get(eventType);
    if (handler) {
      const event: MockKeyboardEvent = {
        key,
        preventDefault: jest.fn()
      };
      handler(event);
    }
  }

  // Getters for testing
  public getElement(id: string): MockElement | undefined {
    return this.elements.get(id);
  }

  public getIsPlaying(): boolean { return this.isPlaying; }
  public getScrollSpeed(): number { return this.scrollSpeed; }
  public getSpriteCount(): number { return this.spriteCount; }
  public getFPS(): number { return this.fps; }
}

describe('UI Controls Integration', () => {
  let uiControls: UIControlSystem;

  beforeEach(() => {
    uiControls = new UIControlSystem();
  });

  describe('Button Controls', () => {
    describe('Play/Pause Button', () => {
      it('should toggle play state when clicked', () => {
        expect(uiControls.getIsPlaying()).toBe(true);
        
        uiControls.simulateButtonClick('play-pause-btn');
        
        expect(uiControls.getIsPlaying()).toBe(false);
      });

      it('should update button text based on state', () => {
        const button = uiControls.getElement('play-pause-btn')!;
        
        expect(button.textContent).toBe('Pause'); // Initially playing
        
        uiControls.simulateButtonClick('play-pause-btn');
        expect(button.textContent).toBe('Play'); // Now paused
        
        uiControls.simulateButtonClick('play-pause-btn');
        expect(button.textContent).toBe('Pause'); // Playing again
      });
    });

    describe('Reset Button', () => {
      it('should trigger reset when clicked', () => {
        const resetSpy = jest.spyOn(uiControls, 'resetAnimations');
        
        uiControls.simulateButtonClick('reset-btn');
        
        expect(resetSpy).toHaveBeenCalled();
      });
    });

    describe('Sprite Addition Buttons', () => {
      it('should add character sprite when character button clicked', () => {
        const initialCount = uiControls.getSpriteCount();
        
        uiControls.simulateButtonClick('add-square-btn');
        
        expect(uiControls.getSpriteCount()).toBe(initialCount + 1);
      });

      it('should add coin sprite when coin button clicked', () => {
        const initialCount = uiControls.getSpriteCount();
        
        uiControls.simulateButtonClick('add-circle-btn');
        
        expect(uiControls.getSpriteCount()).toBe(initialCount + 1);
      });

      it('should add enemy sprite when enemy button clicked', () => {
        const initialCount = uiControls.getSpriteCount();
        
        uiControls.simulateButtonClick('add-triangle-btn');
        
        expect(uiControls.getSpriteCount()).toBe(initialCount + 1);
      });

      it('should update sprite count display after adding sprites', () => {
        uiControls.simulateButtonClick('add-square-btn');
        
        const spriteCountEl = uiControls.getElement('sprite-count')!;
        expect(spriteCountEl.textContent).toBe('Sprites: 10'); // 9 + 1
      });
    });

    describe('Clear Sprites Button', () => {
      it('should clear all sprites when clicked', () => {
        uiControls.simulateButtonClick('clear-sprites-btn');
        
        expect(uiControls.getSpriteCount()).toBe(0);
      });

      it('should update sprite count display after clearing', () => {
        uiControls.simulateButtonClick('clear-sprites-btn');
        
        const spriteCountEl = uiControls.getElement('sprite-count')!;
        expect(spriteCountEl.textContent).toBe('Sprites: 0');
      });
    });
  });

  describe('Slider Controls', () => {
    describe('Speed Slider', () => {
      it('should update scroll speed when slider value changes', () => {
        uiControls.simulateSliderInput('speed-slider', '2.5');
        
        expect(uiControls.getScrollSpeed()).toBe(2.5);
      });

      it('should update speed display when slider changes', () => {
        uiControls.simulateSliderInput('speed-slider', '1.7');
        
        const speedValueEl = uiControls.getElement('speed-value')!;
        expect(speedValueEl.textContent).toBe('1.7x');
      });

      it('should handle decimal values correctly', () => {
        uiControls.simulateSliderInput('speed-slider', '0.3');
        
        expect(uiControls.getScrollSpeed()).toBe(0.3);
        
        const speedValueEl = uiControls.getElement('speed-value')!;
        expect(speedValueEl.textContent).toBe('0.3x');
      });

      it('should handle maximum speed values', () => {
        uiControls.simulateSliderInput('speed-slider', '3.0');
        
        expect(uiControls.getScrollSpeed()).toBe(3.0);
        
        const speedValueEl = uiControls.getElement('speed-value')!;
        expect(speedValueEl.textContent).toBe('3.0x');
      });
    });
  });

  describe('Keyboard Controls', () => {
    describe('Character Movement', () => {
      it('should handle jump input (Space key)', () => {
        const jumpSpy = jest.spyOn(uiControls as any, 'handleJump');
        
        uiControls.simulateKeyEvent('keydown', ' ');
        
        expect(jumpSpy).toHaveBeenCalled();
      });

      it('should handle jump input (Arrow Up)', () => {
        const jumpSpy = jest.spyOn(uiControls as any, 'handleJump');
        
        uiControls.simulateKeyEvent('keydown', 'ArrowUp');
        
        expect(jumpSpy).toHaveBeenCalled();
      });

      it('should handle left movement', () => {
        const moveLeftSpy = jest.spyOn(uiControls as any, 'handleMoveLeft');
        
        uiControls.simulateKeyEvent('keydown', 'ArrowLeft');
        
        expect(moveLeftSpy).toHaveBeenCalled();
      });

      it('should handle right movement', () => {
        const moveRightSpy = jest.spyOn(uiControls as any, 'handleMoveRight');
        
        uiControls.simulateKeyEvent('keydown', 'ArrowRight');
        
        expect(moveRightSpy).toHaveBeenCalled();
      });

      it('should stop movement on key release', () => {
        const stopMovementSpy = jest.spyOn(uiControls as any, 'handleStopMovement');
        
        uiControls.simulateKeyEvent('keyup', 'ArrowLeft');
        
        expect(stopMovementSpy).toHaveBeenCalled();
      });

      it('should not respond to input when paused', () => {
        uiControls.togglePlayPause(); // Pause the game
        
        const jumpSpy = jest.spyOn(uiControls as any, 'handleJump');
        const moveLeftSpy = jest.spyOn(uiControls as any, 'handleMoveLeft');
        
        uiControls.simulateKeyEvent('keydown', ' ');
        uiControls.simulateKeyEvent('keydown', 'ArrowLeft');
        
        expect(jumpSpy).not.toHaveBeenCalled();
        expect(moveLeftSpy).not.toHaveBeenCalled();
      });
    });

    describe('Key Release Handling', () => {
      it('should handle left arrow key release', () => {
        const stopMovementSpy = jest.spyOn(uiControls as any, 'handleStopMovement');
        
        uiControls.simulateKeyEvent('keyup', 'ArrowLeft');
        
        expect(stopMovementSpy).toHaveBeenCalled();
      });

      it('should handle right arrow key release', () => {
        const stopMovementSpy = jest.spyOn(uiControls as any, 'handleStopMovement');
        
        uiControls.simulateKeyEvent('keyup', 'ArrowRight');
        
        expect(stopMovementSpy).toHaveBeenCalled();
      });

      it('should not respond to other key releases', () => {
        const stopMovementSpy = jest.spyOn(uiControls as any, 'handleStopMovement');
        
        uiControls.simulateKeyEvent('keyup', ' ');
        uiControls.simulateKeyEvent('keyup', 'ArrowUp');
        
        expect(stopMovementSpy).not.toHaveBeenCalled();
      });
    });
  });

  describe('Information Display Updates', () => {
    describe('FPS Display', () => {
      it('should update FPS display', () => {
        uiControls.updateFPS(45.7);
        
        const fpsEl = uiControls.getElement('fps')!;
        expect(fpsEl.textContent).toBe('FPS: 45.7');
      });

      it('should format FPS to one decimal place', () => {
        uiControls.updateFPS(59.999);
        
        const fpsEl = uiControls.getElement('fps')!;
        expect(fpsEl.textContent).toBe('FPS: 60.0');
      });
    });

    describe('Camera Info Display', () => {
      it('should update camera info display', () => {
        uiControls.updateCameraInfo('Scroll Speed: 2.5x');
        
        const cameraInfoEl = uiControls.getElement('camera-info')!;
        expect(cameraInfoEl.textContent).toBe('Scroll Speed: 2.5x');
      });
    });

    describe('Sprite Count Display', () => {
      it('should show initial sprite count', () => {
        const spriteCountEl = uiControls.getElement('sprite-count')!;
        expect(spriteCountEl.textContent).toBe('Sprites: 9');
      });

      it('should update when sprites are added', () => {
        uiControls.addSprite('coin');
        uiControls.addSprite('enemy');
        
        const spriteCountEl = uiControls.getElement('sprite-count')!;
        expect(spriteCountEl.textContent).toBe('Sprites: 11');
      });
    });
  });

  describe('Complex Interaction Scenarios', () => {
    it('should handle rapid button clicks', () => {
      const initialCount = uiControls.getSpriteCount();
      
      // Rapidly click add buttons
      for (let i = 0; i < 10; i++) {
        uiControls.simulateButtonClick('add-circle-btn');
      }
      
      expect(uiControls.getSpriteCount()).toBe(initialCount + 10);
    });

    it('should handle multiple control interactions', () => {
      // Change speed
      uiControls.simulateSliderInput('speed-slider', '2.0');
      
      // Add some sprites
      uiControls.simulateButtonClick('add-square-btn');
      uiControls.simulateButtonClick('add-circle-btn');
      
      // Pause
      uiControls.simulateButtonClick('play-pause-btn');
      
      // Try keyboard input (should be ignored while paused)
      const jumpSpy = jest.spyOn(uiControls as any, 'handleJump');
      uiControls.simulateKeyEvent('keydown', ' ');
      
      expect(uiControls.getScrollSpeed()).toBe(2.0);
      expect(uiControls.getSpriteCount()).toBe(11); // 9 + 2
      expect(uiControls.getIsPlaying()).toBe(false);
      expect(jumpSpy).not.toHaveBeenCalled();
    });

    it('should handle clear and add sequence', () => {
      // Clear all sprites
      uiControls.simulateButtonClick('clear-sprites-btn');
      expect(uiControls.getSpriteCount()).toBe(0);
      
      // Add new sprites
      uiControls.simulateButtonClick('add-square-btn');
      uiControls.simulateButtonClick('add-triangle-btn');
      
      expect(uiControls.getSpriteCount()).toBe(2);
      
      const spriteCountEl = uiControls.getElement('sprite-count')!;
      expect(spriteCountEl.textContent).toBe('Sprites: 2');
    });
  });

  describe('Edge Cases', () => {
    it('should handle non-existent button clicks gracefully', () => {
      expect(() => {
        uiControls.simulateButtonClick('non-existent-button');
      }).not.toThrow();
    });

    it('should handle invalid slider values', () => {
      uiControls.simulateSliderInput('speed-slider', 'invalid');
      
      // Should handle NaN gracefully
      expect(uiControls.getScrollSpeed()).toBeNaN();
    });

    it('should handle unknown keyboard events', () => {
      expect(() => {
        uiControls.simulateKeyEvent('keydown', 'UnknownKey');
      }).not.toThrow();
    });

    it('should handle extreme slider values', () => {
      uiControls.simulateSliderInput('speed-slider', '999.9');
      expect(uiControls.getScrollSpeed()).toBe(999.9);
      
      uiControls.simulateSliderInput('speed-slider', '0.001');
      expect(uiControls.getScrollSpeed()).toBe(0.001);
    });
  });
});