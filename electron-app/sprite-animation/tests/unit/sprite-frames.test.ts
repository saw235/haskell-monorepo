/**
 * Unit tests for sprite frame creation functions
 * Tests the pixel art frame data structures and validation
 */

import { describe, it, expect, beforeEach } from '@jest/globals';

// We need to create a testable version of the sprite frame functions
// Since they're currently private methods in the class, we'll extract them for testing
interface PixelArtFrame {
  width: number;
  height: number;
  pixels: string[][];
}

// Mock implementation extracted from the main class for testing
class SpriteFrameCreator {
  createCharacterFrames(): PixelArtFrame[] {
    return [
      // Frame 1 - Standing
      {
        width: 8,
        height: 8,
        pixels: [
          ["#000000", "#FFDBAC", "#FFDBAC", "#000000", "#000000", "#000000", "#000000", "#000000"],
          ["#000000", "#FFDBAC", "#FFDBAC", "#FFDBAC", "#000000", "#000000", "#000000", "#000000"],
          ["#000000", "#8B4513", "#8B4513", "#8B4513", "#8B4513", "#000000", "#000000", "#000000"],
          ["#000000", "#FF0000", "#FF0000", "#FF0000", "#FF0000", "#000000", "#000000", "#000000"],
          ["#000000", "#FF0000", "#FFFF00", "#FF0000", "#FF0000", "#000000", "#000000", "#000000"],
          ["#000000", "#0000FF", "#0000FF", "#0000FF", "#0000FF", "#000000", "#000000", "#000000"],
          ["#000000", "#8B4513", "#000000", "#8B4513", "#000000", "#8B4513", "#000000", "#000000"],
          ["#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
        ],
      },
      // Frame 2 - Walking
      {
        width: 8,
        height: 8,
        pixels: [
          ["#000000", "#FFDBAC", "#FFDBAC", "#000000", "#000000", "#000000", "#000000", "#000000"],
          ["#000000", "#FFDBAC", "#FFDBAC", "#FFDBAC", "#000000", "#000000", "#000000", "#000000"],
          ["#000000", "#8B4513", "#8B4513", "#8B4513", "#8B4513", "#000000", "#000000", "#000000"],
          ["#000000", "#FF0000", "#FF0000", "#FF0000", "#FF0000", "#000000", "#000000", "#000000"],
          ["#000000", "#FF0000", "#FFFF00", "#FF0000", "#FF0000", "#000000", "#000000", "#000000"],
          ["#000000", "#0000FF", "#0000FF", "#0000FF", "#0000FF", "#000000", "#000000", "#000000"],
          ["#000000", "#000000", "#8B4513", "#000000", "#8B4513", "#000000", "#000000", "#000000"],
          ["#000000", "#8B4513", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
        ],
      },
    ];
  }

  createCoinFrames(): PixelArtFrame[] {
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

  createEnemyFrames(): PixelArtFrame[] {
    return [
      // Frame 1 - Enemy normal
      {
        width: 8,
        height: 8,
        pixels: [
          ["#000000", "#000000", "#8B008B", "#8B008B", "#8B008B", "#000000", "#000000", "#000000"],
          ["#000000", "#8B008B", "#FF1493", "#FF1493", "#FF1493", "#8B008B", "#000000", "#000000"],
          ["#8B008B", "#FF1493", "#FF0000", "#000000", "#FF0000", "#FF1493", "#8B008B", "#000000"],
          ["#8B008B", "#FF1493", "#FF1493", "#FF1493", "#FF1493", "#FF1493", "#8B008B", "#000000"],
          ["#8B008B", "#FF1493", "#000000", "#FF1493", "#000000", "#FF1493", "#8B008B", "#000000"],
          ["#000000", "#8B008B", "#FF1493", "#000000", "#FF1493", "#8B008B", "#000000", "#000000"],
          ["#000000", "#000000", "#8B008B", "#8B008B", "#8B008B", "#000000", "#000000", "#000000"],
          ["#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
        ],
      },
      // Frame 2 - Enemy angry
      {
        width: 8,
        height: 8,
        pixels: [
          ["#000000", "#000000", "#8B008B", "#8B008B", "#8B008B", "#000000", "#000000", "#000000"],
          ["#000000", "#8B008B", "#FF1493", "#FF1493", "#FF1493", "#8B008B", "#000000", "#000000"],
          ["#8B008B", "#FF1493", "#FF0000", "#000000", "#FF0000", "#FF1493", "#8B008B", "#000000"],
          ["#8B008B", "#FF1493", "#FF1493", "#FF0000", "#FF1493", "#FF1493", "#8B008B", "#000000"],
          ["#8B008B", "#FF1493", "#000000", "#FF1493", "#000000", "#FF1493", "#8B008B", "#000000"],
          ["#000000", "#8B008B", "#FF1493", "#000000", "#FF1493", "#8B008B", "#000000", "#000000"],
          ["#000000", "#000000", "#8B008B", "#8B008B", "#8B008B", "#000000", "#000000", "#000000"],
          ["#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000", "#000000"],
        ],
      },
    ];
  }
}

describe('Sprite Frame Creation', () => {
  let frameCreator: SpriteFrameCreator;

  beforeEach(() => {
    frameCreator = new SpriteFrameCreator();
  });

  describe('Character Frames', () => {
    it('should create character frames with correct structure', () => {
      const frames = frameCreator.createCharacterFrames();
      
      expect(frames).toHaveLength(2);
      expect(frames[0]).toHaveProperty('width', 8);
      expect(frames[0]).toHaveProperty('height', 8);
      expect(frames[0]).toHaveProperty('pixels');
      expect(frames[0].pixels).toHaveLength(8);
      expect(frames[0].pixels[0]).toHaveLength(8);
    });

    it('should have valid hex color values', () => {
      const frames = frameCreator.createCharacterFrames();
      const hexColorPattern = /^#[0-9A-F]{6}$/i;
      
      frames.forEach((frame, frameIndex) => {
        frame.pixels.forEach((row, rowIndex) => {
          row.forEach((pixel, colIndex) => {
            expect(pixel).toMatch(hexColorPattern);
          }, `Frame ${frameIndex}, Row ${rowIndex}, Col ${colIndex} has invalid color: ${pixel}`);
        });
      });
    });

    it('should have different pixels between frames (animation)', () => {
      const frames = frameCreator.createCharacterFrames();
      const frame1 = frames[0];
      const frame2 = frames[1];
      
      // Check that at least some pixels are different
      let differentPixels = 0;
      for (let y = 0; y < frame1.height; y++) {
        for (let x = 0; x < frame1.width; x++) {
          if (frame1.pixels[y][x] !== frame2.pixels[y][x]) {
            differentPixels++;
          }
        }
      }
      
      expect(differentPixels).toBeGreaterThan(0);
    });
  });

  describe('Coin Frames', () => {
    it('should create coin frames with correct structure', () => {
      const frames = frameCreator.createCoinFrames();
      
      expect(frames).toHaveLength(2);
      expect(frames[0]).toHaveProperty('width', 6);
      expect(frames[0]).toHaveProperty('height', 6);
      expect(frames[0]).toHaveProperty('pixels');
      expect(frames[0].pixels).toHaveLength(6);
      expect(frames[0].pixels[0]).toHaveLength(6);
    });

    it('should contain gold colors', () => {
      const frames = frameCreator.createCoinFrames();
      const goldColors = ['#FFD700', '#FFFF00', '#FFA500'];
      
      frames.forEach(frame => {
        let hasGoldColor = false;
        frame.pixels.forEach(row => {
          row.forEach(pixel => {
            if (goldColors.includes(pixel)) {
              hasGoldColor = true;
            }
          });
        });
        expect(hasGoldColor).toBe(true);
      });
    });

    it('should show spinning animation effect', () => {
      const frames = frameCreator.createCoinFrames();
      const frame1 = frames[0]; // Full coin
      const frame2 = frames[1]; // Thin coin
      
      // Count non-black pixels in each frame
      const countNonBlackPixels = (frame: PixelArtFrame) => {
        let count = 0;
        frame.pixels.forEach(row => {
          row.forEach(pixel => {
            if (pixel !== '#000000') count++;
          });
        });
        return count;
      };
      
      const frame1NonBlack = countNonBlackPixels(frame1);
      const frame2NonBlack = countNonBlackPixels(frame2);
      
      // Frame 2 (thin coin) should have fewer non-black pixels
      expect(frame2NonBlack).toBeLessThan(frame1NonBlack);
    });
  });

  describe('Enemy Frames', () => {
    it('should create enemy frames with correct structure', () => {
      const frames = frameCreator.createEnemyFrames();
      
      expect(frames).toHaveLength(2);
      expect(frames[0]).toHaveProperty('width', 8);
      expect(frames[0]).toHaveProperty('height', 8);
      expect(frames[0]).toHaveProperty('pixels');
      expect(frames[0].pixels).toHaveLength(8);
      expect(frames[0].pixels[0]).toHaveLength(8);
    });

    it('should contain purple/magenta colors', () => {
      const frames = frameCreator.createEnemyFrames();
      const enemyColors = ['#8B008B', '#FF1493'];
      
      frames.forEach(frame => {
        let hasEnemyColor = false;
        frame.pixels.forEach(row => {
          row.forEach(pixel => {
            if (enemyColors.includes(pixel)) {
              hasEnemyColor = true;
            }
          });
        });
        expect(hasEnemyColor).toBe(true);
      });
    });

    it('should show angry animation (difference between frames)', () => {
      const frames = frameCreator.createEnemyFrames();
      const frame1 = frames[0]; // Normal
      const frame2 = frames[1]; // Angry
      
      // Look for red pixels difference (angry face has more red)
      const countRedPixels = (frame: PixelArtFrame) => {
        let count = 0;
        frame.pixels.forEach(row => {
          row.forEach(pixel => {
            if (pixel === '#FF0000') count++;
          });
        });
        return count;
      };
      
      const frame1Red = countRedPixels(frame1);
      const frame2Red = countRedPixels(frame2);
      
      // Frame 2 (angry) should have more red pixels
      expect(frame2Red).toBeGreaterThan(frame1Red);
    });
  });

  describe('Frame Validation', () => {
    it('should have consistent pixel array dimensions', () => {
      const allFrames = [
        ...frameCreator.createCharacterFrames(),
        ...frameCreator.createCoinFrames(),
        ...frameCreator.createEnemyFrames()
      ];
      
      allFrames.forEach((frame, index) => {
        expect(frame.pixels).toHaveLength(frame.height);
        frame.pixels.forEach((row, rowIndex) => {
          expect(row).toHaveLength(frame.width);
        }, `Frame ${index}, row ${rowIndex} width mismatch`);
      });
    });

    it('should not have any undefined or null pixels', () => {
      const allFrames = [
        ...frameCreator.createCharacterFrames(),
        ...frameCreator.createCoinFrames(),
        ...frameCreator.createEnemyFrames()
      ];
      
      allFrames.forEach((frame, frameIndex) => {
        frame.pixels.forEach((row, rowIndex) => {
          row.forEach((pixel, colIndex) => {
            expect(pixel).toBeDefined();
            expect(pixel).not.toBeNull();
            expect(typeof pixel).toBe('string');
          }, `Frame ${frameIndex}, Row ${rowIndex}, Col ${colIndex}`);
        });
      });
    });
  });
});