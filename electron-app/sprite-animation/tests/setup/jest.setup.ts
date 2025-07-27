// Global Jest setup file
import { beforeAll, afterAll, beforeEach, afterEach } from '@jest/globals';

// Setup Canvas mock
beforeAll(() => {
  // Mock HTMLCanvasElement
  global.HTMLCanvasElement = class HTMLCanvasElement {
    width = 800;
    height = 600;
    
    getContext() {
      return {
        fillStyle: '',
        fillRect: jest.fn(),
        clearRect: jest.fn(),
        drawImage: jest.fn(),
        canvas: { width: this.width, height: this.height }
      };
    }
  } as any;

  // Mock performance.now for consistent timing tests
  global.performance = {
    now: jest.fn(() => Date.now())
  } as any;

  // Mock window object
  Object.defineProperty(window, 'addEventListener', {
    value: jest.fn(),
    writable: true
  });

  Object.defineProperty(window, 'removeEventListener', {
    value: jest.fn(),
    writable: true
  });

  // Mock document.getElementById
  Object.defineProperty(document, 'getElementById', {
    value: jest.fn((id: string) => ({
      id,
      textContent: '',
      onclick: null,
      oninput: null,
      value: '1',
      addEventListener: jest.fn(),
      removeEventListener: jest.fn()
    })),
    writable: true
  });
});

beforeEach(() => {
  // Clear all mocks before each test
  jest.clearAllMocks();
});

afterEach(() => {
  // Clean up any test artifacts
});

afterAll(() => {
  // Global cleanup
});