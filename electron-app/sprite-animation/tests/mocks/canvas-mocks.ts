// Canvas API mocks for testing

export const mockCanvasContext = {
  fillStyle: '',
  strokeStyle: '',
  lineWidth: 1,
  font: '',
  textAlign: 'start' as CanvasTextAlign,
  textBaseline: 'alphabetic' as CanvasTextBaseline,
  
  // Drawing methods
  fillRect: jest.fn(),
  strokeRect: jest.fn(),
  clearRect: jest.fn(),
  fillText: jest.fn(),
  strokeText: jest.fn(),
  drawImage: jest.fn(),
  
  // Path methods
  beginPath: jest.fn(),
  closePath: jest.fn(),
  moveTo: jest.fn(),
  lineTo: jest.fn(),
  arc: jest.fn(),
  stroke: jest.fn(),
  fill: jest.fn(),
  
  // Transform methods
  save: jest.fn(),
  restore: jest.fn(),
  scale: jest.fn(),
  rotate: jest.fn(),
  translate: jest.fn(),
  transform: jest.fn(),
  setTransform: jest.fn(),
  
  // Properties
  canvas: {
    width: 800,
    height: 600,
    toDataURL: jest.fn(() => 'data:image/png;base64,mock'),
    toBlob: jest.fn()
  }
};

export const mockCanvas = {
  width: 800,
  height: 600,
  getContext: jest.fn(() => mockCanvasContext),
  toDataURL: jest.fn(() => 'data:image/png;base64,mock'),
  toBlob: jest.fn(),
  addEventListener: jest.fn(),
  removeEventListener: jest.fn()
};

// Mock HTML Canvas Element constructor
export const MockHTMLCanvasElement = jest.fn().mockImplementation(() => mockCanvas);

// Setup global mocks
beforeEach(() => {
  // Reset all canvas mock calls
  Object.values(mockCanvasContext).forEach(method => {
    if (typeof method === 'function' && method.mockClear) {
      method.mockClear();
    }
  });
  
  if (mockCanvas.getContext.mockClear) {
    mockCanvas.getContext.mockClear();
  }
});