// Mock implementations for Babylon.js components

export class MockVector3 {
  constructor(public x: number = 0, public y: number = 0, public z: number = 0) {}
  
  static Zero() {
    return new MockVector3(0, 0, 0);
  }
  
  clone() {
    return new MockVector3(this.x, this.y, this.z);
  }
  
  addInPlace(vector: MockVector3) {
    this.x += vector.x;
    this.y += vector.y;
    this.z += vector.z;
    return this;
  }
  
  scale(factor: number) {
    return new MockVector3(this.x * factor, this.y * factor, this.z * factor);
  }
}

export class MockColor3 {
  constructor(public r: number, public g: number, public b: number) {}
}

export class MockDynamicTexture {
  private _context: any;
  
  constructor(public name: string, public size: number, public scene: any, public generateMipMaps: boolean = true) {
    this._context = {
      fillStyle: '',
      fillRect: jest.fn(),
      clearRect: jest.fn(),
      canvas: { width: size, height: size }
    };
  }
  
  getContext() {
    return this._context;
  }
  
  update() {
    return this;
  }
  
  dispose() {
    // Mock disposal
  }
}

export class MockStandardMaterial {
  public diffuseTexture: any = null;
  public diffuseColor: MockColor3 | null = null;
  public hasAlpha: boolean = false;
  
  constructor(public name: string, public scene: any) {}
  
  dispose() {
    // Mock disposal
  }
}

export class MockMesh {
  public position: MockVector3 = new MockVector3();
  public material: any = null;
  
  constructor(public name: string, public scene: any) {}
  
  dispose() {
    // Mock disposal
  }
}

export class MockScene {
  constructor(public engine: any) {}
  
  render() {
    // Mock render
  }
  
  dispose() {
    // Mock disposal
  }
}

export class MockEngine {
  private _fps = 60;
  private _renderLoop: (() => void) | null = null;
  
  constructor(public canvas: HTMLCanvasElement, public antialias: boolean) {}
  
  runRenderLoop(renderFunction: () => void) {
    this._renderLoop = renderFunction;
    // Don't actually run the loop in tests
  }
  
  getFps() {
    return this._fps;
  }
  
  getRenderingCanvas() {
    return this.canvas;
  }
  
  resize() {
    // Mock resize
  }
  
  dispose() {
    // Mock disposal
  }
}

export class MockFreeCamera {
  public position: MockVector3 = new MockVector3();
  public mode: number = 0;
  public orthoLeft: number = 0;
  public orthoRight: number = 0;
  public orthoTop: number = 0;
  public orthoBottom: number = 0;
  
  constructor(public name: string, position: MockVector3, public scene: any) {
    this.position = position;
  }
  
  setTarget(target: MockVector3) {
    // Mock set target
  }
}

export class MockHemisphericLight {
  public intensity: number = 1.0;
  
  constructor(public name: string, public direction: MockVector3, public scene: any) {}
}

export const MockMeshBuilder = {
  CreatePlane: jest.fn((name: string, options: any, scene: any) => {
    return new MockMesh(name, scene);
  })
};

// Export all mocks with the same names as Babylon.js exports
export const Engine = MockEngine;
export const Scene = MockScene;
export const FreeCamera = MockFreeCamera;
export const HemisphericLight = MockHemisphericLight;
export const MeshBuilder = MockMeshBuilder;
export const StandardMaterial = MockStandardMaterial;
export const DynamicTexture = MockDynamicTexture;
export const Color3 = MockColor3;
export const Vector3 = MockVector3;
export const Mesh = MockMesh;