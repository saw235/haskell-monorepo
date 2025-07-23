// TypeScript version of renderer.js for Babylon.js scene
import { Engine, Scene, ArcRotateCamera, HemisphericLight, DirectionalLight, MeshBuilder, StandardMaterial, PBRMaterial, Color3, Vector3 } from '@babylonjs/core';

window.addEventListener('DOMContentLoaded', () => {
    const canvas = document.getElementById('renderCanvas') as HTMLCanvasElement;
    const engine = new Engine(canvas, true);
    const scene = new Scene(engine);

    // Camera
    const camera = new ArcRotateCamera('camera', Math.PI / 4, Math.PI / 3, 12, new Vector3(0, 1, 0), scene);
    camera.attachControl(canvas, true);
    camera.wheelDeltaPercentage = 0.01;

    // Lighting
    const hemiLight = new HemisphericLight('hemi', new Vector3(0, 1, 0), scene);
    hemiLight.intensity = 0.7;
    const dirLight = new DirectionalLight('dir', new Vector3(-1, -2, -1), scene);
    dirLight.position = new Vector3(10, 10, 10);
    dirLight.intensity = 0.6;

    // Materials
    const standardMat = new StandardMaterial('standardMat', scene);
    standardMat.diffuseColor = new Color3(0.2, 0.6, 0.9);
    standardMat.specularColor = new Color3(0.8, 0.8, 0.8);
    const pbrMat = new PBRMaterial('pbrMat', scene);
    pbrMat.albedoColor = new Color3(0.9, 0.7, 0.2);
    pbrMat.metallic = 0.7;
    pbrMat.roughness = 0.3;

    // Geometry
    const box = MeshBuilder.CreateBox('box', { size: 2 }, scene);
    box.position = new Vector3(0, 1, 0); // Center the cube
    box.material = standardMat;
    camera.setTarget(box.position); // Camera looks at the cube
    const sphere = MeshBuilder.CreateSphere('sphere', { diameter: 2 }, scene);
    sphere.position = new Vector3(2.5, 1, 0);
    sphere.material = pbrMat;
    const ground = MeshBuilder.CreateGround('ground', { width: 12, height: 12 }, scene);
    const groundMat = new StandardMaterial('groundMat', scene);
    groundMat.diffuseColor = new Color3(0.3, 0.8, 0.3);
    ground.material = groundMat;

    // Animation state
    let rotationEnabled = true;
    let usePBR = true;
    let bouncePhase = 0;

    // Overlay UI
    const fpsEl = document.getElementById('fps')!;
    const cameraInfoEl = document.getElementById('camera-info')!;
    (document.getElementById('toggle-rotation') as HTMLButtonElement).onclick = () => { rotationEnabled = !rotationEnabled; };
    (document.getElementById('switch-material') as HTMLButtonElement).onclick = () => {
        usePBR = !usePBR;
        box.material = usePBR ? pbrMat : standardMat;
        sphere.material = usePBR ? standardMat : pbrMat;
    };

    // Render loop
    engine.runRenderLoop(() => {
        if (rotationEnabled) {
            box.rotation.y += 0.025;
        }
        // Bouncing sphere
        bouncePhase += 0.04;
        sphere.position.y = 1 + Math.abs(Math.sin(bouncePhase)) * 1.2;
        // Overlay updates
        fpsEl.textContent = `FPS: ${engine.getFps().toFixed(1)}`;
        cameraInfoEl.textContent = `Camera: alpha=${camera.alpha.toFixed(2)}, beta=${camera.beta.toFixed(2)}, radius=${camera.radius.toFixed(2)}`;
        scene.render();
    });

    // Resize
    window.addEventListener('resize', () => engine.resize());
}); 