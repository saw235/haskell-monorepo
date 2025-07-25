# Electron Babylon Example

A Babylon.js + Electron example application built with Bazel, demonstrating 3D graphics in a desktop application.

## Features

- Babylon.js 3D scene with basic geometry (cube, sphere, plane)
- Interactive camera controls (orbit, zoom, pan)
- Basic lighting (ambient + directional)
- Material system demonstration (standard, PBR)
- Simple animation (rotating objects)
- Electron integration with secure preload script
- Built with Bazel for reproducible builds

## Project Structure

```
electron-app/babylon-example/
├── main.js          # Main Electron process
├── preload.js       # Preload script for secure IPC
├── renderer.js      # Renderer process (Babylon.js scene)
├── index.html       # Main HTML file
├── styles.css       # Styling
├── package.json     # Node.js dependencies
├── BUILD.bazel      # Bazel build configuration
├── assets/          # 3D models, textures, sounds
│   ├── models/
│   ├── textures/
│   └── sounds/
└── README.md        # This file
```

## Quick Start

### Using Bazel:

```bash
# Install dependencies (if not already done)
bazel run -- @pnpm//:pnpm --dir $(pwd)/electron-app/babylon-example install

# Run the app in normal mode
bazel run //electron-app/babylon-example:electron-babylon-example

# Run the app in development mode (with DevTools)
bazel run //electron-app/babylon-example:electron-babylon-example-dev
```

### Using npm directly:

```bash
# Install dependencies
cd electron-app/babylon-example
pnpm install

# Start the app
pnpm start
```

## Babylon.js Scene Features

- Rotating cube with different materials
- Bouncing sphere
- Ground plane with texture
- Skybox for environment
- Interactive camera controls

## Development

- Hot reload for development
- TypeScript support (optional)
- Easy to extend with more Babylon.js features

## Building for Distribution

The app is configured with electron-builder for creating distributable packages:

```bash
cd electron-app/babylon-example
pnpm run build
```

This will create platform-specific packages in the `dist/` directory.

## Bazel Integration

- Dependencies managed through `aspect_rules_js`
- Build artifacts cached and reproducible
- Works seamlessly in the monorepo environment

### Available Bazel Targets

- `//electron-app/babylon-example:electron-babylon-example` - Run the app in normal mode
- `//electron-app/babylon-example:electron-babylon-example-dev` - Run the app in development mode (with DevTools)
- `//electron-app/babylon-example:electron-babylon-example-package` - Package for distribution

## Troubleshooting

If you encounter issues:

1. Make sure all dependencies are installed: `bazel run -- @pnpm//:pnpm --dir $(pwd)/electron-app/babylon-example install`
2. Check that the `.bazelignore` file includes `electron-app/babylon-example/node_modules`
3. Verify the MODULE.bazel file includes the electron-babylon-example npm workspace configuration

## Next Steps

This example can be extended with:

- More complex 3D scenes
- Interactive GUI controls
- Model loading (GLTF/GLB)
- Particle systems and post-processing
- Sound integration
- Testing frameworks
