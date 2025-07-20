# Electron Hello World

A simple Electron application built with Bazel, demonstrating the basic setup for desktop applications using Electron.

## Features

- Modern, responsive UI with gradient background
- Secure preload script for main-renderer communication
- Interactive demo with message passing between processes
- App information display (Electron version, platform, Node.js version)
- Built with Bazel for reproducible builds

## Project Structure

```
electron-app/
├── main.js          # Main Electron process
├── preload.js       # Preload script for secure IPC
├── renderer.js      # Renderer process logic
├── index.html       # Main HTML file
├── styles.css       # Styling
├── package.json     # Node.js dependencies
├── BUILD.bazel      # Bazel build configuration
├── start-electron.sh # Convenience script to run the app
└── README.md        # This file
```

## Quick Start

### Using the convenience script:
```bash
# Run in normal mode
./start-electron.sh

# Run in development mode (with DevTools)
./start-electron.sh --dev
```

### Using npm directly:
```bash
# Install dependencies (if not already done)
bazel run -- @pnpm//:pnpm --dir $(pwd)/electron-app install

# Start the app
cd electron-app
npx electron .
```

### Using Bazel:
```bash
# Build the app
bazel build //electron-app:electron-hello-world-files

# Run the app in normal mode
bazel run //electron-app:electron-hello-world

# Run the app in development mode (with DevTools)
bazel run //electron-app:electron-hello-world-dev

# The app files are available in bazel-bin/electron-app/
```

## Development

The app includes several interactive features:

1. **App Information Display**: Shows Electron version, platform, and Node.js version
2. **Message Passing Demo**: Click the button to send messages between main and renderer processes
3. **Real-time Logging**: See message timestamps and communication flow

## Security Features

- Context isolation enabled
- Node integration disabled
- Secure preload script for IPC communication
- No direct access to Node.js APIs from renderer

## Building for Distribution

The app is configured with electron-builder for creating distributable packages:

```bash
cd electron-app
npm run build
```

This will create platform-specific packages in the `dist/` directory.

## Bazel Integration

This app is fully integrated with Bazel:

- Dependencies managed through `aspect_rules_js`
- Build artifacts cached and reproducible
- Works seamlessly in the monorepo environment

### Available Bazel Targets

- `//electron-app:electron-hello-world-files` - Build target with all app files
- `//electron-app:electron-hello-world` - Run the app in normal mode
- `//electron-app:electron-hello-world-dev` - Run the app in development mode (with DevTools)
- `//electron-app:electron-hello-world-package` - Package for distribution

## Troubleshooting

If you encounter issues:

1. Make sure all dependencies are installed: `bazel run -- @pnpm//:pnpm --dir $(pwd)/electron-app install`
2. Check that the `.bazelignore` file includes `electron-app/node_modules`
3. Verify the MODULE.bazel file includes the electron-hello-world npm workspace configuration

## Next Steps

This is a basic setup that you can extend with:

- Additional UI components
- More complex IPC communication
- Integration with other parts of your Bazel monorepo
- Custom build configurations
- Testing frameworks 