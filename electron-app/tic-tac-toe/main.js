/**
 * @fileoverview Tic-Tac-Toe Electron Main Process
 * @description Main process for the Tic-Tac-Toe Electron application.
 * Handles window creation, IPC communication, and HTTP requests to the Haskell backend.
 * 
 * This module serves as the bridge between the Electron renderer process and the
 * Haskell HTTP server, providing a clean API for game operations.
 * 
 * Architecture:
 * - Creates and manages the main application window
 * - Handles IPC communication with renderer process
 * - Makes HTTP requests to Haskell backend server
 * - Manages application lifecycle events
 * 
 * @author Tic-Tac-Toe Development Team
 * @version 1.0.0
 * @license MIT
 */

const { app, BrowserWindow, ipcMain } = require('electron');
const path = require('path');
const fetch = require('node-fetch');

// Keep a global reference of the window object
let mainWindow;

// Haskell server configuration
const SERVER_PORT = 8081;
const SERVER_URL = `http://localhost:${SERVER_PORT}`;

/**
 * Creates the main application window with appropriate settings.
 * 
 * The window is configured with:
 * - Fixed size (600x700) for consistent UI
 * - Context isolation enabled for security
 * - Preload script for safe IPC communication
 * - Custom application icon
 * - Non-resizable for consistent layout
 * 
 * @returns {void}
 */
function createWindow() {
  // Create the browser window
  mainWindow = new BrowserWindow({
    width: 600,
    height: 700,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, 'preload.js')
    },
    icon: path.join(__dirname, 'icon.png'),
    title: 'Tic-Tac-Toe',
    resizable: false
  });

  // Load the index.html file
  mainWindow.loadFile('index.html');

  // Open DevTools in development mode
  if (process.argv.includes('--dev')) {
    mainWindow.webContents.openDevTools();
  }

  // Emitted when the window is closed
  mainWindow.on('closed', () => {
    mainWindow = null;
  });
}

// This method will be called when Electron has finished initialization
app.whenReady().then(createWindow);

/**
 * IPC handler for starting a new game.
 * 
 * Makes a POST request to the Haskell server to initialize a new game state.
 * Returns the new game state to the renderer process.
 * 
 * @returns {Promise<Object>} Object containing success status and game data or error
 */
ipcMain.handle('game-new-game', async () => {
  try {
    const response = await fetch(`${SERVER_URL}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        action: 'new_game'
      })
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    return { success: true, data };
  } catch (error) {
    console.error('Error starting new game:', error);
    return { success: false, error: error.message };
  }
});

/**
 * IPC handler for making a move in the game.
 * 
 * Makes a POST request to the Haskell server with the specified position.
 * The position should be an array [row, col] with 0-based indices.
 * 
 * @param {Event} event - IPC event object
 * @param {Array<number>} position - [row, col] coordinates for the move
 * @returns {Promise<Object>} Object containing success status and updated game data or error
 */
ipcMain.handle('game-make-move', async (event, position) => {
  try {
    const response = await fetch(`${SERVER_URL}`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        action: 'make_move',
        position: position
      })
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    return { success: true, data };
  } catch (error) {
    console.error('Error making move:', error);
    return { success: false, error: error.message };
  }
});

/**
 * IPC handler for getting the current game state.
 * 
 * Makes a GET request to the Haskell server to retrieve the current
 * game state without making any changes.
 * 
 * @returns {Promise<Object>} Object containing success status and current game data or error
 */
ipcMain.handle('game-get-state', async () => {
  try {
    const response = await fetch(`${SERVER_URL}`, {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      }
    });
    
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    
    const data = await response.json();
    return { success: true, data };
  } catch (error) {
    console.error('Error getting game state:', error);
    return { success: false, error: error.message };
  }
});

// Quit when all windows are closed
app.on('window-all-closed', () => {
  // Always quit when all windows are closed to ensure proper cleanup
  app.quit();
});

app.on('activate', () => {
  // On macOS it's common to re-create a window when the dock icon is clicked
  if (mainWindow === null) {
    createWindow();
  }
}); 