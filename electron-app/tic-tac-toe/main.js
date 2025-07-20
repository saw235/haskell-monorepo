const { app, BrowserWindow, ipcMain } = require('electron');
const path = require('path');
const fetch = require('node-fetch');

// Keep a global reference of the window object
let mainWindow;

// Haskell server configuration
const SERVER_PORT = 8081;
const SERVER_URL = `http://localhost:${SERVER_PORT}`;

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

// IPC handlers for game communication
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