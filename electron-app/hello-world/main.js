const { app, BrowserWindow, ipcMain } = require('electron');
const path = require('path');

// Keep a global reference of the window object
let mainWindow;

function createWindow() {
  // Create the browser window
  mainWindow = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true,
      preload: path.join(__dirname, 'preload.js')
    },
    icon: null, // You can add an icon here later
    title: 'Electron Hello World'
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

// IPC handlers
ipcMain.on('message', (event, message) => {
  console.log('Received message from renderer:', message);
  
  // Send a response back to the renderer
  const response = `Main process received: "${message}" and processed it at ${new Date().toLocaleTimeString()}`;
  event.reply('message', response);
});

// Quit when all windows are closed
app.on('window-all-closed', () => {
  // On macOS it is common for applications to stay open even when all windows are closed
  if (process.platform !== 'darwin') {
    app.quit();
  }
});

app.on('activate', () => {
  // On macOS it's common to re-create a window when the dock icon is clicked
  if (mainWindow === null) {
    createWindow();
  }
}); 