const { contextBridge, ipcRenderer } = require('electron');

// Expose protected methods that allow the renderer process to use
// the ipcRenderer without exposing the entire object
contextBridge.exposeInMainWorld('gameAPI', {
  // Game actions
  newGame: () => ipcRenderer.invoke('game-new-game'),
  makeMove: (position) => ipcRenderer.invoke('game-make-move', position),
  getState: () => ipcRenderer.invoke('game-get-state'),
  
  // Utility methods
  getAppVersion: () => process.versions.electron,
  getPlatform: () => process.platform,
  getNodeVersion: () => process.versions.node
}); 