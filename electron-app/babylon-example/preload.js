const { contextBridge, ipcRenderer } = require("electron");

// Expose protected methods that allow the renderer process to use
// the ipcRenderer without exposing the entire object
contextBridge.exposeInMainWorld("electronAPI", {
  // Example: Send a message to the main process
  sendMessage: (message) => ipcRenderer.send("message", message),

  // Example: Receive a message from the main process
  onMessage: (callback) => ipcRenderer.on("message", callback),

  // Example: Get app version
  getAppVersion: () => process.versions.electron,

  // Example: Get platform info
  getPlatform: () => process.platform,

  // Example: Get Node.js version
  getNodeVersion: () => process.versions.node,
});
