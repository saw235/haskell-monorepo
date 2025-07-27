const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("electronAPI", {
  runSimulation: (params) => ipcRenderer.invoke("run-simulation", params),
});