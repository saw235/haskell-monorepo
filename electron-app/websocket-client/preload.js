const { contextBridge } = require("electron");

contextBridge.exposeInMainWorld("electronAPI", {
  // Expose any needed APIs here
  log: (message) => console.log(message),
});
