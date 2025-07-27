const { app, BrowserWindow, ipcMain } = require("electron");
const path = require("path");
const { spawn } = require("child_process");

let mainWindow;
let haskellProcess;

const createWindow = () => {
  mainWindow = new BrowserWindow({
    width: 1400,
    height: 900,
    webPreferences: {
      preload: path.join(__dirname, "preload.js"),
      contextIsolation: true,
      enableRemoteModule: false,
      nodeIntegration: false,
    },
  });

  mainWindow.loadFile("index.html");

  if (process.argv.includes("--dev")) {
    mainWindow.webContents.openDevTools();
  }
};

// Handle simulation requests via WebSocket (server is already running)
ipcMain.handle("run-simulation", async (event, params) => {
  console.log("IPC handler called with params:", params);
  return new Promise((resolve, reject) => {
    const {
      pendulumLength,
      dampingCoeff,
      initialAngle,
      initialVelocity,
      timeEnd,
    } = params;

    // Connect to the running WebSocket server
    const WebSocket = require("ws");
    const ws = new WebSocket("ws://127.0.0.1:9162");

    ws.on("open", () => {
      console.log("Connected to Aivika Pendulum WebSocket server");

      // Send simulation request
      const simulateMessage = {
        msgType: "simulate",
        content: JSON.stringify({
          pendulumLength: pendulumLength,
          dampingCoeff: dampingCoeff,
          initialAngle: (initialAngle * Math.PI) / 180, // Convert degrees to radians
          initialVelocity: initialVelocity,
          timeEnd: timeEnd,
        }),
      };

      ws.send(JSON.stringify(simulateMessage));
    });

    ws.on("message", (data) => {
      try {
        const response = JSON.parse(data.toString());
        console.log("Received from server:", response);

        if (response.type === "simulation_result") {
          resolve(response.results);
          ws.close();
        } else if (response.type === "error") {
          reject(new Error(response.content));
          ws.close();
        }
      } catch (e) {
        console.error("Error parsing server response:", e);
        reject(new Error("Invalid server response"));
        ws.close();
      }
    });

    ws.on("error", (error) => {
      console.error("WebSocket error:", error);
      reject(new Error(`WebSocket error: ${error.message}`));
    });

    ws.on("close", () => {
      console.log("WebSocket connection closed");
    });

    // Timeout for the entire operation
    setTimeout(() => {
      if (ws.readyState === WebSocket.OPEN) {
        ws.close();
      }
      reject(new Error("Simulation timeout"));
    }, 30000);
  });
});

app.whenReady().then(() => {
  createWindow();

  app.on("activate", () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on("window-all-closed", () => {
  if (haskellProcess) {
    haskellProcess.kill();
  }
  if (process.platform !== "darwin") app.quit();
});

app.on("before-quit", () => {
  if (haskellProcess) {
    haskellProcess.kill();
  }
});
