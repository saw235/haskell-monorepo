let socket = null;
let isConnected = false;

// Virtual position for keyboard demo
let virtualPosition = { x: 0, y: 0 };
let keyboardEnabled = false;

const statusElement = document.getElementById("status");
const connectBtn = document.getElementById("connect-btn");
const disconnectBtn = document.getElementById("disconnect-btn");
const pingBtn = document.getElementById("ping-btn");
const echoBtn = document.getElementById("echo-btn");
const calcBtn = document.getElementById("calc-btn");
const sendBtn = document.getElementById("send-btn");
const clearBtn = document.getElementById("clear-btn");
const messagesDiv = document.getElementById("messages");
const calcInput = document.getElementById("calc-input");
const msgTypeInput = document.getElementById("msg-type");
const msgContentInput = document.getElementById("msg-content");
const keyboardArea = document.getElementById("keyboard-area");
const keyboardStatus = document.getElementById("keyboard-status");
const positionDisplay = document.getElementById("position-display");
const keyboardEnabledCheckbox = document.getElementById("keyboard-enabled");

function addMessage(content, type = "received") {
  const messageDiv = document.createElement("div");
  messageDiv.className = `message ${type}`;

  const timestamp = document.createElement("div");
  timestamp.className = "timestamp";
  timestamp.textContent = new Date().toLocaleTimeString();

  const messageContent = document.createElement("div");
  messageContent.className = "message-content";
  messageContent.textContent = content;

  messageDiv.appendChild(timestamp);
  messageDiv.appendChild(messageContent);
  messagesDiv.appendChild(messageDiv);
  messagesDiv.scrollTop = messagesDiv.scrollHeight;
}

function updateConnectionStatus(connected) {
  isConnected = connected;
  statusElement.textContent = connected ? "Connected" : "Disconnected";
  statusElement.className = connected ? "connected" : "";

  connectBtn.disabled = connected;
  disconnectBtn.disabled = !connected;
  pingBtn.disabled = !connected;
  echoBtn.disabled = !connected;
  calcBtn.disabled = !connected;
  sendBtn.disabled = !connected;
  calcInput.disabled = !connected;
  msgTypeInput.disabled = !connected;
  msgContentInput.disabled = !connected;
  keyboardEnabledCheckbox.disabled = !connected;

  if (!connected) {
    keyboardEnabled = false;
    keyboardEnabledCheckbox.checked = false;
    updateKeyboardStatus();
  }
}

function connectWebSocket() {
  try {
    socket = new WebSocket("ws://127.0.0.1:9160");

    socket.onopen = function (event) {
      addMessage("Connected to Haskell WebSocket server");
      updateConnectionStatus(true);
    };

    socket.onmessage = function (event) {
      try {
        const data = JSON.parse(event.data);
        addMessage(`[${data.type}] ${data.content}`);
      } catch (e) {
        addMessage(`Raw message: ${event.data}`);
      }
    };

    socket.onclose = function (event) {
      addMessage("Disconnected from server", "error");
      updateConnectionStatus(false);
    };

    socket.onerror = function (error) {
      addMessage(`WebSocket error: ${error}`, "error");
      updateConnectionStatus(false);
    };
  } catch (error) {
    addMessage(`Connection error: ${error}`, "error");
    updateConnectionStatus(false);
  }
}

function disconnectWebSocket() {
  if (socket) {
    socket.close();
    socket = null;
  }
  updateConnectionStatus(false);
}

function sendMessage(type, content) {
  if (socket && isConnected) {
    const message = JSON.stringify({ msgType: type, content: content });
    socket.send(message);
    addMessage(`[SENT] ${type}: ${content}`, "sent");
  }
}

// Event listeners
connectBtn.addEventListener("click", connectWebSocket);
disconnectBtn.addEventListener("click", disconnectWebSocket);

pingBtn.addEventListener("click", () => {
  sendMessage("ping", "ping");
});

echoBtn.addEventListener("click", () => {
  sendMessage("echo", "Hello from Electron!");
});

calcBtn.addEventListener("click", () => {
  const expression = calcInput.value.trim();
  if (expression) {
    sendMessage("calculate", expression);
    calcInput.value = "";
  }
});

sendBtn.addEventListener("click", () => {
  const type = msgTypeInput.value.trim();
  const content = msgContentInput.value.trim();
  if (type && content) {
    sendMessage(type, content);
    msgTypeInput.value = "";
    msgContentInput.value = "";
  }
});

clearBtn.addEventListener("click", () => {
  messagesDiv.innerHTML = "";
});

// Handle Enter key for inputs
calcInput.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    calcBtn.click();
  }
});

msgContentInput.addEventListener("keypress", (e) => {
  if (e.key === "Enter") {
    sendBtn.click();
  }
});

// Keyboard functionality
function updateKeyboardStatus() {
  if (keyboardEnabled) {
    keyboardStatus.textContent = "Keyboard input ACTIVE - Use arrow keys!";
    keyboardArea.classList.add("active");
    positionDisplay.textContent = `Position: (${virtualPosition.x}, ${virtualPosition.y})`;
  } else {
    keyboardStatus.textContent =
      "Click here and enable checkbox to use arrow keys...";
    keyboardArea.classList.remove("active");
    positionDisplay.textContent = "Position: (0, 0)";
  }
}

function sendKeyboardInput(key, direction) {
  if (socket && isConnected && keyboardEnabled) {
    const message = JSON.stringify({
      msgType: "keyboard",
      content: JSON.stringify({
        key: key,
        direction: direction,
        position: virtualPosition,
      }),
    });
    socket.send(message);
    addMessage(
      `[SENT] keyboard: ${key} (${direction}) at (${virtualPosition.x}, ${virtualPosition.y})`,
      "sent",
    );
  }
}

function handleKeyPress(event) {
  if (!keyboardEnabled || !isConnected) return;

  let key = "";
  let direction = "";

  switch (event.key) {
    case "ArrowUp":
      key = "up";
      direction = "north";
      virtualPosition.y += 1;
      break;
    case "ArrowDown":
      key = "down";
      direction = "south";
      virtualPosition.y -= 1;
      break;
    case "ArrowLeft":
      key = "left";
      direction = "west";
      virtualPosition.x -= 1;
      break;
    case "ArrowRight":
      key = "right";
      direction = "east";
      virtualPosition.x += 1;
      break;
    default:
      return; // Ignore other keys
  }

  event.preventDefault();
  updateKeyboardStatus();
  sendKeyboardInput(key, direction);
}

// Keyboard event listeners
keyboardArea.addEventListener("click", () => {
  keyboardArea.focus();
});

keyboardArea.addEventListener("keydown", handleKeyPress);

keyboardEnabledCheckbox.addEventListener("change", (e) => {
  keyboardEnabled = e.target.checked;
  if (keyboardEnabled) {
    virtualPosition = { x: 0, y: 0 }; // Reset position
    keyboardArea.focus();
  }
  updateKeyboardStatus();
});

// Initialize
updateConnectionStatus(false);
updateKeyboardStatus();
addMessage("WebSocket client ready. Click Connect to start.", "received");
