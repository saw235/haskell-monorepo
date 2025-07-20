// Wait for the DOM to be fully loaded
document.addEventListener('DOMContentLoaded', () => {
    // Get DOM elements
    const electronVersionEl = document.getElementById('electron-version');
    const platformEl = document.getElementById('platform');
    const nodeVersionEl = document.getElementById('node-version');
    const messageBtn = document.getElementById('message-btn');
    const messageLog = document.getElementById('message-log');

    // Display app information
    if (window.electronAPI) {
        electronVersionEl.textContent = window.electronAPI.getAppVersion();
        platformEl.textContent = window.electronAPI.getPlatform();
        nodeVersionEl.textContent = window.electronAPI.getNodeVersion();
    } else {
        electronVersionEl.textContent = 'Not available';
        platformEl.textContent = 'Not available';
        nodeVersionEl.textContent = 'Not available';
    }

    // Handle message button click
    messageBtn.addEventListener('click', () => {
        const timestamp = new Date().toLocaleTimeString();
        const message = `Hello from renderer process! (${timestamp})`;
        
        // Add message to log
        addMessageToLog(`📤 ${message}`);
        
        // Send message to main process
        if (window.electronAPI) {
            window.electronAPI.sendMessage(message);
        }
    });

    // Listen for messages from main process
    if (window.electronAPI) {
        window.electronAPI.onMessage((event, message) => {
            const timestamp = new Date().toLocaleTimeString();
            addMessageToLog(`📥 Main process: ${message} (${timestamp})`);
        });
    }

    // Helper function to add messages to the log
    function addMessageToLog(message) {
        const messageElement = document.createElement('div');
        messageElement.textContent = message;
        messageElement.style.marginBottom = '8px';
        messageElement.style.padding = '4px 0';
        messageElement.style.borderBottom = '1px solid #e2e8f0';
        
        messageLog.appendChild(messageElement);
        
        // Auto-scroll to bottom
        messageLog.scrollTop = messageLog.scrollHeight;
    }

    // Add some initial content to show the app is working
    setTimeout(() => {
        addMessageToLog('🚀 Electron Hello World app is ready!');
    }, 500);
}); 