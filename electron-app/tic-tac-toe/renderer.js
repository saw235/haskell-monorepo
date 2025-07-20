/**
 * @fileoverview Tic-Tac-Toe Electron Renderer Process
 * @description Renderer process for the Tic-Tac-Toe Electron application.
 * Handles the user interface, game state management, and communication with the main process.
 * 
 * This module manages the game UI, handles user interactions, and coordinates
 * with the main process to communicate with the Haskell backend server.
 * 
 * Architecture:
 * - Manages game state and UI updates
 * - Handles user interactions (clicks, keyboard shortcuts)
 * - Communicates with main process via IPC
 * - Provides visual feedback and animations
 * - Monitors connection status to backend
 * 
 * @author Tic-Tac-Toe Development Team
 * @version 1.0.0
 * @license MIT
 */

// Game state object containing all current game information
let gameState = {
    board: [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']], // 3x3 game board
    currentPlayer: 'X', // Current player (X or O)
    gameOver: false, // Whether the game has ended
    winner: null, // Winner if game is over (X, O, or null for tie)
    validMoves: [] // List of valid move positions
};

// DOM element references for efficient updates
let gameBoard, currentPlayerEl, gameStatusEl, newGameBtn, resetBtn, connectionStatusEl;

/**
 * Initializes the application when the DOM is fully loaded.
 * Sets up event listeners, initializes the game, and starts connection monitoring.
 */
document.addEventListener('DOMContentLoaded', () => {
    initializeElements();
    setupEventListeners();
    displayAppInfo();
    initializeGame();
    startConnectionCheck();
});

/**
 * Initializes references to DOM elements for efficient updates.
 * Caches element references to avoid repeated DOM queries.
 */
function initializeElements() {
    gameBoard = document.getElementById('game-board');
    currentPlayerEl = document.getElementById('current-player');
    gameStatusEl = document.getElementById('game-status');
    newGameBtn = document.getElementById('new-game-btn');
    resetBtn = document.getElementById('reset-btn');
    connectionStatusEl = document.getElementById('connection-status');
}

/**
 * Sets up event listeners for user interactions.
 * Binds click handlers to buttons and keyboard shortcuts.
 */
function setupEventListeners() {
    newGameBtn.addEventListener('click', startNewGame);
    resetBtn.addEventListener('click', resetGame);
}

/**
 * Displays application information including Electron version and platform.
 * Uses the gameAPI exposed through the preload script.
 */
function displayAppInfo() {
    if (window.gameAPI) {
        document.getElementById('electron-version').textContent = window.gameAPI.getAppVersion();
        document.getElementById('platform').textContent = window.gameAPI.getPlatform();
    }
}

/**
 * Initializes the game by starting a new game and setting up the initial state.
 * Handles connection errors and updates the UI accordingly.
 * 
 * @returns {Promise<void>}
 */
async function initializeGame() {
    try {
        const result = await window.gameAPI.newGame();
        if (result.success) {
            updateGameState(result.data);
            renderBoard();
            updateConnectionStatus(true);
        } else {
            console.error('Failed to initialize game:', result.error);
            updateConnectionStatus(false);
        }
    } catch (error) {
        console.error('Error initializing game:', error);
        updateConnectionStatus(false);
    }
}

/**
 * Starts periodic connection checking to monitor backend server status.
 * Checks connection every 5 seconds and updates the UI accordingly.
 */
function startConnectionCheck() {
    // Check connection every 5 seconds
    setInterval(async () => {
        try {
            const result = await window.gameAPI.getState();
            updateConnectionStatus(result.success);
        } catch (error) {
            updateConnectionStatus(false);
        }
    }, 5000);
}

/**
 * Updates the connection status indicator in the UI.
 * Shows visual feedback about the connection to the Haskell backend.
 * 
 * @param {boolean} connected - Whether the connection is active
 */
function updateConnectionStatus(connected) {
    const statusDot = connectionStatusEl.querySelector('.status-dot');
    const statusText = connectionStatusEl.querySelector('.status-text');
    
    if (connected) {
        statusDot.classList.add('connected');
        statusText.textContent = 'Connected to Haskell backend';
    } else {
        statusDot.classList.remove('connected');
        statusText.textContent = 'Disconnected from backend';
    }
}

/**
 * Starts a new game by requesting a fresh game state from the backend.
 * Updates the UI and shows success/error messages to the user.
 * 
 * @returns {Promise<void>}
 */
async function startNewGame() {
    try {
        const result = await window.gameAPI.newGame();
        if (result.success) {
            updateGameState(result.data);
            renderBoard();
            showMessage('New game started!', 'success');
        } else {
            showMessage('Failed to start new game: ' + result.error, 'error');
        }
    } catch (error) {
        showMessage('Error starting new game: ' + error.message, 'error');
    }
}

/**
 * Resets the game by starting a new game.
 * Alias for startNewGame for better semantic clarity.
 * 
 * @returns {Promise<void>}
 */
async function resetGame() {
    await startNewGame();
}

/**
 * Updates the local game state with data from the backend.
 * Triggers UI updates to reflect the new state.
 * 
 * @param {Object} data - Game state data from the backend
 */
function updateGameState(data) {
    gameState = {
        board: data.board,
        currentPlayer: data.currentPlayer,
        gameOver: data.gameOver,
        winner: data.winner,
        validMoves: data.validMoves || []
    };
    
    updateUI();
}

/**
 * Updates the user interface to reflect the current game state.
 * Updates player display, game status, and board appearance.
 */
function updateUI() {
    // Update current player display
    currentPlayerEl.textContent = gameState.currentPlayer;
    currentPlayerEl.className = gameState.currentPlayer === 'X' ? 'player-x' : 'player-o';
    
    // Update game status
    if (gameState.gameOver) {
        if (gameState.winner) {
            gameStatusEl.textContent = `Player ${gameState.winner} wins!`;
            showWinnerAnnouncement(gameState.winner);
            gameBoard.classList.add('game-over');
            gameBoard.classList.remove('tie');
        } else {
            gameStatusEl.textContent = "It's a tie!";
            showTieAnnouncement();
            gameBoard.classList.add('game-over', 'tie');
        }
    } else {
        gameStatusEl.textContent = 'Game in progress';
        gameBoard.classList.remove('game-over', 'tie');
    }
}

/**
 * Renders the game board in the DOM.
 * Creates clickable cells for each position and applies appropriate styling.
 * Handles winning cell highlighting and disabled state for filled cells.
 */
function renderBoard() {
    gameBoard.innerHTML = '';
    
    for (let row = 0; row < 3; row++) {
        for (let col = 0; col < 3; col++) {
            const cell = document.createElement('button');
            cell.className = 'cell';
            cell.dataset.row = row;
            cell.dataset.col = col;
            
            const cellValue = gameState.board[row][col];
            if (cellValue !== ' ') {
                cell.textContent = cellValue;
                cell.classList.add(cellValue.toLowerCase());
                cell.classList.add('disabled');
            }
            
            // Check if this is a winning cell
            if (gameState.gameOver && gameState.winner && cellValue === gameState.winner) {
                cell.classList.add('winning');
            }
            
            cell.addEventListener('click', () => handleCellClick(row, col));
            gameBoard.appendChild(cell);
        }
    }
}

/**
 * Handles clicks on game board cells.
 * Validates the move and sends it to the backend if valid.
 * Shows error messages for invalid moves.
 * 
 * @param {number} row - Row index (0-2)
 * @param {number} col - Column index (0-2)
 * @returns {Promise<void>}
 */
async function handleCellClick(row, col) {
    // Don't allow moves if game is over or cell is already filled
    if (gameState.gameOver || gameState.board[row][col] !== ' ') {
        return;
    }
    
    try {
        const result = await window.gameAPI.makeMove([row, col]);
        if (result.success) {
            updateGameState(result.data);
            renderBoard();
        } else {
            showMessage('Invalid move: ' + result.data.message, 'error');
        }
    } catch (error) {
        showMessage('Error making move: ' + error.message, 'error');
    }
}

/**
 * Shows a winner announcement overlay when a player wins.
 * Displays a celebratory message with the winner's name.
 * Auto-removes after 5 seconds or when manually dismissed.
 * 
 * @param {string} winner - The winning player (X or O)
 */
function showWinnerAnnouncement(winner) {
    // Remove existing announcement
    const existing = document.querySelector('.winner-announcement');
    if (existing) {
        existing.remove();
    }
    
    const announcement = document.createElement('div');
    announcement.className = 'winner-announcement';
    announcement.innerHTML = `
        <h2>🎉 Congratulations! 🎉</h2>
        <p>Player <strong>${winner}</strong> wins!</p>
        <button class="btn btn-primary" onclick="this.parentElement.remove()">Continue</button>
    `;
    
    document.body.appendChild(announcement);
    
    // Auto-remove after 5 seconds
    setTimeout(() => {
        if (announcement.parentElement) {
            announcement.remove();
        }
    }, 5000);
}

/**
 * Shows a tie announcement overlay when the game ends in a draw.
 * Displays a message indicating the tie and encouraging another game.
 * Auto-removes after 6 seconds or when manually dismissed.
 */
function showTieAnnouncement() {
    // Remove existing announcements
    const existingWinner = document.querySelector('.winner-announcement');
    const existingTie = document.querySelector('.tie-announcement');
    if (existingWinner) existingWinner.remove();
    if (existingTie) existingTie.remove();
    
    const announcement = document.createElement('div');
    announcement.className = 'tie-announcement';
    announcement.innerHTML = `
        <h2>🤝 Too bad... it's a draw! 🤝</h2>
        <p>Neither player could claim victory this time.</p>
        <p class="tie-message">Better luck next time!</p>
        <button class="btn btn-secondary" onclick="this.parentElement.remove()">Try Again</button>
    `;
    
    document.body.appendChild(announcement);
    
    // Auto-remove after 6 seconds
    setTimeout(() => {
        if (announcement.parentElement) {
            announcement.remove();
        }
    }, 6000);
}

/**
 * Shows a temporary toast notification message.
 * Creates a styled notification that appears in the top-right corner.
 * Auto-removes after 3 seconds.
 * 
 * @param {string} message - The message to display
 * @param {string} type - The type of message ('error', 'success', or 'info')
 */
function showMessage(message, type = 'info') {
    // Create a simple toast notification
    const toast = document.createElement('div');
    toast.style.cssText = `
        position: fixed;
        top: 20px;
        right: 20px;
        background: ${type === 'error' ? '#e74c3c' : type === 'success' ? '#2ecc71' : '#3498db'};
        color: white;
        padding: 1rem;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
        z-index: 1000;
        max-width: 300px;
        word-wrap: break-word;
    `;
    toast.textContent = message;
    
    document.body.appendChild(toast);
    
    // Remove after 3 seconds
    setTimeout(() => {
        if (toast.parentElement) {
            toast.remove();
        }
    }, 3000);
}

/**
 * Sets up keyboard shortcuts for game actions.
 * - 'N' or 'n': Start new game
 * - 'R' or 'r': Reset game
 */
document.addEventListener('keydown', (event) => {
    if (event.key === 'n' || event.key === 'N') {
        startNewGame();
    } else if (event.key === 'r' || event.key === 'R') {
        resetGame();
    }
}); 