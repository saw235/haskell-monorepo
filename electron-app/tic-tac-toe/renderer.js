// Game state
let gameState = {
    board: [[' ', ' ', ' '], [' ', ' ', ' '], [' ', ' ', ' ']],
    currentPlayer: 'X',
    gameOver: false,
    winner: null,
    validMoves: []
};

// DOM elements
let gameBoard, currentPlayerEl, gameStatusEl, newGameBtn, resetBtn, connectionStatusEl;

// Wait for the DOM to be fully loaded
document.addEventListener('DOMContentLoaded', () => {
    initializeElements();
    setupEventListeners();
    displayAppInfo();
    initializeGame();
    startConnectionCheck();
});

function initializeElements() {
    gameBoard = document.getElementById('game-board');
    currentPlayerEl = document.getElementById('current-player');
    gameStatusEl = document.getElementById('game-status');
    newGameBtn = document.getElementById('new-game-btn');
    resetBtn = document.getElementById('reset-btn');
    connectionStatusEl = document.getElementById('connection-status');
}

function setupEventListeners() {
    newGameBtn.addEventListener('click', startNewGame);
    resetBtn.addEventListener('click', resetGame);
}

function displayAppInfo() {
    if (window.gameAPI) {
        document.getElementById('electron-version').textContent = window.gameAPI.getAppVersion();
        document.getElementById('platform').textContent = window.gameAPI.getPlatform();
    }
}

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

async function resetGame() {
    await startNewGame();
}

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

// Keyboard shortcuts
document.addEventListener('keydown', (event) => {
    if (event.key === 'n' || event.key === 'N') {
        startNewGame();
    } else if (event.key === 'r' || event.key === 'R') {
        resetGame();
    }
}); 