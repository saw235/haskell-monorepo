/**
 * @fileoverview Tic-Tac-Toe Electron Preload Script
 * @description Preload script that safely exposes APIs to the renderer process.
 * Provides a secure bridge between the renderer process and the main process.
 * 
 * This script runs in a privileged context and exposes only the necessary
 * functions to the renderer process, preventing direct access to Node.js APIs
 * and ensuring security through context isolation.
 * 
 * Security Features:
 * - Context isolation enabled
 * - Limited API exposure
 * - No direct Node.js access from renderer
 * - Controlled IPC communication
 * 
 * @author Tic-Tac-Toe Development Team
 * @version 1.0.0
 * @license MIT
 */

const { contextBridge, ipcRenderer } = require('electron');

/**
 * Exposes protected methods that allow the renderer process to use
 * the ipcRenderer without exposing the entire object.
 * 
 * The gameAPI object provides:
 * - Game action methods (newGame, makeMove, getState)
 * - Utility methods (getAppVersion, getPlatform, getNodeVersion)
 * 
 * All methods are asynchronous and return promises that resolve with
 * the result from the main process.
 */
contextBridge.exposeInMainWorld('gameAPI', {
  /**
   * Starts a new game by requesting a fresh game state from the backend.
   * 
   * @returns {Promise<Object>} Promise that resolves to game state or error
   */
  newGame: () => ipcRenderer.invoke('game-new-game'),
  
  /**
   * Makes a move at the specified position.
   * 
   * @param {Array<number>} position - [row, col] coordinates for the move
   * @returns {Promise<Object>} Promise that resolves to updated game state or error
   */
  makeMove: (position) => ipcRenderer.invoke('game-make-move', position),
  
  /**
   * Gets the current game state without making any changes.
   * 
   * @returns {Promise<Object>} Promise that resolves to current game state or error
   */
  getState: () => ipcRenderer.invoke('game-get-state'),
  
  /**
   * Gets the Electron version information.
   * 
   * @returns {string} The Electron version string
   */
  getAppVersion: () => process.versions.electron,
  
  /**
   * Gets the current platform (win32, darwin, linux, etc.).
   * 
   * @returns {string} The platform identifier
   */
  getPlatform: () => process.platform,
  
  /**
   * Gets the Node.js version information.
   * 
   * @returns {string} The Node.js version string
   */
  getNodeVersion: () => process.versions.node
}); 