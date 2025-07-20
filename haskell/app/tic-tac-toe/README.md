# Tic-Tac-Toe Game

A simple command-line tic-tac-toe game implemented in Haskell.

## How to Play

1. **Build the game:**
   ```bash
   bazel build //haskell/app/tic-tac-toe:tic-tac-toe
   ```

2. **Run the game:**
   ```bash
   bazel run //haskell/app/tic-tac-toe:tic-tac-toe
   ```

3. **Game Rules:**
   - Players take turns placing X and O on the 3x3 board
   - Enter moves as "row column" (e.g., "2 3" for row 2, column 3)
   - First player to get 3 in a row (horizontally, vertically, or diagonally) wins
   - If the board fills up without a winner, it's a tie

## Game Features

- **Interactive gameplay** with clear board display
- **Input validation** to prevent invalid moves
- **Win detection** for all possible winning combinations
- **Tie detection** when the board is full
- **User-friendly interface** with numbered rows and columns

## Example Game Session

```
Welcome to Tic-Tac-Toe!
Players take turns placing X and O on the board.
The first player to get 3 in a row (horizontally, vertically, or diagonally) wins!

    1   2   3
  +---+---+---+
1 |   |   |   |
2 |   |   |   |
3 |   |   |   |
  +---+---+---+

Player X's turn
Enter row (1-3) and column (1-3) separated by space (e.g., '2 3'):
> 1 1

    1   2   3
  +---+---+---+
1 | X |   |   |
2 |   |   |   |
3 |   |   |   |
  +---+---+---+
```

## Implementation Details

The game is built using:
- **Pure Haskell** with no external dependencies beyond base
- **Functional programming** principles with immutable game state
- **Pattern matching** for game logic
- **Maybe types** for safe input parsing and move validation
- **List comprehensions** for board operations and win detection

## Files

- `Main.hs` - Complete game implementation
- `BUILD.bazel` - Bazel build configuration
- `README.md` - This documentation 