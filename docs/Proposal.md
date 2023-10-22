# Project Title

Team members:

- Jordan Chin
- Badaruddin Shaik
- Chung-An Huang
- Charlie Leyens

## Summary Description

A summary description of the overall goal of the project.

We are going to create a chess game using concepts learned throughout the semester. This includes IO interaction between two users to display the chess board and pieces. We also want to implement input validation for valid moves and a coordinate grid to represent each point on the board from a-h and 1-8. Chess pieces have to moved and removed correctly and when a king piece is checked, the player is warned and must move the king out of check. Castling is not added? 

## Additional Details

- One or more typical "use cases". These might include "storyboards" explaining
  how a user would interact with the program or some interesting "input/output"
  examples.

At the beginning of the game, the first player can choose if they want to be white or black. At every turn, the chess board will appear and a prompt will appear such as 'It is white's turn. Please input a position'. The user will have to select a coordinate with a piece and another coordinate valid coordinate. If the destination is taken by an opposing piece, he opposing piece will be removed and the spot is taken. An example input is 'a5 a6' which moves a pawn from the position a5 to a6. If an invalid input like 's1 a5' is used, we send out a message that says 'Invalid move. Please try again'

## Key components
- A sketch of intended components (key functions, key data structures, separate
  modules).  To satisfy the "multiple Haskell modules" requirement, it may
  suffice to separate an application into a "model-controller" module and a
  "view" module (e.g., a "text view" module that could be replaced by a "GUI
  view" module).

We can represent each chess piece as an algebraic data type where they can get the color, chess type and the coordinate it is currently in. In addition, we can represent the 8x8 chessboard as an array of arrays where we represent each coordinate as a Maybe data type `[[Maybe Chesspiece]]`. 

## Testing
- Thoughts on testing. These might include critical functions or data structures
  that will be given
  [`tasty`](https://hackage.haskell.org/package/tasty) tests.

We can test the process of the movement of a chess piece as well as the display of the chess board. Input validation will also need to be tested in case it is out of bounds or invalid. 

## Stretch Goals
- Thoughts on a "minimal viable product" and "stretch goals". Be sure to review
  the final project grading rubric and consider organizing the project around a
  core deliverable that will almost certainly be achieved and then a number of
  extensions and features that could be added to ensure that project is of
  suitable size/scope/effort.

The minimal viable product should be completing a full game of chess between two users. 

Some additional features that can be included is

- A timer for each player that counts down like in a normal chess game
- Flipping the board at every turn so that the user's pieces are at the bottom of the screen.
- Special moves like Castling
- Draw or stalement condition

## Functionality at Checkpoint
- Expected functionality to be completed at the Checkpoint.

The expected functionality is listed below

- The data structure for the chess board and pieces are created
- Input prompts and validations for moves