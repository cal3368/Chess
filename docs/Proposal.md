# Chess

Team members:

- Jordan Chin
- Badaruddin Shaik
- Chung-An Huang
- Charlie Leyens

## Summary Description

We are going to create a chess game using concepts learned throughout the semester. This includes IO interaction between two users to display the chess board and pieces. We also want to implement input validation for valid moves and a coordinate grid to represent each point on the board from a-h and 1-8. We will symbolically represent each type of piece in its current location on the board. Chess pieces have to be moved and removed correctly and when a king piece is checked, the player is warned and must move the king out of check. When checkmate is reached the players will be notified of who won and the game will end. We need to define a situation for a draw occurring. All the traditional chess rules will be followed. If time permits we may add support for castling and timed games.

## Additional Details
At the beginning of the game, the first player can choose if they want to be white or black. At every turn, the chess board will appear and a prompt will appear such as 'It is white's turn. Please input a position'. The user will have to select a coordinate with a piece and another valid coordinate. If the destination is taken by an opposing piece, the opposing piece will be removed and the spot is taken. An example input is 'a5 a6' which moves a pawn from the position a5 to a6. If an invalid input like 's1 a5' is used, we send out a message that says 'Invalid move. Please try again' We may implement further help messages that tell the player the appropriate type of move for the pieces. After each move the current game board will be displayed. At the end, a message of "Checkmate! 'COLOR' Wins!" If we end up including an implementation of a timer, after each turn, it would display the time remaining for the player who just went and the time remaining for the player who's turn it is now. If the time reaches zero, the players will be alerted and the other player will win. For example "White has run out of time! Black Wins!". Further commands for the users will be to forfeit, agree to draw, and possibly save game.

## Key components
We will represent the game using the model-view-controller design. We can represent each chess piece as an algebraic data type where they can get the color, chess type and the coordinate it is currently in. In addition, we can represent the 8x8 chessboard as an array of arrays where we represent each coordinate as a Maybe data type. This woud fullfill a text view module which could possibly be replaced by a GUI view module if it seems doable. Each sqaure in the board can be represented by a data type that either contains a chess piece or is Empty `data Square = Piece | Empty`. The functionality of the game will be contained in the model-controller module. This module will contain functions related to moving the pieces, determining validity of moves, and determining a winner. We may also need a data type to represent whether the game is over or not. The view module will just visually represent the internal state of the game kept by the model-controller module.

Some important data structures:
Piece
Color
Square
Board

Some important functions:<br>
`checkCheckmate :: (Board) -> Bool` <br>
`isMoveValid :: move -> Piece -> Bool` <br>
`showBoard :: IO()` <br>
`move :: Piece -> Position -> Maybe Board`<br>
`removePiece :: Piece -> Board` <br>
`promotePiece :: Piece -> Board` <br>

## Testing
We can test the process of the movement of a chess piece as well as the display of the chess board. Input validation will also need to be tested in case it is out of bounds or invalid. It will be helpful to include as many tests as posible as we build up the game to ensure all possibilities are functioning. 

The following is a sample list of test cases we will develop to ensure the basic funtionality of the program. In addition, we will perform tests for the data structures used to store different chess pieces and their properties.

- TestCase (assertBool "Checkmate Test" (checkCheckmate b))
- TestCase (assertBool "Move Validity Test" (isMoveValid m p))
- TestCase (assertBool "Board validity test" (isBoardValid b))
- TestCase (asssertEqual "Move Test" nb (move b pc po b))
- TestCase (assertEqual "Remove Piece Test" nb (removePiece b p b))
- TestCase (assertEqual "Promote Piece Test" nb (promotePiece b p b))

## Stretch Goals
The minimal viable product should be completing a full game of chess between two users.

Some additional features that can be included is

- A timer for each player that counts down like in a normal chess game
- Flipping the board at every turn so that the user's pieces are at the bottom of the screen.
- Special moves like Castling
- Draw or stalement condition
- Save/resume game option
- GUI representation that functions instead of text input

## Functionality at Checkpoint
The expected functionality is listed below:

- The data structure for the chess board and pieces are created
- Input prompts and validations for moves
- Test cases for basic functions have been completed
