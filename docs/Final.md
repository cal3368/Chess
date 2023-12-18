# Chess

Team members:

- Jordan Chin
- Badaruddin Shaikh
- Charlie Leyens

## Summary Description

We are going to create a chess game using concepts learned throughout the semester. This includes IO interaction between two users to display the chess board and pieces. We also want to implement input validation for valid moves and a coordinate grid using Data.Map.Struct to represent each point on the board from a-h and 1-8. We will symbolically represent each type of piece in its current location on the board. Chess pieces have to be moved and removed correctly and when a king piece is checked, the player is warned and must move the king out of check or another piece to protect the king. When checkmate is reached the players will be notified of who won and the game will end. We need to define a situation for a draw occurring. All the traditional chess rules will be followed. If time permits we may add support for timed games and a stalemate situation.

## Project Execution Summary

- The first implementation to consider is deciding how to represent the chess pieces and the chess board. 
- Using the data types created, we considered the conditions of moving a piece from one square to another. This involved going through various rules for example, if a piece is blocking the path as well as how each chess type moves according to chess rules.
- Once we have implemented a check for piece movement, the hardest implementation was determining if a board was in Check or Checkmate. This involved going through each piece on the board and checking if the king is under attack or if the player has no valid moves to save the king. 
- Throughout each of the stages, we modify the `play` function to include chess movement and checks to ensure our functions work correctly. 
- Castling was implemented after the check and checkmate conditions and we had to modify some existing functions like `checkLegal` to allow a king to move two spaces and the rook and king to be at correct positions. 
- Once we have the chess game working, we looked at implementing a stretch goal of creating a timer with the board.

## Data Types

There are a couple of data types that are considered when implementing Chess. First, we have to represent each Chess piece and we decided on using Enums to list out the different colors and chess types as they are constant values. This is shown in our data types `Color` and `Type`.

```
data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Knight | Bishop | Queen | King | Rook
  deriving (Eq, Enum, Bounded, Read, Show)
```

Next, we create a Piece data type that combines Color and Type into a constructor with an additional boolean attribute to determine if a piece has moved. The boolean attribute is used when a player wants to Castle and checks if the King or Rook has moved.

```
data Piece = Piece Color Type Bool
  deriving (Eq)
```

To represent each square in the chess board, we use type called Location which is a tuple of Character and Int. To represent the chess board, we use `Data.Strict.Map` where the key is a Location and the value is a chess Piece.

```
type Location = (Char, Int)

type Board = Map.Map Location Piece
```

To display each chess piece on the board, we use unicode characters in terminal UI.

```
instance Show Piece where
  show (Piece White Pawn _) = "♙"
  show (Piece White Rook _) = "♖"
  show (Piece White Knight _) = "♘"
  show (Piece White Bishop _) = "♗"
  show (Piece White Queen _) = "♕"
  show (Piece White King _) = "♔"
  show (Piece Black Pawn _) = "♟"
  show (Piece Black Rook _) = "♜"
  show (Piece Black Knight _) = "♞"
  show (Piece Black Bishop _) = "♝"
  show (Piece Black Queen _) = "♛"
  show (Piece Black King _) = "♚"
```

## Maneuvering Chess pieces

To move chess pieces on a board, there are multiple conditions that need to be passed. 

- First, we need to check if the piece we are moving is the same color as the current player. The function `validateOwner` checks this condition. In the implementation, we check if the color attribute of `Piece` matches with the current player.
- The next condition to check is if the piece is in bounds of the chess board which can be done by checking if the letter is in between `a` and `h` and the number is between 1 and 8. 
- In addition, we need to check if there are any pieces in between the square we are moving from and the square we are moving to which is checked by `checkCastlingAdjSq`. 
- The last condition is checking if the move puts the player in check because if the move is made, the player loses.

### Chess Piece movement

For each chess type, different chess rules have to be followed when moving around the chess board. For example bishops can only move diagonally and rooks move vertically and horiozontally. The function `checkLegal` handles the condition when a piece wants to move from one square to another and utilizes pattern matching to match each chess type with the squares they can move to. A rook pattern matching is shown below to illustrate horizontal and vertical movement.

```
checkLegal (c1, i1) (c2, i2) (Piece _ Rook _) board
  | c1 == c2 && checkBetweenCol (c1, i1) (c2, i2) board = True
  | i1 == i2 && checkBetweenRow (c1, i1) (c2, i2) board = True
  | otherwise = False
```

However, there are cases where a piece like a King or Pawn can move outside of it's conventional movement or move only in one direction. For pawns, they are not allowed to move backwards so we only allow white pawns to move up a row and black pawns to move down a row. In addition, if a pawn has not moved, they are allowed to move two squares forward which can be checked by looking at a pawn's original location. The last move a pawn can make is moving diagonally to take a piece. The last pattern match shows that if an opposing players' piece is present diagonal to the pawn, we can make the move and capture it.

```
checkLegal (c1, i1) (c2, i2) (Piece White Pawn _) board
  | i1 == 2 && c1 == c2 && (i2 - i1 == 2 || i2 - i1 == 1) && not (checkBetweenCol (c1, i1) (c2, i2) board) = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | i1 == 2 && c1 == c2 && (i2 - i1 == 2 || i2 - i1 == 1) = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | c1 == c2 && i2 - i1 == 1 = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | abs (c2i - c1i) == 1 && (i2 - i1) == 1 = case Map.lookup (c2, i2) board of
      Just (Piece Black _ _) -> True
      _ -> False
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
```

In the case of a King, we usually move one square around the King but castling allows the King to move two spaces to the left and right.

## Castling

Castling is the only move in chess that allows 2 pieces to move in a turn and there are 4 conditions that need to be met before we can castle. First, we need to check that the King and Rook has not moved yet.

Initially, we had a data type `Piece` as shown below where Color is White or Black and Type is the type of chess piece it is.

```
data Piece = Piece Color Type
  deriving (Eq)
```

However, with the condition of checking if a piece has moved, we decided to add an extra boolean attribute to each Piece to indicate if it is possible to castle. Using this boolean, we check if both the King and Rook is True which indicates that neither of the pieces have moved.

```
data Piece = Piece Color Type Boolean
  deriving (Eq)
```

The next condition to check is if there are pieces in between the Rook and the King. The function `checkBetweenRows` checks all the squares between the Rook and King and will return True if there are no pieces present. The third condition is checking if the King is in check by using the `isCheck` function. Finally, the fourth condition is checking if the squares the king passes through castling has the possibility of being taken. This means that if the two squares on the left and right of the king could be taken by the opposing player's pieces, we cannot castle. To check for this condition, we iterate through each of the opposing players' pieces and if one of them can land on that square, we return False.

### Design Choices
A design choice that was made was determining if the King or Rook has moved. The initial idea was to introduce an additional parameter in our play function which will act as a counter. We would increment the counter whenever a rook or king moves, and if the counter exceeds 2, we allow the player to castle. However, there are a few issues with this implementation. For instance, we lack information about which piece will be moved in the subsequent iterations of the play function which leads to a possibility of a Rook moving back and forth, leading to continuous increments in the counter. 

A possible improvement in implementation is to split the King and Rook into seperate data types so only the boolean attributes are used for them. Currently, each chess piece has a boolean attribute but only the King and Rook would use the boolean values.

```
data Piece = Piece Color Type | PieceKingRook Color Type Boolean
  deriving (Eq)
```

## Check

After a player makes a legal move, the new state of the board is checked to see if the opponent is in check. It does this by determining if the player who made the move has any pieces that can make a legal move and reach the opponents king. If this is the case then the game also checks if it is checkmate which is explained below. If it is not checkmate than the opponent is alerted it is in check and the game continues. As with any time a player makes a move, the move they make must result in them not being in check.

## Checkmate

At the end of every move, we check if the board is not in checkmate after the move is made. We iterate through all the pieces on the board and if the piece is the opposing players' color, we move to the next piece. If it's the same color, we check if the piece can move the player out of check through any of it's legal moves. 

```
isNotCheckMate :: Color -> Board -> Board -> Bool
isNotCheckMate White b1 b2 = case Map.toList b2 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == Black
      then isNotCheckMate White b1 (Map.fromList rest)
      else any (checkMove White key value b1) allLocations || isNotCheckMate White b1 (Map.fromList rest)
```

## Additional Details

## Additional Haskell Libraries Required

- containers >= 0.6.7 && < 0.7
- time
- ansi-terminal >= 0.11 && < 0.12
- HUnit

## Code Structure
We utilized the model-view-controller structure for our project. The view component is represented by our DrawBoard module. This module contains the code responsible for displaying the current game state on a board in the terminal. The model component is contained in out Chess module and is responsible for the functional aspects of the game of chess being implemented in our program. This includes everything that goes with making moves and changing the board approproately. The Controller module is responsible to the user interaction and contains the IO interactions that allow the game to run and display in the terminal. We decided to use this structure because it broke the program into its necessary components and provided a good framework.

## Code Example

```
-- Move a piece from one location to another
makeMove :: Location -> Location -> Piece -> Board -> Board
makeMove ('e', 1) ('g', 1) (Piece White King True) b = castle White b "Right"
makeMove ('e', 1) ('c', 1) (Piece White King True) b = castle White b "Left"
makeMove ('e', 8) ('g', 8) (Piece Black King True) b = castle Black b "Right"
makeMove ('e', 8) ('c', 8) (Piece Black King True) b = castle Black b "Left"
makeMove l1@(_, 7) l2@(_, 8) (Piece White Pawn _) b = promotePawn White l1 l2 b
makeMove l1@(_, 2) l2@(_, 1) (Piece Black Pawn _) b = promotePawn Black l1 l2 b
makeMove l1 l2 p b = mod_board
  where
    insertion = Map.insert l2 p b
    mod_board = Map.delete l1 insertion
```

```
We are going to create a chess game using concepts learned throughout the semester. This includes IO interaction between two users to display the chess board and pieces. We also want to implement input validation for valid moves and a coordinate grid using Data.Map.Struct to represent each point on the board from a-h and 1-8. We will symbolically represent each type of piece in its current location on the board. Chess pieces have to be moved and removed correctly and when a king piece is checked, the player is warned and must move the king out of check or another piece to protect the king. When checkmate is reached the players will be notified of who won and the game will end. We need to define a situation for a draw occurring. All the traditional chess rules will be followed. If time permits we may add support for timed games and a stalemate situation.

```

- Choose (at least) one code excerpt that is a particularly good example of
  Haskell features, idioms, and/or style and describe it.

## Challenges

- One difficulty we faced was to create the isCheck and isCheckmate functions using Haskell at first because the most obvious approaching is iterating over all the pieces on the board. This is more complicated with Haskell. Ultimately we were able to solve the probelm by passing two list representations of our board to the function, one that removes pieces as we access the first item in the list and one that remains the same so we can check the conditions of check and checkmate.
- Another difficulty we faced was not having immutable data types to store attributes such as whether the kings and rooks have moved for castling. To get around this issue we added a boolean at the end of each Piece and made it True for all the Pieces even though they did not need any boolean for castling.
- We had trouble finding a way to refine our play/playWithTimer functions so they could be one function and also so that we did not need to have a version for each player making their move.
- Were any parts of the code particularly difficult to expres using Haskell?
  What are the challenges in refining and/or refactoring this code to be a
  better example of idiomatic Haskell?
- Adding dependencies in general was a difficult task to get around, we primarily looked at the cabal files for the assignments to understand how things actually work and how is cabal file used. We initally started by directly adding the changes to `.cabal` file only to find out that stack basically rebuilds the .cabal file from `package.yaml`. We also had problem installing `HUnit` since the official page for HUnit has no instructions on how to download the packages unlike how npm has the detailed documentation how to go about this process.

## Attempted and Abandoned Approaches

- We attempted to utilize a `TUI` library called Brick for displaying and playing the game. It would have made it easier to display the board because it would not have continuously displayed the next board in the terminal. Instead, it would have updated the screen each time a move was made. It would have made the timer easier to display, as well. We abandoned this because we were having trouble implementing the idea with Brick. We decided to spend our time implementing the timer in our current method because we were not certain we could accomplish the TUI in time. We wanted to add as many features as we could with the rest of our time.

- We also attempted to provide a save game option to our implementation. We spent time researching ways to accomplish this, but unfortunately ran out of time to accomplish this stretch goal.

Review the final project grading rubric and discuss any relevant aspects of the
project.

## Testing.

- We have provided an extensive suite of test functions. Since Chess has a number of rules on how different pieces should behave and different in different situation and each piece can have a different way of moving around the board.
- Multiple test have been written for the functions to cater to the different rules that different pieces are bound to.
- We use `HUnit` testing which is similar to the `JUnit` testing in Java to test the individual functions.
