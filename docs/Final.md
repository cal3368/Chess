# Chess

Team members:

- Jordan Chin
- Badaruddin Shaikh
- Charlie Leyens

## Summary Description

Reiterate the summary description of the overall goal of the project (updated as
necessary from the Proposal and/or Checkpoint documents).

## Project Execution Summary

## Data Types

There are a couple of data types that have to be considered when implementing Chess. First, we have to represent each Chess piece and we decided to use Enums to list out the different colors and chess types as they are constant values. This is shown in our data types `Color` and `Type`.

```
data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Knight | Bishop | Queen | King | Rook
  deriving (Eq, Enum, Bounded, Read, Show)
```

Next, we create a Piece data type that combines the two data types into a constructor with an additional boolean attribute to determine if a piece has moved. The boolean attribute is used when a player wants to Castle and checks if the King or Rook has moved.

```
data Piece = Piece Color Type Bool
  deriving (Eq)
```

To represent each square in the chess board, we use type called Location which is a tuple of Character and Int. To represent the chess board, we use `Data.Strict.Map` where the key is a Location and the value is a chess Piece. 

```
type Location = (Char, Int)

type Board = Map.Map Location Piece
```

To represent each chess piece on the board, we use unicode characters in terminal UI.

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

## Castling

Castling is the only move in chess that alloows 2 pieces to move in a turn and there are 4 conditions that need to be met before we can castle. First, we need to check that the King and Rook has not moved yet. 

Initially, we had a data type `Piece` as shown below where Color is White or Black and Type is the type of chess piece it is.

```
data Piece = Piece Color Type
  deriving (Eq)
```

However, with the condition of checking if a piece has moved, we decided to add an extra boolean attribute to each Piece to indicate if it has moved. 
```
data Piece = Piece Color Type Boolean
  deriving (Eq)
```

The next condition checked is if there are pieces in between the Rook and the King. The function `checkBetweenRows` is used if the squares king-side or queen-side are occupied and will return True if there are no pieces present. The third condition is checking if the King is in check by using the `isCheck` function. Finally, the fourth condition is checking if the squares the king passes through castling has the possibility of being taken. This means that if the two squares on the left and right of the king could be taken by the opposing player's pieces, we cannot castle. To check for this condition, we iterate through each of the opposing players' pieces and if one of them can land on that square, we return False.

Some design choices that were made on castling was how to determine if the King or Rook has moved. The initial idea was to introduce an additional parameter in our play function, acting as a counter. We would increment the counter whenever a rook or king moves, and if the counter exceeds 2, we can then consider castling on that side. However, there are a few issues with this implementation. For instance, we lack information about which piece will be moved in the subsequent iterations of the play function. Additionally, a piece has the potential to move back and forth, leading to continuous increments in the counter. 

A possible improvement in implementation is to split the King and Rook into seperate data types so only the boolean attributes are used for them. Currently, each chess piece has a boolean attribute but only the King and Rook would use the boolean values so the data type can be split as shown below.

```
data Piece = Piece Color Type | PieceKingRook Color Type Boolean
  deriving (Eq)
```

## Additional Details

## Additional Haskell Libraries Required
  - containers >= 0.6.7 && < 0.7
  - time
  - ansi-terminal >= 0.11 && < 0.12
  - HUnit

- Briefly describe the structure of the code (what are the main components, the
  module dependency structure). Why was the project modularized in this way?
- Choose (at least) one code excerpt that is a particularly good example of
  Haskell features, idioms, and/or style and describe it.
- Were any parts of the code particularly difficult to expres using Haskell?
  What are the challenges in refining and/or refactoring this code to be a
  better example of idiomatic Haskell?

## Attempted and Abandoned Approaches
- We attempted to utilize a TUI library called Brick for displaying and playing the game. It would have made it easier to display the board because it would not have continuously displayed the next board in the terminal. Instead, it would have updated the screen each time a move was made. It would have made the timer easier to display, as well. We abandoned this because we were having trouble implementing the idea with Brick. We decided to spend our time implementing the timer in our current method because we were not certain we could accomplish the TUI in time. We wanted to add as many features as we could with the rest of our time.

- We also attempted to provide a save game option to our implementation. We spent time researching ways to accomplish this, but unfortunately ran out of time to accomplish this stretch goal.

Review the final project grading rubric and discuss any relevant aspects of the
project.
