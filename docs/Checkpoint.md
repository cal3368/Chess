# Chess

Team members:

- Jordan Chin
- Badaruddin Shaikh
- Charlie Leyens

## Summary Description

We are going to create a chess game using concepts learned throughout the semester. This includes IO interaction between two users to display the chess board and pieces. We also want to implement input validation for valid moves and a coordinate grid using Data.Map.Struct to represent each point on the board from a-h and 1-8. We will symbolically represent each type of piece in its current location on the board. Chess pieces have to be moved and removed correctly and when a king piece is checked, the player is warned and must move the king out of check or another piece to protect the king. When checkmate is reached the players will be notified of who won and the game will end. We need to define a situation for a draw occurring. All the traditional chess rules will be followed. If time permits we may add support for timed games and a stalemate situation.

## Checkpoint Progress Summary

Give a summary of the progress made and lessons learned thus far.

### IO Functionality

We were able to get a working IO function that takes user input for names, and moves. The input for moves is checked for validity in terms of being in the proper form and being within the proper range. There is an input option for quit that ends the game. This option will be more of a resigning option eventually. We will also implement an offer draw input that offers a draw to your opponent and the opponent can respond yes or no. The main IO function switches back and forth between each person’s turn currently but does not have functionality beyond that. We have created further functions for checking the validity of each move but have not implemented them into the game functioning yet and moving/removing pieces. To see the visual implementation of our board one can run ghci Chess and then run the main function.

### Testing

We wrote tests to manually test out the pure functions extensively, which upon further research we realized wasn't the best idea, we plan to switch to HUnit testing we did try doing that trying to re-initialize the project with `stack new` (since we needed to have a cabal file and add the HUnit dependency) but that was quite last minute and we realized that we started running into different problems and we decided to push the restructuring to after the checkpoint 1.
As for now we have testing that tests the pure functions, while we couldn’t test out the impure functions. Since we couldn’t find the way to check what's getting printed on the terminal. We explored the idea of converting all our impure functions to pure functions so they return their results and we can compare to the expected result. Although we could update the signature of all the IO() functions to also return the data that is getting returned from the function. But that did not look like the most efficient way to do it. We have been reading about testing in detail now and will most likely incline towards limiting the number of impure functions , code reuse and using HUnit testing and QuickCheck.

-Would it be possible to somehow change our input retrieval and check by using a parser?

-For the newBoard function, is there a better way to instantiate the pieces instead of using a list of every piece?

Note: Be sure that all `.hs` source files and any supporting files (e.g.,
`stack.yaml`, `package.yaml`/`<package_name>.cabal` files, data files, examples,
...) have been committed and pushed.

### Data types

We made different data types to represent the chess pieces and the board. The data types are listed below where we have enums for the color and chess pieces. To represent a square, the cell can either be empty or occupied by a Piece data type that takes in a Color and Type. This makes it easy to pattern match in the helper functions we create later on.

```
data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)
```

```
data Type = Pawn | Rook | Knight | Bishop | Queen | King
deriving (Eq, Enum, Bounded, Read, Show)
```

```
data Square = Square Piece | Empty
deriving (Eq)
```

```
data Piece = Piece Color Type
deriving (Eq)
```

### Board Data structure

The chess pieces on the board are represented with unicode characters for example a king is displayed as "♔". In addition, the board is represented with a `Map.map` from the `Data.Map.Strict` library and each cell is a Piece data type where it is empty or occupied. The board locations span from (1,1) to (8,8) instead of (0,0) to (7,7) for easier implementation and the users will input the cells from 1-8.

### Chess functions

We have implemented various functions to check the move validation made by the user. For example, we checked if the user moving the pieces owns that piece as well as checking if the move will go out of bounds. Furthermore, we created another function that checks the behavior of a chess piece type aligns with the move it is trying to make. For example, bishops can only move diagonally and pawns can only move forward from their side of the board.

## Additional Details

- List any additional Haskell libraries required for the project (i.e., what
  `extra-deps` have been added to `stack.yaml` files and `[build-depends]` have
  been added to `package.yaml`/`<package_name>.cabal` files).
  - Data.Map.Struct
  - Data.Char
  - Data.Maybe
  - System.IO
- Briefly describe the structure of the code (what are the main components, the
  module dependency structure).
  - Are structure does not currently follow our desired final result of following the MVC format. Currently we have one file for tests, one for the board, and one for the other functions. We will look to refine the structure as we progress.
- Pose any questions that you may have about your project and/or request
  feedback on specific aspects of the project.
