module DrawBoard
  ( Color (..),
    Type (..),
    Square (..),
    Piece (..),
    Location,
    Board,
    newBoard,
    drawBoard,
  )
where

import Data.Char
import Data.Map.Strict qualified as Map
import Data.Maybe qualified
import System.IO

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

data Square = Square Piece | Empty
  deriving (Eq)

data Piece = Piece Color Type
  deriving (Eq)

instance Show Piece where
  show (Piece White Pawn) = "♙"
  show (Piece White Rook) = "♖"
  show (Piece White Knight) = "♘"
  show (Piece White Bishop) = "♗"
  show (Piece White Queen) = "♕"
  show (Piece White King) = "♔"
  show (Piece Black Pawn) = "♟"
  show (Piece Black Rook) = "♜"
  show (Piece Black Knight) = "♞"
  show (Piece Black Bishop) = "♝"
  show (Piece Black Queen) = "♛"
  show (Piece Black King) = "♚"

instance Show Square where
  show (Square piece) = show piece
  show Empty = " "

type Location = (Char, Int)

type Board = Map.Map Location Square

newBoard :: Board
newBoard =
  Map.fromList
    [ (('a', 1), Square (Piece White Rook)),
      (('b', 1), Square (Piece White Knight)),
      (('c', 1), Square (Piece White Bishop)),
      (('d', 1), Square (Piece White Queen)),
      (('e', 1), Square (Piece White King)),
      (('f', 1), Square (Piece White Bishop)),
      (('g', 1), Square (Piece White Knight)),
      (('h', 1), Square (Piece White Rook)),
      (('a', 2), Square (Piece White Pawn)),
      (('b', 2), Square (Piece White Pawn)),
      (('c', 2), Square (Piece White Pawn)),
      (('d', 2), Square (Piece White Pawn)),
      (('e', 2), Square (Piece White Pawn)),
      (('f', 2), Square (Piece White Pawn)),
      (('g', 2), Square (Piece White Pawn)),
      (('h', 2), Square (Piece White Pawn)),
      (('a', 8), Square (Piece Black Rook)),
      (('b', 8), Square (Piece Black Knight)),
      (('c', 8), Square (Piece Black Bishop)),
      (('d', 8), Square (Piece Black Queen)),
      (('e', 8), Square (Piece Black King)),
      (('f', 8), Square (Piece Black Bishop)),
      (('g', 8), Square (Piece Black Knight)),
      (('h', 8), Square (Piece Black Rook)),
      (('a', 7), Square (Piece Black Pawn)),
      (('b', 7), Square (Piece Black Pawn)),
      (('c', 7), Square (Piece Black Pawn)),
      (('d', 7), Square (Piece Black Pawn)),
      (('e', 7), Square (Piece Black Pawn)),
      (('f', 7), Square (Piece Black Pawn)),
      (('g', 7), Square (Piece Black Pawn)),
      (('h', 7), Square (Piece Black Pawn))
    ]

getSquare :: Board -> Location -> Square
getSquare board (c, i) = Data.Maybe.fromMaybe Empty (Map.lookup (c, i) board)

drawBoard :: Board -> IO ()
drawBoard board = do
  putStrLn "   a   b   c   d   e   f   g   h"
  putStrLn " +-------------------------------+"
  mapM_ drawRow [8, 7, 6, 5, 4, 3, 2, 1]
  putStrLn "   a   b   c   d   e   f   g   h"
  where
    drawRow :: Int -> IO ()
    drawRow row = do
      putStr (show row ++ "|")
      mapM_ drawSquare (getRowSquare 8 row board)
      putStr (show row)
      putStrLn ""
      if row == 1
        then putStrLn " +-------------------------------+"
        else putStrLn " +---+---+---+---+---+---+---+---+"

    drawSquare :: Square -> IO ()
    drawSquare square = putStr (" " ++ show square ++ " |")

getRowSquare :: Int -> Int -> Board -> [Square]
getRowSquare 0 _ _ = []
getRowSquare c n board = case Map.lookup (chr (96 + c), n) board of
  Nothing -> getRowSquare (pred c) n board ++ [Empty]
  Just i -> getRowSquare (pred c) n board ++ [i]
