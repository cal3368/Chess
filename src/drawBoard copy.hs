module DrawBoard
  ( Color (..),
    Type (..),
    Piece (..),
    Location,
    Board,
    newBoard,
    drawBoard,
  )
where

import Brick (Padding (Max, Pad), Widget, hLimit, padLeft, padRight, simpleMain, str, vBox, vLimit, withBorderStyle)
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style qualified as BS
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Core
import Data.Char
import Data.Map.Strict qualified as Map
import Data.Maybe
import System.IO

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

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

type Location = (Char, Int)

type Board = Map.Map Location Piece

newBoard :: Board
newBoard =
  Map.fromList
    [ (('a', 1), Piece White Rook),
      (('b', 1), Piece White Knight),
      (('c', 1), Piece White Bishop),
      (('d', 1), Piece White Queen),
      (('e', 1), Piece White King),
      (('f', 1), Piece White Bishop),
      (('g', 1), Piece White Knight),
      (('h', 1), Piece White Rook),
      (('a', 2), Piece White Pawn),
      (('b', 2), Piece White Pawn),
      (('c', 2), Piece White Pawn),
      (('d', 2), Piece White Pawn),
      (('e', 2), Piece White Pawn),
      (('f', 2), Piece White Pawn),
      (('g', 2), Piece White Pawn),
      (('h', 2), Piece White Pawn),
      (('a', 8), Piece Black Rook),
      (('b', 8), Piece Black Knight),
      (('c', 8), Piece Black Bishop),
      (('d', 8), Piece Black Queen),
      (('e', 8), Piece Black King),
      (('f', 8), Piece Black Bishop),
      (('g', 8), Piece Black Knight),
      (('h', 8), Piece Black Rook),
      (('a', 7), Piece Black Pawn),
      (('b', 7), Piece Black Pawn),
      (('c', 7), Piece Black Pawn),
      (('d', 7), Piece Black Pawn),
      (('e', 7), Piece Black Pawn),
      (('f', 7), Piece Black Pawn),
      (('g', 7), Piece Black Pawn),
      (('h', 7), Piece Black Pawn)
    ]

getSquare :: Board -> Location -> Maybe Piece
getSquare board (c, i) = Map.lookup (c, i) board

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

    drawSquare :: Maybe Piece -> IO ()
    drawSquare Nothing = putStr ("  " ++ " |")
    drawSquare (Just piece) = putStr (" " ++ show piece ++ " |")

getRowSquare :: Int -> Int -> Board -> [Maybe Piece]
getRowSquare 0 _ _ = []
getRowSquare c n board = case Map.lookup (chr (96 + c), n) board of
  Nothing -> getRowSquare (pred c) n board ++ [Nothing]
  Just i -> getRowSquare (pred c) n board ++ [Just i]

-- -- Function to convert a ChessState to a string representation of the chess board
-- chessBoardToString :: Board -> String
-- chessBoardToString board =
--   unlines
--     [ "   a   b   c   d   e   f   g   h",
--       " +-------------------------------+",
--       stringRow 8,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 7,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 6,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 5,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 4,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 3,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 2,
--       " +---+---+---+---+---+---+---+---+",
--       stringRow 1,
--       " +-------------------------------+",
--       "   a   b   c   d   e   f   g   h"
--     ]
--   where
--     stringRow :: Int -> String
--     stringRow row = show row ++ "|" ++ concatMap squareString (getRowSquare 8 row board) ++ show row

--     squareString :: Maybe Piece -> String
--     squareString Nothing = "  " ++ " |"
--     squareString (Just piece) = " " ++ show piece ++ " |"

--     showRow :: Int -> String
--     showRow row =
--       show row
--         ++ "| "
--         ++ concatMap (\col -> showSquare (chr (col + ord 'a'), row)) [1 .. 8]

--     showSquare :: Location -> String
--     showSquare loc =
--       case getSquare newBoard loc of
--         Nothing -> ". "
--         Just piece -> show piece ++ " "
