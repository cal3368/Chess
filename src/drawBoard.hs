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

import Data.Char
import qualified Data.Map.Strict as Map

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Knight | Bishop | Queen | King | Rook
  deriving (Eq, Enum, Bounded, Read, Show)

data Piece = Piece Color Type Bool
  deriving (Eq)

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

type Location = (Char, Int)

type Board = Map.Map Location Piece

newBoard :: Board
newBoard =
  Map.fromList
    ( [ (('a', 1), Piece White Rook True),
        (('b', 1), Piece White Knight True),
        (('c', 1), Piece White Bishop True),
        (('d', 1), Piece White Queen True),
        (('e', 1), Piece White King True),
        (('f', 1), Piece White Bishop True),
        (('g', 1), Piece White Knight True),
        (('h', 1), Piece White Rook True),
        (('a', 8), Piece Black Rook True),
        (('b', 8), Piece Black Knight True),
        (('c', 8), Piece Black Bishop True),
        (('d', 8), Piece Black Queen True),
        (('e', 8), Piece Black King True),
        (('f', 8), Piece Black Bishop True),
        (('g', 8), Piece Black Knight True),
        (('h', 8), Piece Black Rook True)
      ]
        ++ whitePawns
        ++ blackPawns
    )

whitePawns :: [(Location, Piece)]
whitePawns = [((x, 2), Piece White Pawn True) | x <- ['a' .. 'h']]

blackPawns :: [(Location, Piece)]
blackPawns = [((x, 7), Piece Black Pawn True) | x <- ['a' .. 'h']]

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
