module DrawBoard (newBoard, drawBoard) where

import Chess
import Data.Char
import qualified Data.Map.Strict as Map

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

test :: Board
test =
  Map.fromList
    [ (('a', 1), Piece White Rook True),
      (('b', 1), Piece White Knight True),
      (('c', 1), Piece White Bishop True),
      (('f', 3), Piece White Queen True),
      (('g', 1), Piece White King False),
      (('f', 7), Piece White Bishop True),
      (('f', 1), Piece White Rook False),
      (('a', 8), Piece Black Rook True),
      (('e', 5), Piece Black Knight True),
      (('c', 8), Piece Black Bishop True),
      (('d', 8), Piece Black Queen True),
      (('f', 8), Piece Black King False),
      (('d', 4), Piece Black Bishop True),
      (('f', 6), Piece Black Knight True),
      (('h', 8), Piece Black Rook True),
      (('a', 7), Piece Black Pawn True),
      (('b', 7), Piece Black Pawn True),
      (('c', 7), Piece Black Pawn True),
      (('d', 7), Piece Black Pawn True),
      (('g', 7), Piece Black Pawn True),
      (('h', 7), Piece Black Pawn True),
      (('a', 2), Piece White Pawn True),
      (('b', 2), Piece White Pawn True),
      (('c', 2), Piece White Pawn True),
      (('e', 4), Piece White Pawn True),
      (('f', 2), Piece White Pawn True),
      (('g', 2), Piece White Pawn True),
      (('h', 2), Piece Black Rook True)
    ]
