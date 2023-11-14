module DrawBoard
  ( Color (..),
    Type (..),
    Square (..),
    Piece (..),
    Location,
    Board,
    newBoard,
    drawBoard,
    -- new,
    -- newHelper,
    -- drawHorizontal,
    -- drawVertical,
    -- draw,
    -- chessPiece,
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

-- data Board where
--   Board :: (Map.Map (Int, Int) Square) -> Board
--   deriving (Eq, Show)

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

-- new :: Board
-- new = Board (Map.fromList (newHelper 0 0))

-- createPiecesRow :: Color -> [((Int, Int), Square)]
-- createPiecesRow White =
--   [ ((0, 0), Square (Piece White Rook)),
--     ((0, 1), Square (Piece White Knight)),
--     ((0, 2), Square (Piece White Bishop)),
--     ((0, 3), Square (Piece White Queen)),
--     ((0, 4), Square (Piece White King)),
--     ((0, 5), Square (Piece White Bishop)),
--     ((0, 6), Square (Piece White Knight)),
--     ((0, 7), Square (Piece White Rook))
--   ]
-- createPiecesRow Black =
--   [ ((7, 0), Square (Piece Black Rook)),
--     ((7, 1), Square (Piece Black Knight)),
--     ((7, 2), Square (Piece Black Bishop)),
--     ((7, 3), Square (Piece Black Queen)),
--     ((7, 4), Square (Piece Black King)),
--     ((7, 5), Square (Piece Black Bishop)),
--     ((7, 6), Square (Piece Black Knight)),
--     ((7, 7), Square (Piece Black Rook))
--   ]

-- createWhitePawns :: Int -> [((Int, Int), Square)]
-- createWhitePawns 7 = [((1, 7), Square (Piece White Pawn))]
-- createWhitePawns col = ((1, col), Square (Piece White Pawn)) : createWhitePawns (succ col)

-- createBlackPawns :: Int -> [((Int, Int), Square)]
-- createBlackPawns 7 = [((1, 7), Square (Piece Black Pawn))]
-- createBlackPawns col = ((6, col), Square (Piece Black Pawn)) : createBlackPawns (succ col)

-- newHelper :: Int -> Int -> [((Int, Int), Square)]
-- newHelper curm curn
--   | curm == 0 = createPiecesRow White ++ newHelper (curm + 1) 0
--   | curm == 1 = createWhitePawns 0 ++ newHelper (curm + 1) 0
--   | curm == 6 = createBlackPawns 0 ++ newHelper (curm + 1) 0
--   | curm + 1 == 8 = createPiecesRow Black
--   | curn + 1 == 8 = ((curm, curn), Empty) : newHelper (curm + 1) 0
--   | otherwise = ((curm, curn), Empty) : newHelper curm (curn + 1)

-- drawHorizontal :: Int -> String
-- drawHorizontal 0 = "-\n"
-- drawHorizontal n = "-+" ++ drawHorizontal (n - 1)

-- draw :: Int -> Board -> String
-- draw cur_row (Board coor)
--   | cur_row == 1 = drawVertical cur_row 7 (Map.toList coor) ++ "A B C D E F G H"
--   | otherwise = drawVertical cur_row 7 (Map.toList coor) ++ drawHorizontal 7 ++ draw (pred cur_row) (Board (Map.drop 8 coor))

-- chessPiece :: Square -> String
-- chessPiece Empty = " "
-- chessPiece (Piece White Pawn) = "\x2659"
-- chessPiece (Piece White Rook) = "\x2656"
-- chessPiece (Piece White Knight) = "\x2658"
-- chessPiece (Piece White Bishop) = "\x2657"
-- chessPiece (Piece White Queen) = "\x2655"
-- chessPiece (Piece White King) = "\x2654"
-- chessPiece (Piece Black Pawn) = "\x265F"
-- chessPiece (Piece Black Rook) = "\x265C"
-- chessPiece (Piece Black Knight) = "\x265E"
-- chessPiece (Piece Black Bishop) = "\x265D"
-- chessPiece (Piece Black Queen) = "\x265B"
-- chessPiece (Piece Black King) = "\x265A"
-- chessPiece _ = " "

getSquare :: Board -> Location -> Square
getSquare board (c, i) = Data.Maybe.fromMaybe Empty (Map.lookup (c, i) board)

-- test = new

-- drawVertical :: Int -> Int -> [((Int, Int), Square)] -> String
-- drawVertical _ 0 (x : xs) = chessPiece (snd x) ++ "\n"
-- drawVertical cur_row num (x : xs)
--   | num == 7 = show cur_row ++ " " ++ chessPiece (snd x) ++ "|" ++ drawVertical cur_row (num - 1) xs
--   | otherwise = chessPiece (snd x) ++ "|" ++ drawVertical cur_row (num - 1) xs

-- main :: IO ()
-- main = do
--   hSetEncoding stdout utf8
--   putStrLn (chessPiece (Piece White Pawn))

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
