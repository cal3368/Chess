module DrawBoard
  ( Color (..),
    Type (..),
    Square,
    -- Piece,
    Location,
    Board,
    new,
    newHelper,
    drawHorizontal,
    drawVertical,
    draw,
    chessPiece,
  )
where

import Data.Map.Strict qualified as Map
import System.IO

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

data Square = Piece Color Type | Empty
  deriving (Eq, Read, Show)

-- data Piece = Color :@: Type
--   deriving (Eq, Read, Show)

type Location = (Char, Int)

data Board where
  Board :: (Map.Map (Int, Int) Square) -> Board
  deriving (Eq, Read, Show)

new :: Board
new = Board (Map.fromList (newHelper 0 0))

createPiecesRow :: Color -> [((Int, Int), Square)]
createPiecesRow White =
  [ ((0, 0), Piece White Rook),
    ((0, 1), Piece White Knight),
    ((0, 2), Piece White Bishop),
    ((0, 3), Piece White Queen),
    ((0, 4), Piece White King),
    ((0, 5), Piece White Bishop),
    ((0, 6), Piece White Knight),
    ((0, 7), Piece White Rook)
  ]
createPiecesRow Black =
  [ ((7, 0), Piece Black Rook),
    ((7, 1), Piece Black Knight),
    ((7, 2), Piece Black Bishop),
    ((7, 3), Piece Black Queen),
    ((7, 4), Piece Black King),
    ((7, 5), Piece Black Bishop),
    ((7, 6), Piece Black Knight),
    ((7, 7), Piece Black Rook)
  ]

createWhitePawns :: Int -> [((Int, Int), Square)]
createWhitePawns 7 = [((1, 7), Piece White Pawn)]
createWhitePawns col = ((1, col), Piece White Pawn) : createWhitePawns (succ col)

createBlackPawns :: Int -> [((Int, Int), Square)]
createBlackPawns 7 = [((1, 7), Piece Black Pawn)]
createBlackPawns col = ((6, col), Piece Black Pawn) : createBlackPawns (succ col)

newHelper :: Int -> Int -> [((Int, Int), Square)]
newHelper curm curn
  | curm == 0 = createPiecesRow White ++ newHelper (curm + 1) 0
  | curm == 1 = createWhitePawns 0 ++ newHelper (curm + 1) 0
  | curm == 6 = createBlackPawns 0 ++ newHelper (curm + 1) 0
  | curm + 1 == 8 = createPiecesRow Black
  | curn + 1 == 8 = ((curm, curn), Empty) : newHelper (curm + 1) 0
  | otherwise = ((curm, curn), Empty) : newHelper curm (curn + 1)

drawHorizontal :: Int -> String
drawHorizontal 0 = "-\n"
drawHorizontal n = "-+" ++ drawHorizontal (n - 1)

draw :: Int -> Board -> String
draw cur_row (Board coor)
  | cur_row == 1 = drawVertical cur_row 7 (Map.toList coor) ++ "A B C D E F G H"
  | otherwise = drawVertical cur_row 7 (Map.toList coor) ++ drawHorizontal 7 ++ draw (pred cur_row) (Board (Map.drop 8 coor))

chessPiece :: Square -> String
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
chessPiece _ = " "

test = new

drawVertical :: Int -> Int -> [((Int, Int), Square)] -> String
drawVertical _ 0 (x : xs) = chessPiece (snd x) ++ "\n"
drawVertical cur_row num (x : xs)
  | num == 7 = show cur_row ++ " " ++ chessPiece (snd x) ++ "|" ++ drawVertical cur_row (num - 1) xs
  | otherwise = chessPiece (snd x) ++ "|" ++ drawVertical cur_row (num - 1) xs

main :: IO ()
main = do
  hSetEncoding stdout utf8
  putStrLn (chessPiece (Piece White Pawn))