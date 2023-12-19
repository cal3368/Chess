module Chess where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import GHC.Char (chr)

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Knight | Bishop | Queen | King | Rook
  deriving (Eq, Enum, Bounded, Read, Show)

data Piece = Piece Color Type Bool
  deriving (Eq)

type Location = (Char, Int)

type Board = Map.Map Location Piece

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

-- Lists all locations of the chess board from a1 to h8 as a list
allLocations :: [Location]
allLocations = [(char, num) | char <- ['a' .. 'h'], num <- [1 .. 8]]

-- Checks if the chess piece is the correct color
validateOwner :: Location -> Color -> Board -> Bool
validateOwner (c, i) color board = case Map.lookup (c, i) board of
  Nothing -> False
  Just p -> color == getColor p

-- Checks if the location is not out of bounds of the chess board
checkInbounds :: Location -> Bool
checkInbounds (c, i)
  | i <= 0 || i > 8 = False
  | (ord c - ord 'a') > 7 || (ord c - ord 'a') < 0 = False
  | otherwise = True

{-
If the destination of a piece lands on another piece, check
the other piece is a different color to take
-}
checkDestColor :: Location -> Color -> Board -> Bool
checkDestColor loc color b = case Map.lookup loc b of
  Nothing -> True
  Just p -> color /= getColor p

-- Checks if a move is valid from one location to another
checkMove :: Color -> Location -> Piece -> Board -> Location -> Bool
checkMove Black source piece b dest =
  validateOwner source Black b
    && checkInbounds dest
    && checkDestColor dest Black b
    && checkLegal source dest piece b
    && checkDirection source dest b
    && not (isCheck mod_board mod_board White)
  where
    insertion = Map.insert dest piece b
    mod_board = Map.delete source insertion
checkMove White source piece b dest =
  validateOwner source White b
    && checkInbounds dest
    && checkDestColor dest White b
    && checkLegal source dest piece b
    && checkDirection source dest b
    && not (isCheck mod_board mod_board Black)
  where
    insertion = Map.insert dest piece b
    mod_board = Map.delete source insertion

-- Checks if the rook and king have not moved
checkCastlingMoved :: Location -> Location -> Piece -> Board -> Bool
checkCastlingMoved (_, _) (_, _) (Piece _ _ False) _ = False
checkCastlingMoved (c1, _) (c2, _) (Piece White _ True) board =
  if ord c2 > ord c1
    then case Map.lookup ('h', 1) board of
      Nothing -> False
      Just (Piece _ tr br) -> tr == Rook && br
    else case Map.lookup ('a', 1) board of
      Nothing -> False
      Just (Piece _ tr br) -> tr == Rook && br
checkCastlingMoved (c1, _) (c2, _) (Piece Black _ True) board =
  if c2 > c1
    then case Map.lookup ('h', 8) board of
      Nothing -> False
      Just (Piece _ tr br) -> tr == Rook && br
    else case Map.lookup ('a', 8) board of
      Nothing -> False
      Just (Piece _ tr br) -> tr == Rook && br

-- Check if the adjascent squares of the king is under attack
checkCastlingCheck :: Location -> Location -> Color -> Board -> Board -> Bool
checkCastlingCheck (c1, i1) (c2, i2) White board1 board2 = case Map.toList board1 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == White
      then checkCastlingCheck (c1, i1) (c2, i2) White (Map.fromList rest) board2
      else not (any (checkMove Black key value board2) checkedSq) && checkCastlingCheck (c1, i1) (c2, i2) White (Map.fromList rest) board2
    where
      checkedSq = if c2 > c1 then [('f', 1), ('g', 1)] else [('d', 1), ('c', 1)]
checkCastlingCheck (c1, i1) (c2, i2) Black board1 board2 = case Map.toList board1 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == Black
      then checkCastlingCheck (c1, i1) (c2, i2) Black (Map.fromList rest) board2
      else not (any (checkMove White key value board2) checkedSq) && checkCastlingCheck (c1, i1) (c2, i2) Black (Map.fromList rest) board2
    where
      checkedSq = if c2 > c1 then [('f', 8), ('g', 8)] else [('d', 8), ('c', 8)]

-- Checks there are no pieces between king and rook
checkCastlingAdjSq :: Board -> Color -> Bool -> Bool
checkCastlingAdjSq board White True = checkBetweenRow ('e', 1) ('a', 1) board
checkCastlingAdjSq board White False = checkBetweenRow ('e', 1) ('h', 1) board
checkCastlingAdjSq board Black True = checkBetweenRow ('e', 8) ('a', 8) board
checkCastlingAdjSq board Black False = checkBetweenRow ('e', 8) ('h', 8) board

-- Checks if castling is possible
checkCastling :: Location -> Location -> Piece -> Board -> Bool
checkCastling (c1, i1) (c2, i2) (Piece c t b) board =
  checkCastlingMoved (c1, i1) (c2, i2) (Piece c t b) board
    && not (isCheck board board checkColor)
    && checkCastlingCheck (c1, i1) (c2, i2) c board board
    && checkCastlingAdjSq board c checkRook
  where
    checkRook = c2 < c1
    checkColor = if c == White then Black else White

-- Move the King and Rook into the appropriate position when castling
castle :: Color -> Board -> String -> Board
castle White board "Right" = updatedBoard
  where
    insertedBoard = Map.insert ('f', 1) (Piece White Rook False) (Map.insert ('g', 1) (Piece White King False) board)
    updatedBoard = Map.delete ('e', 1) (Map.delete ('h', 1) insertedBoard)
castle White board "Left" = updatedBoard
  where
    insertedBoard = Map.insert ('d', 1) (Piece White Rook False) (Map.insert ('c', 1) (Piece White King False) board)
    updatedBoard = Map.delete ('a', 1) (Map.delete ('e', 1) insertedBoard)
castle Black board "Right" = updatedBoard
  where
    insertedBoard = Map.insert ('f', 8) (Piece Black Rook False) (Map.insert ('g', 8) (Piece Black King False) board)
    updatedBoard = Map.delete ('e', 8) (Map.delete ('h', 8) insertedBoard)
castle _ board _ = updatedBoard
  where
    insertedBoard = Map.insert ('d', 8) (Piece Black Rook False) (Map.insert ('c', 8) (Piece Black King False) board)
    updatedBoard = Map.delete ('e', 8) (Map.delete ('a', 8) insertedBoard)

-- Checks if there are pieces blocking a move from a direction
checkDirection :: Location -> Location -> Board -> Bool
checkDirection (c1, i1) (c2, i2) board
  | c1 /= c2 && i1 /= i2 = checkDiagonal (c1, i1) (c2, i2) board
  | c1 == c2 = checkBetweenCol (c1, i1) (c2, i2) board
  | otherwise = checkBetweenRow (c1, i1) (c2, i2) board

-- Checks if a piece can move from one location to another according to the chess type property
checkLegal :: Location -> Location -> Piece -> Board -> Bool
checkLegal (c1, i1) (c2, i2) (Piece _ Knight _) _
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 2
      || abs (c2i - c1i) == 2 && abs (i2 - i1) == 1
      || abs (c1i - c2i) == 1 && abs (i1 - i2) == 2
      || abs (c1i - c2i) == 2 && abs (i1 - i2) == 1 =
      True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
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
checkLegal (c1, i1) (c2, i2) (Piece Black Pawn _) board
  | i1 == 7 && c1 == c2 && (i1 - i2 == 2 || i1 - i2 == 1) && not (checkBetweenCol (c1, i1) (c2, i2) board) = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | i1 == 7 && c1 == c2 && (i1 - i2 == 2 || i1 - i2 == 1) = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | c1 == c2 && i1 - i2 == 1 = case Map.lookup (c2, i2) board of
      Just _ -> False
      Nothing -> True
  | abs (c2i - c1i) == 1 && (i1 - i2) == 1 = case Map.lookup (c2, i2) board of
      Just (Piece White _ _) -> True
      _ -> False
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ King False) _
  | abs (i2 - i1) == 1 && c2 == c1 = True
  | abs (c2i - c1i) == 1 && i2 == i1 = True
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal l1@(c1, i1) l2@(c2, i2) (Piece White King True) b
  | c2 == 'g' || c2 == 'c' && checkCastling l1 l2 (Piece White King True) b = True
  | abs (i2 - i1) == 1 && c2 == c1 = True
  | abs (c2i - c1i) == 1 && i2 == i1 = True
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal l1@(c1, i1) l2@(c2, i2) (Piece Black King True) b
  | c2 == 'g' || c2 == 'c' && checkCastling l1 l2 (Piece Black King True) b = True
  | abs (i2 - i1) == 1 && c2 == c1 = True
  | abs (c2i - c1i) == 1 && i2 == i1 = True
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Queen _) board
  | c1 == c2 && checkBetweenCol (c1, i1) (c2, i2) board = True
  | i1 == i2 && checkBetweenRow (c1, i1) (c2, i2) board = True
  | abs (i2 - i1) == abs (c2i - c1i) && checkDiagonal (c1, i1) (c2, i2) board = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Rook _) board
  | c1 == c2 && checkBetweenCol (c1, i1) (c2, i2) board = True
  | i1 == i2 && checkBetweenRow (c1, i1) (c2, i2) board = True
  | otherwise = False
checkLegal (c1, i1) (c2, i2) (Piece _ Bishop _) board
  | abs (i2 - i1) == abs (c2i - c1i) && checkDiagonal (c1, i1) (c2, i2) board = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'

-- Gets the color of a piece
getColor :: Piece -> Color
getColor (Piece color _ _) = color

-- Promotes a pawn if it reaches the other end of the board
promotePawn :: Color -> Location -> Location -> Board -> Board
promotePawn White l1 l2 b = Map.insert l2 (Piece White Queen True) (Map.delete l1 b)
promotePawn Black l1 l2 b = Map.insert l2 (Piece Black Queen True) (Map.delete l1 b)

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

{- Function to find the location of the opposite color King.
Used for checking if a player is in check/checkmate
-}
locateKing :: Color -> Board -> Maybe Location
locateKing White board =
  case Map.toList board of
    [] -> Nothing
    ((key, value) : rest) ->
      if value == Piece Black King True || value == Piece Black King False
        then Just key
        else locateKing White (Map.fromList rest)
locateKing Black board =
  case Map.toList board of
    [] -> Nothing
    ((key, value) : rest) ->
      if value == Piece White King True || value == Piece White King False
        then Just key
        else locateKing Black (Map.fromList rest)

{- Function to check if a board is in check by using the opposing players color -}
isCheck :: Board -> Board -> Color -> Bool
isCheck board1 board2 White = case locateKing White board1 of
  Nothing -> False
  Just kingLocation -> case Map.toList board2 of
    [] -> False
    ((key, value) : rest) ->
      ( if getColor value == Black
          then isCheck board1 (Map.fromList rest) White
          else
            (checkLegal key kingLocation value board1 && isCheckAux key kingLocation value board1)
              || isCheck board1 (Map.fromList rest) White
      )
isCheck board1 board2 _ = case locateKing Black board1 of
  Nothing -> False
  Just kingLocation -> case Map.toList board2 of
    [] -> False
    ((key, value) : rest) ->
      ( if getColor value == White
          then isCheck board1 (Map.fromList rest) Black
          else
            (checkLegal key kingLocation value board1 && isCheckAux key kingLocation value board1)
              || isCheck board1 (Map.fromList rest) Black
      )

-- Helper function for isCheck
isCheckAux :: Location -> Location -> Piece -> Board -> Bool
isCheckAux (c1, i1) (c2, i2) (Piece White Pawn _) _ = i2 - i1 == 1 && abs (ord c2 - ord c1) == 1
isCheckAux (c1, i1) (c2, i2) (Piece Black Pawn _) _ = i1 - i2 == 1 && abs (ord c2 - ord c1) == 1
isCheckAux l1@(_, i1) l2@(_, i2) (Piece _ Rook _) board =
  if i1 == i2
    then checkBetweenCol l1 l2 board
    else checkBetweenRow l1 l2 board
isCheckAux l1@(_, _) l2@(_, _) (Piece _ Bishop _) board = checkDiagonal l1 l2 board
isCheckAux l1@(c1, i1) l2@(c2, i2) (Piece _ Queen _) board
  | i1 == i2 = checkBetweenRow l1 l2 board
  | c1 == c2 = checkBetweenCol l1 l2 board
  | otherwise = checkDiagonal l1 l2 board
isCheckAux _ _ _ _ = False

{- Function to check if there is an unblocked path between two locations in a row -}
checkBetweenRow :: Location -> Location -> Board -> Bool
checkBetweenRow (c1, i1) (c2, i2) board
  | (ord c2 - ord c1) > 1 = case Map.lookup (chr (ord c1 + 1), i1) board of
      Nothing -> checkBetweenRow (chr (ord c1 + 1), i1) (c2, i2) board
      Just _ -> False
  | (ord c1 - ord c2) > 1 = case Map.lookup (chr (ord c2 + 1), i1) board of
      Nothing -> checkBetweenRow (c1, i1) (chr (ord c2 + 1), i1) board
      Just _ -> False
  | otherwise = True

{- Function to check if there is an unblocked path between two locations in a column -}
checkBetweenCol :: Location -> Location -> Board -> Bool
checkBetweenCol (c1, i1) (c2, i2) board
  | i2 - i1 > 1 = case Map.lookup (c1, i1 + 1) board of
      Nothing -> checkBetweenCol (c1, i1 + 1) (c2, i2) board
      Just _ -> False
  | i1 - i2 > 1 = case Map.lookup (c1, i2 + 1) board of
      Nothing -> checkBetweenRow (c1, i1) (c2, i2 + 1) board
      Just _ -> False
  | otherwise = True

{- Function to check if there is an unblocked path between two location in a diagonal -}
checkDiagonal :: Location -> Location -> Board -> Bool
checkDiagonal (c1, i1) (c2, i2) board
  | i2 - i1 > 1 && (ord c2 - ord c1) > 1 = case Map.lookup (chr (ord c1 + 1), i1 + 1) board of
      Nothing -> checkDiagonal (chr (ord c1 + 1), i1 + 1) (c2, i2) board
      Just _ -> False
  | i1 - i2 > 1 && (ord c1 - ord c2) > 1 = case Map.lookup (chr (ord c2 + 1), i2 + 1) board of
      Nothing -> checkDiagonal (c1, i1) (chr (ord c2 + 1), i2 + 1) board
      Just _ -> False
  | i2 - i1 > 1 && (ord c1 - ord c2) > 1 = case Map.lookup (chr (ord c2 + 1), i2 - 1) board of
      Nothing -> checkDiagonal (c1, i1) (chr (ord c2 + 1), i2 - 1) board
      Just _ -> False
  | i1 - i2 > 1 && (ord c2 - ord c1) > 1 = case Map.lookup (chr (ord c1 + 1), i1 - 1) board of
      Nothing -> checkDiagonal (chr (ord c1 + 1), i1 - 1) (c2, i2) board
      Just _ -> False
  | otherwise = True

{- Checks if the board is not in checkmate by checking if any pieces
of the color can move to prevent check -}
isNotCheckMate :: Color -> Board -> Board -> Bool
isNotCheckMate White b1 b2 = case Map.toList b2 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == Black
      then isNotCheckMate White b1 (Map.fromList rest)
      else any (checkMove White key value b1) allLocations || isNotCheckMate White b1 (Map.fromList rest)
isNotCheckMate Black b1 b2 = case Map.toList b2 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == White
      then isNotCheckMate Black b1 (Map.fromList rest)
      else any (checkMove Black key value b1) allLocations || isNotCheckMate Black b1 (Map.fromList rest)

-- Checks if the board is in stalemate
isStalemate :: Color -> Board -> Board -> Bool
isStalemate c b1 b2 = case Map.toList b1 of
  [] -> True
  ((key, value) : rest) ->
    if getColor value /= c
      then isStalemate c (Map.fromList rest) b2
      else not (any (checkMove c key value b2) allLocations) && isStalemate c (Map.fromList rest) b2
