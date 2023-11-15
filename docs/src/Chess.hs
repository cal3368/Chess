module Chess where

import Data.Char (digitToInt, isAlpha, isDigit, ord)
import Data.Map.Strict qualified as Map
import DrawBoard

getName :: String -> IO String
getName color = do
  putStrLn ("Enter name of " ++ color ++ ":")
  getLine

validateOwner :: Location -> Color -> Board -> Bool
validateOwner (c, i) color board = case Map.lookup (c, i) board of
  Nothing -> False
  Just p -> color == getColor (getPiece p)

checkInbounds :: Location -> Bool
checkInbounds (c, i)
  | i <= 0 || i > 8 = False
  | (ord c - ord 'a') > 7 || (ord c - ord 'a') < 0 = False
  | otherwise = True

checkLegal :: Location -> Location -> Piece -> Bool
checkLegal (c1, i1) (c2, i2) (Piece _ Knight)
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 2
      || abs (c2i - c1i) == 2 && abs (i2 - i1) == 1
      || abs (c1i - c2i) == 1 && abs (i1 - i2) == 2
      || abs (c1i - c2i) == 2 && abs (i1 - i2) == 1 =
      True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece White Pawn)
  | i1 == 2 && c1 == c2 && (i2 - i1 == 2 || i2 - i1 == 1) = True
  | c1 == c2 && (i2 - i1 == 2 || i2 - i1 == 1) = True
  | abs (c2i - c1i) == 1 && (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece Black Pawn)
  | i1 == 7 && c1 == c2 && (i1 - i2 == 2 || i1 - i2 == 1) = True
  | c1 == c2 && (i1 - i2 == 2 || i1 - i2 == 1) = True
  | abs (c2i - c1i) == 1 && (i1 - i2) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ King)
  | abs (i2 - i1) == 1 && c2 == c1 = True
  | abs (c2i - c1i) == 1 && i2 == i1 = True
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Queen)
  | c1 == c2 || i1 == i2 || abs (i2 - i1) == abs (c2i - c1i) = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'

getPiece :: Square -> Piece
getPiece (Square piece) = piece

getColor :: Piece -> Color
getColor (Piece color _) = color

getType :: Piece -> Type
getType (Piece _ t) = t

promptForAndValidate :: String -> IO (Location, Location)
promptForAndValidate msg = do
  putStrLn msg
  input <- getLine
  case input of
    "quit" -> do
      putStrLn "Game is over"
      return (('q', 0), ('q', 0))
    [x1, y1, ' ', x2, y2] -> do
      ( if isAlpha x1
          && isAlpha x2
          && x1 < 'h'
          && x2 < 'h'
          && isDigit y1
          && isDigit y2
          && digitToInt y1 > 0
          && digitToInt y1 <= 8
          && digitToInt y2 > 0
          && digitToInt y2 <= 8
          then return ((x1, digitToInt y1), (x2, digitToInt y2))
          else do
            putStrLn invalid
            promptForAndValidate msg
        )
    _ -> do
      putStrLn invalid
      promptForAndValidate msg
  where
    invalid = "Invalid move. Try again."

play :: Board -> String -> String -> Int -> IO ()
play board p1 p2 1 = do
  putStrLn ""
  drawBoard board
  putStrLn ""
  move <- promptForAndValidate (p1 ++ " make your move:")
  case move of
    (('q', 0), ('q', 0)) ->
      return ()
    -- Possible add a pause game, save game, and restart game option
    _ ->
      -- make move
      play board p1 p2 2
play board p1 p2 _ = return ()

main :: IO ()
main = do
  putStrLn "Chess\n"
  player1 <- getName "white"
  player2 <- getName "black"
  let board = newBoard
  play board player1 player2 1
