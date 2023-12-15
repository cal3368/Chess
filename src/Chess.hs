module Chess where

import Data.Char (chr, digitToInt, isAlpha, isDigit, ord)
import Data.Map.Strict qualified as Map
import DrawBoard
  ( Board,
    Color (..),
    Location,
    Piece (..),
    Type (..),
    drawBoard,
    newBoard,
  )

getName :: String -> IO String
getName color = do
  putStrLn ("Enter name of " ++ color ++ ":")
  getLine

validateOwner :: Location -> Color -> Board -> Bool
validateOwner (c, i) color board = case Map.lookup (c, i) board of
  Nothing -> False
  Just p -> color == getColor p

-- checkInbounds :: Location -> Bool
-- checkInbounds (c, i)
--   | i <= 0 || i > 8 = False
--   | (ord c - ord 'a') > 7 || (ord c - ord 'a') < 0 = False
--   | otherwise = True

checkMove :: Location -> Location -> Piece -> Board -> Bool
checkMove source dest (Piece c t) b =
  validateOwner source c b
    && checkLegal source dest (Piece c t)
    && not (isCheck mod_board c)
  where
    insertion = Map.insert dest (Piece c t) b
    mod_board = Map.delete source insertion

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
checkLegal (c1, i1) (c2, i2) (Piece _ Rook)
  | c1 == c2 || i1 == i2 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Bishop)
  | abs (i2 - i1) == abs (c2i - c1i) = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'

getColor :: Piece -> Color
getColor (Piece color _) = color

getType :: Piece -> Type
getType (Piece _ t) = t

makeMove :: (Location, Location) -> Color -> Board -> Board
makeMove (l1@(c1, i1), l2@(c2, i2)) color board
  | not (validateOwner (c1, i1) color board) = board

-- Not completed. Need to check validity

{- Function to find the location of the opposite color King.
Used for checking if a player is in check/checkate
-}
locateKing :: Color -> Board -> Maybe Location
locateKing White board =
  case Map.toList board of
    [] -> Nothing
    ((key, value) : rest) ->
      if value == Piece Black King
        then Just key
        else locateKing White (Map.fromList rest)
locateKing Black board =
  case Map.toList board of
    [] -> Nothing
    ((key, value) : rest) ->
      if value == Piece White King
        then Just key
        else locateKing Black (Map.fromList rest)

{- Function to check if a board is in check -}
isCheck :: Board -> Color -> Bool
isCheck board White = case locateKing Black board of
  Nothing -> False
  Just kingLocation -> case Map.toList board of
    [] -> False
    ((key, value) : rest) ->
      ( if getColor value == White
          then isCheck (Map.fromList rest) White
          else
            (checkLegal key kingLocation value && isCheckAux key kingLocation value board)
              || isCheck (Map.fromList rest) White
      )
isCheck board _ = case locateKing White board of
  Nothing -> False
  Just kingLocation -> case Map.toList board of
    [] -> False
    ((key, value) : rest) ->
      ( if getColor value == Black
          then isCheck (Map.fromList rest) Black
          else
            (checkLegal key kingLocation value && isCheckAux key kingLocation value board)
              || isCheck (Map.fromList rest) Black
      )

isCheckAux :: Location -> Location -> Piece -> Board -> Bool
isCheckAux (c1, i1) (c2, i2) (Piece White Pawn) _ = i2 - i1 == 1 && abs (ord c2 - ord c1) == 1
isCheckAux (c1, i1) (c2, i2) (Piece Black Pawn) _ = i1 - i2 == 1 && abs (ord c2 - ord c1) == 1
isCheckAux l1@(c1, i1) l2@(c2, i2) (Piece _ Rook) board =
  if i1 == i2
    then checkBetweenCol l1 l2 board
    else checkBetweenRow l1 l2 board
isCheckAux l1@(c1, i1) l2@(c2, i2) (Piece _ Bishop) board = checkDiagonal l1 l2 board
isCheckAux l1@(c1, i1) l2@(c2, i2) (Piece _ Queen) board
  | i1 == i2 = checkBetweenRow l1 l2 board
  | c1 == c2 = checkBetweenCol l1 l2 board
  | otherwise = checkDiagonal l1 l2 board
isCheckAux _ _ _ _ = True

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
play board p1 p2 _ = do
  putStrLn ""
  drawBoard board
  putStrLn ""
  move <- promptForAndValidate (p2 ++ " make your move:")
  case move of
    (('q', 0), ('q', 0)) ->
      return ()
    -- Possible add a pause game, save game, and restart game option
    _ ->
      -- make move
      play board p1 p2 1

main :: IO ()
main = do
  putStrLn "Chess\n"
  player1 <- getName "white"
  player2 <- getName "black"
  let board = newBoard
  play board player1 player2 1
