module Chess where

import Data.Char (chr, digitToInt, isAlpha, isDigit, ord)
import qualified Data.Map.Strict as Map
import Data.Time.Clock
import DrawBoard
  ( Board,
    Color (..),
    Location,
    Piece (..),
    Type (..),
    drawBoard,
    newBoard,
  )

allLocations :: [Location]
allLocations = [(char, num) | char <- ['a' .. 'h'], num <- [1 .. 8]]

checkPromotion :: Location -> Color -> Bool
checkPromotion (_, row) White = row == 8
checkPromotion (_, row) Black = row == 1

getName :: String -> IO String
getName color = do
  putStrLn ("Enter name of " ++ color ++ ":")
  getLine

validateOwner :: Location -> Color -> Board -> Bool
validateOwner (c, i) color board = case Map.lookup (c, i) board of
  Nothing -> False
  Just p -> color == getColor p

checkInbounds :: Location -> Bool
checkInbounds (c, i)
  | i <= 0 || i > 8 = False
  | (ord c - ord 'a') > 7 || (ord c - ord 'a') < 0 = False
  | otherwise = True

checkDestSourceColors :: Location -> Color -> Board -> Bool
checkDestSourceColors loc color b = case Map.lookup loc b of
  Nothing -> True
  Just p -> color /= getColor p

checkMove :: Color -> Location -> Piece -> Board -> Location -> Bool
checkMove Black source piece b dest =
  validateOwner source Black b
    && checkInbounds dest
    && checkDestSourceColors dest Black b
    && checkLegal source dest piece b
    && checkDirection source dest b
    && not (isCheck mod_board mod_board White)
  where
    insertion = Map.insert dest piece b
    mod_board = Map.delete source insertion
checkMove White source piece b dest =
  validateOwner source White b
    && checkInbounds dest
    && checkDestSourceColors dest White b
    && checkLegal source dest piece b
    && checkDirection source dest b
    && not (isCheck mod_board mod_board Black)
  where
    insertion = Map.insert dest piece b
    mod_board = Map.delete source insertion

-- checkMove :: Location -> Piece -> Board -> Location -> Bool
-- checkMove source (Piece Black t) b dest =
--   validateOwner source Black b
--     && checkLegal source dest (Piece Black t) b
--     && not (isCheck mod_board mod_board White)
--   where
--     insertion = Map.insert dest (Piece Black t) b
--     mod_board = Map.delete source insertion
-- checkMove source (Piece White t) b dest =
--   validateOwner source White b
--     && checkLegal source dest (Piece White t) b
--     && not (isCheck mod_board mod_board Black)
--   where
--     insertion = Map.insert dest (Piece White t) b
--     mod_board = Map.delete source insertion

checkDirection :: Location -> Location -> Board -> Bool
checkDirection (c1, i1) (c2, i2) board
  | c1 /= c2 && i1 /= i2 = checkDiagonal (c1, i1) (c2, i2) board
  | c1 == c2 = checkBetweenCol (c1, i1) (c2, i2) board
  | otherwise = checkBetweenRow (c1, i1) (c2, i2) board

checkLegal :: Location -> Location -> Piece -> Board -> Bool
checkLegal (c1, i1) (c2, i2) (Piece _ Knight) _
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 2
      || abs (c2i - c1i) == 2 && abs (i2 - i1) == 1
      || abs (c1i - c2i) == 1 && abs (i1 - i2) == 2
      || abs (c1i - c2i) == 2 && abs (i1 - i2) == 1 =
      True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece White Pawn) board
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
      Just (Piece Black _) -> True
      _ -> False
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece Black Pawn) board
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
      Just (Piece White _) -> True
      _ -> False
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ King) _
  | abs (i2 - i1) == 1 && c2 == c1 = True
  | abs (c2i - c1i) == 1 && i2 == i1 = True
  | abs (c2i - c1i) == 1 && abs (i2 - i1) == 1 = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Queen) board
  | c1 == c2 && checkBetweenCol (c1, i1) (c2, i2) board = True
  | i1 == i2 && checkBetweenRow (c1, i1) (c2, i2) board = True
  | abs (i2 - i1) == abs (c2i - c1i) && checkDiagonal (c1, i1) (c2, i2) board = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Rook) board
  | c1 == c2 && checkBetweenCol (c1, i1) (c2, i2) board = True
  | i1 == i2 && checkBetweenRow (c1, i1) (c2, i2) board = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'
checkLegal (c1, i1) (c2, i2) (Piece _ Bishop) board
  | abs (i2 - i1) == abs (c2i - c1i) && checkDiagonal (c1, i1) (c2, i2) board = True
  | otherwise = False
  where
    c1i = ord c1 - ord 'a'
    c2i = ord c2 - ord 'a'

getColor :: Piece -> Color
getColor (Piece color _) = color

getType :: Piece -> Type
getType (Piece _ t) = t

makeMove :: Location -> Location -> Piece -> Board -> Board
makeMove l1 l2 p b = mod_board
  where
    insertion = Map.insert l2 p b
    mod_board = Map.delete l1 insertion

-- makeMove :: (Location, Location) -> Color -> Board -> Board
-- makeMove (l1@(c1, i1), l2@(c2, i2)) color board
--   | not (validateOwner (c1, i1) color board) = board

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

isCheckMate :: Color -> Board -> Board -> Bool
isCheckMate White b1 b2 = case Map.toList b2 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == Black
      then isCheckMate White b1 (Map.fromList rest)
      else any (checkMove White key value b1) allLocations || isCheckMate White b1 (Map.fromList rest)
isCheckMate Black b1 b2 = case Map.toList b2 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == White
      then isCheckMate Black b1 (Map.fromList rest)
      else any (checkMove Black key value b1) allLocations || isCheckMate Black b1 (Map.fromList rest)

isStalemate :: Color -> Board -> Board -> Bool
isStalemate c b1 b2 = case Map.toList b1 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value /= c
      then isStalemate c (Map.fromList rest) b2
      else not (any (checkMove c key value b2) allLocations) && isStalemate c (Map.fromList rest) b2

promptForAndValidate :: String -> IO (Location, Location)
promptForAndValidate msg = do
  putStrLn msg
  input <- getLine
  case input of
    "quit" -> do
      putStrLn "Game is over"
      return (('q', 0), ('q', 0))
    "draw" -> do
      return (('x', 0), ('x', 0))
    "help" -> do
      return (('z', 0), ('z', 0))
    [x1, y1, ' ', x2, y2] -> do
      ( if isAlpha x1
          && isAlpha x2
          && x1 <= 'h'
          && x2 <= 'h'
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

respondToDraw :: IO String
respondToDraw = do
  putStrLn "Do you accept the draw?"
  response <- getLine
  if response == "yes"
    then return "yes"
    else
      if response == "no"
        then return "no"
        else do
          putStrLn "Please respond with 'yes' or 'no'"
          respondToDraw

play :: Board -> String -> String -> Int -> IO ()
play board p1 p2 1 = do
  putStrLn ""
  drawBoard board
  putStrLn ""
  if not (isCheck board board White) && isStalemate White board board
    then do
      putStrLn (p1 ++ " is not in check and cannot make a move. It's a stalemate!")
      return ()
    else do
      move <- promptForAndValidate (p1 ++ " make your move:")
      case move of
        (('q', 0), ('q', 0)) ->
          return ()
        (('z', 0), ('z', 0)) -> do
          displayInstructions
          play board p1 p2 1
        (('x', 0), ('x', 0)) -> do
          putStrLn (p1 ++ " offers a draw to " ++ p2)
          response <- respondToDraw
          if response == "no"
            then play board p1 p2 1
            else do
              putStrLn "Draw accepted. It's a draw!"
              return ()
        (l1, l2) ->
          case Map.lookup l1 board of
            Just piece ->
              if checkMove White l1 piece board l2
                then
                  let newBoard = makeMove l1 l2 piece board
                   in if isCheck newBoard newBoard White && (not (isCheckMate Black newBoard newBoard))
                        then do
                          drawBoard newBoard
                          putStrLn ("Checkmate! " ++ p1 ++ " wins!")
                          return ()
                        else
                          if isCheck newBoard newBoard White
                            then do
                              putStrLn "Check"
                              play newBoard p1 p2 2
                            else play newBoard p1 p2 2
                else do
                  putStrLn "Illegal Move. Try again!"
                  play board p1 p2 1
            Nothing -> do
              putStrLn "Illegal Move. Try again!"
              play board p1 p2 1
play board p1 p2 _ = do
  putStrLn ""
  drawBoard board
  putStrLn ""
  if not (isCheck board board Black) && isStalemate Black board board
    then do
      putStrLn (p2 ++ " is not in check and cannot make a move. It's a stalemate!")
      return ()
    else do
      move <- promptForAndValidate (p2 ++ " make your move:")
      case move of
        (('q', 0), ('q', 0)) ->
          return ()
        (('z', 0), ('z', 0)) -> do
          displayInstructions
          play board p1 p2 2
        (('x', 0), ('x', 0)) -> do
          putStrLn (p2 ++ " offers a draw to " ++ p1)
          response <- respondToDraw
          if response == "no"
            then play board p1 p2 2
            else do
              putStrLn "Draw accepted. It's a draw!"
              return ()
        (l1, l2) ->
          case Map.lookup l1 board of
            Just piece ->
              if checkMove Black l1 piece board l2
                then
                  let newBoard = makeMove l1 l2 piece board
                   in if isCheck newBoard newBoard Black && (not (isCheckMate White newBoard newBoard))
                        then do
                          drawBoard newBoard
                          putStrLn ("Checkmate! " ++ p2 ++ " wins!")
                          return ()
                        else
                          if isCheck newBoard newBoard Black
                            then do
                              putStrLn "Check"
                              play newBoard p1 p2 1
                            else play newBoard p1 p2 1
                else do
                  putStrLn "Illegal Move. Try again!"
                  play board p1 p2 2
            Nothing -> do
              putStrLn "Illegal Move. Try again!"
              play board p1 p2 2

displayInstructions :: IO ()
displayInstructions = do
  putStrLn "Played with two users"
  putStrLn "Possible commands are 'help', 'draw', 'quit', and making a move"
  putStrLn "Moves are made using the form 'xi xi' where x can be any character between a and h and i can be any integer from 1 to 8"
  putStrLn ""

main :: IO ()
main = do
  putStrLn "Chess\n"
  displayInstructions
  player1 <- getName "white"
  player2 <- getName "black"
  let player1Timer = 5400
  let player2Timer = 5400
  let board = newBoard
  play board player1 player2 1
