module Chess
  ( main,
  )
where

import qualified Control.Monad
import Data.Char (chr, digitToInt, isAlpha, isDigit, ord)
import qualified Data.Map.Strict as Map
import Data.Time.Clock (UTCTime (utctDayTime), getCurrentTime)
import DrawBoard
  ( Board,
    Color (..),
    Location,
    Piece (..),
    Type (..),
    drawBoard,
    newBoard,
  )
import System.Console.ANSI (clearScreen, setCursorPosition)
import Text.Read (readMaybe)

allLocations :: [Location]
allLocations = [(char, num) | char <- ['a' .. 'h'], num <- [1 .. 8]]

-- checkPromotion :: Location -> Color -> Bool
-- checkPromotion (_, row) White = row == 8
-- checkPromotion (_, row) Black = row == 1

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

-- Check if the adjascent squares of castle and rook are under check
checkCastlingCheck :: Location -> Location -> Color -> Board -> Board -> Bool
checkCastlingCheck (c1, i1) (c2, i2) White board1 board2 = case Map.toList board1 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == White
      then checkCastlingCheck (c1, i1) (c2, i2) White (Map.fromList rest) board2
      else not (any (checkMove Black key value board2) checkedSq)
    where
      checkedSq = if c2 > c1 then [('f', 1), ('g', 1)] else [('d', 1), ('c', 1)]
checkCastlingCheck (c1, i1) (c2, i2) Black board1 board2 = case Map.toList board1 of
  [] -> False
  ((key, value) : rest) ->
    if getColor value == Black
      then checkCastlingCheck (c1, i1) (c2, i2) Black (Map.fromList rest) board2
      else not (any (checkMove White key value board2) checkedSq)
    where
      checkedSq = if c2 > c1 then [('f', 8), ('g', 8)] else [('d', 8), ('c', 8)]

-- Checks there are no pieces between king and rook
checkCastlingAdjSq :: Board -> Color -> Bool -> Bool
checkCastlingAdjSq board White True = checkBetweenRow ('e', 1) ('a', 1) board
checkCastlingAdjSq board White False = checkBetweenRow ('e', 1) ('h', 1) board
checkCastlingAdjSq board Black True = checkBetweenRow ('e', 8) ('a', 8) board
checkCastlingAdjSq board Black False = checkBetweenRow ('e', 8) ('h', 8) board

-- Check castling condition
checkCastling :: Location -> Location -> Piece -> Board -> Bool
checkCastling (c1, i1) (c2, i2) (Piece c t b) board =
  checkCastlingMoved (c1, i1) (c2, i2) (Piece c t b) board
    && not (isCheck board board checkColor)
    && checkCastlingCheck (c1, i1) (c2, i2) c board board
    && checkCastlingAdjSq board c checkRook
  where
    checkRook = c2 < c1
    checkColor = if c == White then Black else White

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

checkDirection :: Location -> Location -> Board -> Bool
checkDirection (c1, i1) (c2, i2) board
  | c1 /= c2 && i1 /= i2 = checkDiagonal (c1, i1) (c2, i2) board
  | c1 == c2 = checkBetweenCol (c1, i1) (c2, i2) board
  | otherwise = checkBetweenRow (c1, i1) (c2, i2) board

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

getColor :: Piece -> Color
getColor (Piece color _ _) = color

-- getType :: Piece -> Type
-- getType (Piece _ t _) = t

promotePawn :: Color -> Location -> Location -> Board -> Board
promotePawn White l1 l2 b = Map.insert l2 (Piece White Queen True) (Map.delete l1 b)
promotePawn Black l1 l2 b = Map.insert l2 (Piece Black Queen True) (Map.delete l1 b)

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
Used for checking if a player is in check/checkate
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
    "pause" -> do
      return (('p', 0), ('p', 0))
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

getName :: String -> IO String
getName color = do
  putStrLn ("Enter name of " ++ color ++ ":")
  getLine

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

waitForResume :: IO ()
waitForResume = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Enter 'resume' to continue"
  input <- getLine
  Control.Monad.unless (input == "resume") $ waitForResume

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
                  let newBoard1 = makeMove l1 l2 piece board
                   in if isCheck newBoard newBoard1 White && not (isCheckMate Black newBoard1 newBoard1)
                        then do
                          drawBoard newBoard1
                          putStrLn ("Checkmate! " ++ p1 ++ " wins!")
                          return ()
                        else
                          if isCheck newBoard newBoard1 White
                            then do
                              putStrLn "Check"
                              play newBoard1 p1 p2 2
                            else play newBoard1 p1 p2 2
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
                  let newBoard1 = makeMove l1 l2 piece board
                   in if isCheck newBoard1 newBoard1 Black && not (isCheckMate White newBoard1 newBoard1)
                        then do
                          drawBoard newBoard1
                          putStrLn ("Checkmate! " ++ p2 ++ " wins!")
                          return ()
                        else
                          if isCheck newBoard1 newBoard1 Black
                            then do
                              putStrLn "Check"
                              play newBoard1 p1 p2 1
                            else play newBoard1 p1 p2 1
                else do
                  putStrLn "Illegal Move. Try again!"
                  play board p1 p2 2
            Nothing -> do
              putStrLn "Illegal Move. Try again!"
              play board p1 p2 2

playWithTimer :: Board -> String -> String -> Int -> Int -> Int -> IO ()
playWithTimer board p1 p2 1 time1 time2
  | time2 <= 0 = do
      putStrLn (p2 ++ " ran out of time. " ++ p1 ++ " wins!")
      return ()
  | otherwise = do
      putStrLn ""
      drawBoard board
      putStrLn ""
      putStrLn (p1 ++ " has " ++ show time1 ++ " seconds left")
      start <- getCurrentTime
      let startInt = floor $ utctDayTime start :: Int
      if not (isCheck board board White) && isStalemate White board board
        then do
          putStrLn (p1 ++ " is not in check and cannot make a move. It's a stalemate!")
          return ()
        else do
          move <- promptForAndValidate (p1 ++ " make your move:")
          case move of
            (('q', 0), ('q', 0)) ->
              return ()
            (('p', 0), ('p', 0)) -> do
              end <- getCurrentTime
              let endInt = floor $ utctDayTime end :: Int
              let diff = endInt - startInt
              waitForResume
              playWithTimer board p1 p2 1 (time1 - diff) time2
            (('z', 0), ('z', 0)) -> do
              displayInstructions
              end <- getCurrentTime
              let endInt = floor $ utctDayTime end :: Int
              let diff = endInt - startInt
              playWithTimer board p1 p2 1 (time1 - diff) time2
            (('x', 0), ('x', 0)) -> do
              putStrLn (p1 ++ " offers a draw to " ++ p2)
              response <- respondToDraw
              if response == "no"
                then do
                  end <- getCurrentTime
                  let endInt = floor $ utctDayTime end :: Int
                  let diff = endInt - startInt
                  playWithTimer board p1 p2 1 (time1 - diff) time2
                else do
                  putStrLn "Draw accepted. It's a draw!"
                  return ()
            (l1, l2) ->
              case Map.lookup l1 board of
                Just piece ->
                  if checkMove White l1 piece board l2
                    then
                      let newBoard1 = makeMove l1 l2 piece board
                       in if isCheck newBoard newBoard1 White && not (isCheckMate Black newBoard1 newBoard1)
                            then do
                              drawBoard newBoard1
                              putStrLn ("Checkmate! " ++ p1 ++ " wins!")
                              return ()
                            else do
                              end <- getCurrentTime
                              let endInt = floor $ utctDayTime end :: Int
                              let diff = endInt - startInt
                              if isCheck newBoard newBoard1 White
                                then do
                                  putStrLn "Check"
                                  if time1 - diff < 30
                                    then playWithTimer newBoard1 p1 p2 2 30 time2
                                    else playWithTimer newBoard1 p1 p2 2 (time1 - diff) time2
                                else
                                  if time1 - diff < 30
                                    then playWithTimer newBoard1 p1 p2 2 30 time2
                                    else playWithTimer newBoard1 p1 p2 2 (time1 - diff) time2
                    else do
                      putStrLn "Illegal Move. Try again!"
                      end <- getCurrentTime
                      let endInt = floor $ utctDayTime end :: Int
                      let diff = endInt - startInt
                      playWithTimer board p1 p2 1 (time1 - diff) time2
                Nothing -> do
                  putStrLn "Illegal Move. Try again!"
                  end <- getCurrentTime
                  let endInt = floor $ utctDayTime end :: Int
                  let diff = endInt - startInt
                  playWithTimer board p1 p2 1 (time1 - diff) time2
playWithTimer board p1 p2 _ time1 time2
  | time1 <= 0 = do
      putStrLn (p1 ++ " ran out of time. " ++ p2 ++ " wins!")
      return ()
  | otherwise = do
      putStrLn ""
      drawBoard board
      putStrLn ""
      putStrLn (p2 ++ " has " ++ show time2 ++ " seconds left")
      start <- getCurrentTime
      let startInt = floor $ utctDayTime start :: Int
      if not (isCheck board board Black) && isStalemate Black board board
        then do
          putStrLn (p2 ++ " is not in check and cannot make a move. It's a stalemate!")
          return ()
        else do
          move <- promptForAndValidate (p2 ++ " make your move:")
          case move of
            (('q', 0), ('q', 0)) ->
              return ()
            (('p', 0), ('p', 0)) -> do
              end <- getCurrentTime
              let endInt = floor $ utctDayTime end :: Int
              let diff = endInt - startInt
              waitForResume
              playWithTimer board p1 p2 2 time1 (time2 - diff)
            (('z', 0), ('z', 0)) -> do
              displayInstructions
              end <- getCurrentTime
              let endInt = floor $ utctDayTime end :: Int
              let diff = endInt - startInt
              playWithTimer board p1 p2 2 time1 (time2 - diff)
            (('x', 0), ('x', 0)) -> do
              putStrLn (p2 ++ " offers a draw to " ++ p1)
              response <- respondToDraw
              if response == "no"
                then do
                  end <- getCurrentTime
                  let endInt = floor $ utctDayTime end :: Int
                  let diff = endInt - startInt
                  playWithTimer board p1 p2 2 time1 (time2 - diff)
                else do
                  putStrLn "Draw accepted. It's a draw!"
                  return ()
            (l1, l2) ->
              case Map.lookup l1 board of
                Just piece ->
                  if checkMove Black l1 piece board l2
                    then
                      let newBoard1 = makeMove l1 l2 piece board
                       in if isCheck newBoard1 newBoard1 Black && not (isCheckMate White newBoard1 newBoard1)
                            then do
                              drawBoard newBoard1
                              putStrLn ("Checkmate! " ++ p2 ++ " wins!")
                              return ()
                            else do
                              end <- getCurrentTime
                              let endInt = floor $ utctDayTime end :: Int
                              let diff = endInt - startInt
                              if isCheck newBoard1 newBoard1 Black
                                then do
                                  putStrLn "Check"
                                  if time2 - diff < 30
                                    then playWithTimer newBoard1 p1 p2 2 time1 30
                                    else playWithTimer newBoard1 p1 p2 2 time1 (time2 - diff)
                                else
                                  if time2 - diff < 30
                                    then playWithTimer newBoard1 p1 p2 2 time1 30
                                    else playWithTimer newBoard1 p1 p2 2 time1 (time2 - diff)
                    else do
                      putStrLn "Illegal Move. Try again!"
                      end <- getCurrentTime
                      let endInt = floor $ utctDayTime end :: Int
                      let diff = endInt - startInt
                      playWithTimer board p1 p2 2 time1 (time2 - diff)
                Nothing -> do
                  putStrLn "Illegal Move. Try again!"
                  end <- getCurrentTime
                  let endInt = floor $ utctDayTime end :: Int
                  let diff = endInt - startInt
                  playWithTimer board p1 p2 2 time1 (time2 - diff)

displayInstructions :: IO ()
displayInstructions = do
  putStrLn "Played with two users"
  putStrLn "Possible commands are 'help', 'draw', 'quit', and making a move"
  putStrLn "Moves are made using the form 'xi xi' where x can be any character between a and h and i can be any integer from 1 to 8"
  putStrLn "If you are using a timer you can pause with 'pause'"
  putStrLn ""

getTime :: IO Int
getTime = do
  putStrLn "Please Enter Timer Amount('none' if no timer wanted): "
  time <- getLine
  case readMaybe time :: Maybe Int of
    Just i -> return i
    Nothing -> return 0

main :: IO ()
main = do
  putStrLn "Chess\n"
  displayInstructions
  let board = newBoard
  player1 <- getName "white"
  player2 <- getName "black"
  timer <- getTime
  case timer of
    0 -> play board player1 player2 1
    i -> playWithTimer board player1 player2 1 i i
