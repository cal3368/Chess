module Chess where

import Data.Char (digitToInt, isAlpha, isDigit)
import DrawBoard

getName :: String -> IO String
getName color = do
  putStrLn ("Enter name of " ++ color ++ ":")
  getLine

-- getPiece :: Board -> Location -> Maybe Piece

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

play :: String -> String -> Int -> IO ()
play p1 p2 1 = do
  putStrLn ""
  -- putStrLn (drawBoard)
  move <- promptForAndValidate (p1 ++ " make your move")
  case move of
    (('q', 0), ('q', 0)) ->
      return ()
    -- Possible add a pause game, save game, and restart game option
    _ ->
      -- make move
      play p1 p2 2
play p1 p2 _ = return ()

main :: IO ()
main = do
  putStrLn "Chess\n"
  player1 <- getName "white"
  player2 <- getName "black"
  play player1 player2 1
