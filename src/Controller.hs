module Controller (main) where

import Chess
import qualified Control.Monad
import Data.Char (digitToInt, isAlpha, isDigit)
import qualified Data.Map as Map
import Data.Time.Clock (getCurrentTime, utctDayTime)
import DrawBoard
  ( drawBoard,
    newBoard,
  )
import System.Console.ANSI (clearScreen, setCursorPosition)
import Text.Read (readMaybe)

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
  Control.Monad.unless (input == "resume") waitForResume

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
                   in if isCheck newBoard newBoard1 White && not (isNotCheckMate Black newBoard1 newBoard1)
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
                   in if isCheck newBoard1 newBoard1 Black && not (isNotCheckMate White newBoard1 newBoard1)
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
                       in if isCheck newBoard newBoard1 White && not (isNotCheckMate Black newBoard1 newBoard1)
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
                                  if time1 - diff < 30 && 0 < time1 - diff
                                    then playWithTimer newBoard1 p1 p2 2 30 time2
                                    else playWithTimer newBoard1 p1 p2 2 (time1 - diff) time2
                                else
                                  if time1 - diff < 30 && 0 < time1 - diff
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
                       in if isCheck newBoard1 newBoard1 Black && not (isNotCheckMate White newBoard1 newBoard1)
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
                                  if time2 - diff < 30 && 0 < time2 - diff
                                    then playWithTimer newBoard1 p1 p2 1 time1 30
                                    else playWithTimer newBoard1 p1 p2 1 time1 (time2 - diff)
                                else
                                  if time2 - diff < 30 && 0 < time2 - diff
                                    then playWithTimer newBoard1 p1 p2 1 time1 30
                                    else playWithTimer newBoard1 p1 p2 1 time1 (time2 - diff)
                    else do
                      putStrLn "Illegal Move. Try again!"
                      end <- getCurrentTime
                      let endInt = floor $ utctDayTime end :: Int
                      let diff = endInt - startInt
                      playWithTimer board p1 p2 1 time1 (time2 - diff)
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
