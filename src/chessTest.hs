import Chess (checkInbounds, checkLegal, validateOwner)
import DrawBoard (Board, Color (Black, White), Piece (..), Type (..), newBoard)

main :: IO ()
main = do
  testValidateOwner
  testCheckInbound
  testCheckLegal

mockBoard :: Board
mockBoard = newBoard

testFunction :: String -> Bool -> Bool -> IO ()
testFunction testName expected actual =
  if expected == actual
    then putStrLn (testName ++ " passed.")
    else putStrLn (testName ++ " failed.")

testValidateOwner :: IO ()
testValidateOwner = do
  testFunction "Occupied by Correct Color" True (validateOwner ('a', 2) White mockBoard)
  testFunction "Occupied by Wrong Color" False (validateOwner ('a', 2) Black mockBoard)
  testFunction "Empty Location" False (validateOwner ('d', 3) White mockBoard && validateOwner ('d', 3) Black mockBoard)
  testFunction "Out of Bounds" False (validateOwner ('z', 90) White mockBoard && validateOwner ('z', 90) Black mockBoard)

testCheckInbound :: IO ()
testCheckInbound = do
  testFunction "Check in bound" True (checkInbounds ('a', 2))
  testFunction "Check in bound" False (checkInbounds ('a', 9))
  testFunction "Check in bound" False (checkInbounds ('a', -10))
  testFunction "Check in bound" False (checkInbounds ('z', 2))

testCheckLegal :: IO ()
testCheckLegal = do
  -- Pawn Tests
  testFunction "Pawn forward move" True (checkLegal ('d', 2) ('d', 3) (Piece White Pawn))
  testFunction "Pawn illegal move" False (checkLegal ('d', 2) ('d', 5) (Piece White Pawn))

  -- Knight Tests
  testFunction "Knight leagal move" True (checkLegal ('b', 1) ('c', 3) (Piece White Knight))
  testFunction "Knight illegal move" False (checkLegal ('b', 1) ('b', 3) (Piece White Knight))

  -- Rook Tests
  testFunction "Rook vertical move" True (checkLegal ('d', 4) ('d', 7) (Piece White Rook))
  testFunction "Rook horizontal move" True (checkLegal ('f', 5) ('a', 5) (Piece White Rook))
  testFunction "Rook diagonal move" False (checkLegal ('f', 5) ('h', 7) (Piece White Rook))

  -- Bishop Tests
  testFunction "Bishop diagonal move" True (checkLegal ('c', 1) ('f', 4) (Piece White Bishop))
  testFunction "Bishop straight move" False (checkLegal ('c', 1) ('c', 4) (Piece White Bishop))

  -- Queen Tests
  testFunction "Queen diagonal move" True (checkLegal ('d', 1) ('g', 4) (Piece White Queen))
  testFunction "Queen vertical move" True (checkLegal ('d', 1) ('d', 5) (Piece White Queen))
  testFunction "Queen horizontal move" True (checkLegal ('d', 1) ('a', 1) (Piece White Queen))

  -- King Tests
  testFunction "King one square move" True (checkLegal ('e', 1) ('e', 2) (Piece White King))
  testFunction "King two square move" False (checkLegal ('e', 1) ('e', 3) (Piece White King))
