import Chess hiding (main)
import qualified Data.Map.Strict as Map
import DrawBoard
import Test.HUnit

main :: IO Counts
main = do
  runTestTT tests

tests :: Test
tests =
  TestList
    [ TestLabel "Valid Owner" test1,
      TestLabel "Valid Owner" test2,
      TestLabel "Valid Owner" test3,
      TestLabel "Check Inbound" test4,
      TestLabel "Check Inbound" test5,
      TestLabel "Check Inbound" test6,
      TestLabel "Check Inbound" test7,
      TestLabel "Check Legal" test8,
      TestLabel "Check Legal" test9,
      TestLabel "Check Legal" test10,
      TestLabel "Check Legal" test11,
      TestLabel "Check Legal" test12,
      TestLabel "Check Legal" test13,
      TestLabel "Check Legal" test14,
      TestLabel "Check Legal" test15,
      TestLabel "Check Legal" test16,
      TestLabel "Check Legal" test17,
      TestLabel "Check Legal" test18,
      TestLabel "Check Legal" test19,
      TestLabel "Check Legal" test20,
      TestLabel "Check Legal" test21,
      TestLabel "Locate King" test22,
      TestLabel "Locate King" test23,
      TestLabel "Check check" test24,
      TestLabel "Check check" test25,
      TestLabel "Check check" test26,
      TestLabel "Check check" test27,
      TestLabel "Check check" test28,
      TestLabel "Check check" test29,
      TestLabel "Check check" test30,
      TestLabel "Check check" test31,
      TestLabel "Check check" test32
    ]

mockBoard :: Board
mockBoard = newBoard

test1 :: Test
test1 = TestCase (assertEqual "Occupied by Correct Color" True (validateOwner ('a', 2) White mockBoard))

test2 :: Test
test2 = TestCase (assertEqual "Occupied by Wrong Color" False (validateOwner ('a', 2) Black mockBoard))

test3 :: Test
test3 = TestCase (assertEqual "Empty Location" False (validateOwner ('z', 90) White mockBoard && validateOwner ('z', 90) Black mockBoard))

test4 :: Test
test4 = TestCase (assertEqual "Out of Bounds" True (checkInbounds ('a', 2)))

test5 :: Test
test5 = TestCase (assertEqual "Check in bound" False (checkInbounds ('a', 9)))

test6 :: Test
test6 = TestCase (assertEqual "Check in bound" False (checkInbounds ('a', -10)))

test7 :: Test
test7 = TestCase (assertEqual "Check in bound" False (checkInbounds ('z', 2)))

test8 :: Test
test8 = TestCase (assertEqual "Legal Pawn forward move" True (checkLegal ('d', 2) ('d', 3) (Piece White Pawn True) mockBoard))

test9 :: Test
test9 = TestCase (assertEqual "Pawn illegal move" False (checkLegal ('d', 2) ('d', 5) (Piece White Pawn True) mockBoard))

test10 :: Test
test10 = TestCase (assertEqual "Knight leagal move" True (checkLegal ('b', 1) ('c', 3) (Piece White Knight True) mockBoard))

test11 :: Test
test11 = TestCase (assertEqual "Knight illegal move" False (checkLegal ('b', 1) ('b', 3) (Piece White Knight True) mockBoard))

test12 :: Test
test12 = TestCase (assertEqual "Rook vertical move" True (checkLegal ('d', 4) ('d', 7) (Piece White Rook True) mockBoard))

test13 :: Test
test13 = TestCase (assertEqual "Rook horizontal move" True (checkLegal ('f', 5) ('a', 5) (Piece White Rook True) mockBoard))

test14 :: Test
test14 = TestCase (assertEqual "Rook diagonal move" False (checkLegal ('f', 5) ('h', 7) (Piece White Rook True) mockBoard))

test15 :: Test
test15 = TestCase (assertEqual "Bishop diagonal move" True (checkLegal ('c', 1) ('f', 4) (Piece White Bishop True) mockBoard))

test16 :: Test
test16 = TestCase (assertEqual "Bishop straight move" False (checkLegal ('c', 1) ('c', 4) (Piece White Bishop True) mockBoard))

test17 :: Test
test17 = TestCase (assertEqual "Queen diagonal move" True (checkLegal ('d', 1) ('g', 4) (Piece White Queen True) mockBoard))

test18 :: Test
test18 = TestCase (assertEqual "Queen vertical move" True (checkLegal ('d', 1) ('d', 5) (Piece White Queen True) mockBoard))

test19 :: Test
test19 = TestCase (assertEqual "Queen horizontal move" True (checkLegal ('d', 1) ('a', 1) (Piece White Queen True) mockBoard))

test20 :: Test
test20 = TestCase (assertEqual "King one square move" True (checkLegal ('e', 1) ('e', 2) (Piece White King True) mockBoard))

test21 :: Test
test21 = TestCase (assertEqual "King two square move" False (checkLegal ('e', 1) ('e', 3) (Piece White King True) mockBoard))

test22 :: Test
test22 = TestCase (assertEqual "Locating White King" (Just ('e', 1)) (locateKing Black mockBoard))

test23 :: Test
test23 = TestCase (assertEqual "Locating Black King" (Just ('e', 8)) (locateKing White mockBoard))

test24 :: Test
test24 =
  let board = Map.fromList ([(('a', 1), Piece Black Rook True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("Check from Rook - Horizontal") True (isCheck board board Black))

test25 :: Test
test25 =
  let board = Map.fromList ([(('e', 7), Piece Black Rook True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("Check from Rook - Horizontal") True (isCheck board board Black))

test26 :: Test
test26 =
  let board = Map.fromList ([(('a', 5), Piece Black Rook True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("No Cheque from Rook") False (isCheck board board Black))

test27 :: Test
test27 =
  let board = Map.fromList ([(('a', 1), Piece Black Bishop True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("Check from Bishop") False (isCheck board board Black))

test28 :: Test
test28 =
  let board = Map.fromList ([(('a', 5), Piece Black Bishop True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("No Cheque from Rook") True (isCheck board board Black))

test29 :: Test
test29 =
  let board = Map.fromList ([(('d', 2), Piece Black Queen True), (('d', 5), Piece White King True)])
   in TestCase (assertEqual ("Check from Queen - vertical") True (isCheck board board Black))

test30 :: Test
test30 =
  let board = Map.fromList ([(('a', 5), Piece Black Queen True), (('e', 1), Piece White King True)])
   in TestCase (assertEqual ("Check from Queen - Diagonal") True (isCheck board board Black))

test31 :: Test
test31 =
  let board = Map.fromList ([(('g', 5), Piece Black Queen True), (('d', 5), Piece White King True)])
   in TestCase (assertEqual ("Check from Queen - Horizontal") True (isCheck board board Black))

test32 :: Test
test32 =
  let board = Map.fromList ([(('d', 5), Piece Black Queen True), (('f', 4), Piece White King True)])
   in TestCase (assertEqual ("No Cheque from Queen") False (isCheck board board Black))