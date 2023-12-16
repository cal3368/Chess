module Spec where

import Chess hiding (main)
import DrawBoard
import Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  return ()

tests :: Test
tests = TestList [TestLabel "Validate Owner" testValidateOwner]

mockBoard :: Board
mockBoard = newBoard

testValidateOwner = TestCase (assertEqual "Occupied by Correct Color" True (validateOwner ('a', 2) White mockBoard))
