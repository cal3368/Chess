module Chess1
  ( Color (..),
    Type (..),
    Square,
    Location,
    Piece,
  )
where

import Data.Array

data Color = White | Black deriving (Show, Eq)

data Type = Pawn | Rook | Knight | Bishop | Queen | King deriving (Show, Eq)

data Square = Square Piece | Empty

type Location = (Char, Int)

data Piece = Piece Color Type deriving (Show, Eq)
