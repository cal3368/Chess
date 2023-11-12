import Data.Map.Strict qualified as Map

data Color = White | Black
  deriving (Eq, Enum, Bounded, Read, Show)

data Type = Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Eq, Enum, Bounded, Read, Show)

data Square = Piece | Empty
  deriving (Eq, Enum, Bounded, Read, Show)

data Piece = Color :@: Type
  deriving (Eq, Read, Show)

-- data Player = White | Black
--   deriving (Eq, Ord, Read, Show)

data Board where
  Board :: (Map.Map (Int, Int) Square) -> Board
  deriving (Eq, Read, Show)

new :: Board
new = Board (Map.fromList (newHelper 0 0))

newHelper :: Int -> Int -> [((Int, Int), Square)]
newHelper curm curn
  | curm + 1 == 8 && curn + 1 == 8 = [((curm, curn), Empty)]
  | curn + 1 == 8 = ((curm, curn), Empty) : newHelper (curm + 1) 0
  | otherwise = ((curm, curn), Empty) : newHelper curm (curn + 1)

drawHorizontal :: Int -> String
drawHorizontal 0 = "-\n"
drawHorizontal n = "-+" ++ drawHorizontal (n - 1)

draw :: Int -> Board -> String
draw cur_row (Board coor)
  | cur_row == 1 = drawVertical cur_row 7 (Map.toList coor) ++ "A B C D E F G H"
  | otherwise = drawVertical cur_row 7 (Map.toList coor) ++ drawHorizontal 7 ++ draw (pred cur_row) (Board (Map.drop 8 coor))

-- Need to change case for piece on square
getStringCoor :: ((Int, Int), Square) -> String
getStringCoor ((_, _), Empty) = " "
getStringCoor ((_, _), piece) = " "

test = draw 8 new

drawVertical :: Int -> Int -> [((Int, Int), Square)] -> String
drawVertical _ 0 (x : xs) = getStringCoor x ++ "\n"
drawVertical cur_row num (x : xs)
  | num == 7 = show cur_row ++ " " ++ getStringCoor x ++ "|" ++ drawVertical cur_row (num - 1) xs
  | otherwise = getStringCoor x ++ "|" ++ drawVertical cur_row (num - 1) xs