import Debug.Trace

type Row   = [Slot]
type Board = [Row]

data Slot = Red
          | Blue
          | Blank
          | Invalid
            deriving (Eq, Show)

data Direction = Up
               | Right
               | Down
               | Left
                 deriving (Eq, Show)

data Coords x y = Coords x y
                  deriving (Show)


startingBoard = [ [ Red,     Red,     Red,   Invalid, Invalid],
                  [ Red,     Red,     Red,   Invalid, Invalid],
                  [ Red,     Red,     Blank, Blue,    Blue],
                  [ Invalid, Invalid, Blue,  Blue,    Blue],
                  [ Invalid, Invalid, Blue,  Blue,    Blue]]

solvedBoard = [ [ Blue,    Blue,    Blue,  Invalid, Invalid],
                [ Blue,    Blue,    Blue,  Invalid, Invalid],
                [ Blue,    Blue,    Blank, Red,     Red],
                [ Invalid, Invalid, Red,   Red,     Red],
                [ Invalid, Invalid, Red,   Red,     Red]]

height = 5
width  = 5

isSolved = and . zipWith (==) solvedBoard

coordAt (Coords x y) dir spaces
  | dir == Main.Up    = (Coords x (y - spaces))
  | dir == Main.Down  = (Coords x (y + spaces))
  | dir == Main.Left  = (Coords (x - spaces) y)
  | dir == Main.Right = (Coords (x + spaces) y)


-- Gets the Slot value at a certain coord on the board
--slotAt (Coords x y) board = last (take (x+1) (last (take (y+1) board)))
slotAt (Coords x y) board =  board !! y !! x --last (take (x+1) (last (take (y+1) board)))

printBoard :: Board -> IO ()
printBoard board = putStrLn $ boardToString board

boardToString :: Board -> String
boardToString board = foldl (++) "" (map (\row -> rowToString row) board)

rowToString :: Row -> String
rowToString row = foldl (++) "" (map (\slot -> slotToString slot) row) ++ ['\n']

slotToString Red   = "X "
slotToString Blue  = "O "
slotToString Blank = "_ "
slotToString _     = "  "


-- Stolen from haskell chess solver
-- updateList :: Row -> Int -> (Slot -> Slot) -> Row
updateList []     _     _ = []
updateList (x:xs) 0     f = (f x):xs
updateList (x:xs) index f = x:updateList xs (index-1) f

updateMatrix :: (Coords Int Int) -> Slot -> Board -> Board
updateMatrix (Coords x y) newValue board = updateList board y (\z->updateList z x (const newValue)) 
-- end stolen code

possibleMoves = [(direction, distance) | direction <- [Main.Up, Main.Down, Main.Right, Main.Left], distance <- [1,2]]

possibleMovesTo blank = map (\(dir, dist) -> coordAt blank dir dist) possibleMoves

helper item valid list
  | valid     = item:list
  | otherwise = list

otherHelper board x from = helper from (isValidMove piece from blank board) x
                           where blank = findOpenSlot board
                                 piece = slotAt from board

-- almost there
possibleValidMoves board = foldl (otherHelper board) [] (possibleMovesTo $ findOpenSlot board)

findOpenSlot = openSlot (Coords 0 0)

openSlot (Coords x y) board
  | x == width                         = openSlot (Coords 0 (y+1)) board
  | slotAt (Coords x y) board == Blank = (Coords x y)
  | otherwise                          = openSlot (Coords (x+1) y) board

-- Map over 1 and 2 spots away, and map over all directions, and map over both colors. This should get all possible moves to Coords
makeMoves board = map (\from -> move from to board) moves
                  where moves = possibleValidMoves board
                        to    = findOpenSlot board

move from to board = updateMatrix to fromPiece (updateMatrix from toPiece board)
                     where fromPiece = slotAt from board
                           toPiece   = slotAt to board

solve (x:xs)
  | isSolved x = x
  | otherwise  = solve $ xs ++ makeMoves x
              

isOnBoard (Coords x y) = (x >= 0 && x < width) && (y >= 0 && y < height)

directionOfMove (Coords fromX fromY) (Coords toX toY)
  | fromX < toX = Main.Right
  | fromX > toX = Main.Left
  | fromY < toY = Main.Down
  | fromY > toY = Main.Up

coordBetween (Coords fromX fromY) (Coords toX toY) = (Coords ((fromX - toX) `div` 2 + toX) ((fromY - toY) `div` 2 + toY))

isJump (Coords fromX fromY) (Coords toX toY) = abs (fromX - toX) > 1 || abs (fromY - toY) > 1

oppositePiece Red  = Blue
oppositePiece Blue = Red

-- Assumes that to == Blank and from == Red || Blue
-- and from and to are 0 or 1 spaces apart
canMoveFromCoord piece from to board
  | isJump from to = slotAt (coordBetween from to) board == oppositePiece piece
  | otherwise      = True

isValidMove piece from to board = isOnBoard from && isOnBoard to && isValidDirection piece from to && canMoveFromCoord piece from to board

isValidDirection Red  from to = move == Main.Right || move == Main.Down
                                where move = directionOfMove from to
isValidDirection Blue from to = move == Main.Left || move == Main.Up
                                where move = directionOfMove from to
isValidDirection _    _    _  = False
