module Tetrish where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Data.Functor ((<$>))
import qualified Data.Sequence as Seq


-- Piece Type, Position, Orientation

data Piece
  = I
  | L
  | J
  | T
  | Z
  | S
  | O
  deriving (Eq, Show)

-- Board: 10 column, 20 rows
type Cell = Maybe Piece
type Row = Seq.Seq Cell
-- Indexes right and down
type Board = Seq.Seq Row
type Coord = (Int, Int)
type Orientation
  = Rotate0
  | Rotate90
  | Rotate180
  | Rotate270
  deriving (Eq, Ord, Enum, Show)
data Movement
  = RotateClockwise
  | RotateCounterClockwise
  | MoveLeft
  | MoveRight
  | MoveDown
  deriving (Eq, Show)
type ActivePiece = (Piece, Orientation, Coord)

data GameState = GameState { board :: Board
                           , activePiece :: ActivePiece
                           , nextPiece :: Piece
                           }

init :: GameState
init =
  GameState { board = emptyBoard
            , activePiece = (I, (5, 0), Rotate0)
            , nextPiece = Z
            }

emptyRow :: Row
emptyRow =
  Seq.replicate 10 Nothing

emptyBoard :: Board
emptyBoard =
  Seq.replicate 20 emptyRow

getCell :: Board -> Coord -> Maybe Piece
getCell board (row, col) =
  Seq.lookup row board >>= join . Seq.lookup col

setCell :: Board -> Coord -> Cell -> Board
setCell board (row, col) cell =
  case Seq.lookup row board of
    Nothing -> board
    Just original -> Seq.update row (Seq.update col cell original) board

clearRow :: Board -> Int -> Board
clearRow board row =
  emptyRow Seq.<| Seq.deleteAt row board

drawPieceOnBoard :: Board -> ActivePiece -> Maybe Board
--  |
--  |
--  |
--  |
drawPieceOnBoard board (I, Rotate0, (x, y)) =
  let
    getCellInBoard n = getCell board (x, y + n)
    cells = map getCellInBoard [0..3]
    filledInCell = foldr (<|>) Nothing cells
  in
    case filledInCell of
      Just _ -> Nothing
      Nothing -> Just ... drawBoard


  -- (<|>) ((<|>) <$> curry (getCell board) x y <*> curry (getCell board) x (y + 1)) <*> curry (getCell board) x (y + 2)
  -- (Seq.lookup x board >>= Seq.lookup y + 2)
  -- (Seq.lookup x board >>= Seq.lookup y + 3)
--  ----
drawPieceOnBoard board (I, Rotate90, coord) =
drawPieceOnBoard board (I, Rotate180, coord) = drawPieceOnBoard board (I, Rotate0, coord)
drawPieceOnBoard board (I, Rotate270, coord) = drawPieceOnBoard board (I, Rotate90, coord)


-- tick :: GameState -> Movement -> GameState
-- tick (GameState b a n) RotateClockwise =


{--
Movements of the ActivePiece
* Gravity
* Left
* Right
* Rotate Left
* Rotate Right
--}


main :: IO ()
main =
  putStrLn "Welcome to Tetrish"
