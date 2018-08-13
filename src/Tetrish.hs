module Tetrish where

import           Control.Monad                  ( join )
import qualified Data.Sequence                 as Seq


-- Tetromino Type, Position, Orientation

data Tetromino
  = I
  | L
  | J
  | T
  | Z
  | S
  | O
  deriving (Eq, Show)

-- Board: 10 column, 20 rows
type Cell = Maybe Tetromino
type Row = Seq.Seq Cell
-- Indexes right and down
type Board = Seq.Seq Row
type Coord = (Int, Int)
type Orientation = Rotate0 | Rotate90 | Rotate180 | Rotate270 
                   deriving (Eq, Ord, Enum, Show)
type ActiveTetromino = (Tetromino, Coord, Orientation)

data GameState = GameState { board :: Board
                           , activeTetromino :: ActiveTetromino
                           , nextTetromino :: Tetromino
                           , score :: Int
                           , level :: Int
                           }

init :: GameState
init = 
  GameState { board = emptyBoard
            , activeTetromino = (I, (5, 0), Rotate0)
            , nextTetromino = Z
            , score = 0
            , level = 0
            }

emptyRow :: Row
emptyRow = Seq.replicate 10 Nothing

emptyBoard :: Board
emptyBoard = Seq.replicate 20 emptyRow

getCell :: Coord -> Board -> Maybe Tetromino
getCell (row, col) board = Seq.lookup row board >>= join . Seq.lookup col

setCell :: Coord -> Cell -> Board -> Board
setCell (row, col) cell board = case Seq.lookup row board of
  Nothing       -> board
  Just original -> Seq.update row (Seq.update col cell original) board

clearRow :: Int -> Board -> Board
clearRow row board = emptyRow Seq.<| Seq.deleteAt row board

{--
Movements of the ActiveTetromino
* Gravity
* Left
* Right
* Rotate Left
* Rotate Right
--}


main :: IO ()
main = putStrLn "Welcome to Tetrish"
