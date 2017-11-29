module Game where

import Data.List
import Control.Monad
import Data.Maybe

type Cell = (Int, Int)
type Board = [Cell]

-- The dimensions of our board.
width = 10
height = 10

-- All the adjacent points to a given point.
adjacentCells = [(-1,-1), (-1, 0), (0, -1), (1, 1), (1, 0), (0, 1), (1, -1), (-1, 1)]

-- neighbours (x, y) and an array of points and finds all the adjacent points to a given point.
neighbours :: Cell -> Board -> Board
neighbours (x, y) adjacentCells = [(x + a, y + b)  | (a, b) <- adjacentCells]

-- step board will take an initial board state and get the next step in Conway's game of life.
step :: Board -> Board
step board = filteredBoard
  where
    filteredBoard = [x | x <- newBoard, x /= (width, 0)]
      where
        newBoard = map (\ (a,b) -> checkCell (a,b) board) [(x,y) | x <- [0..width - 1], y <- [0..height - 1]]

-- checkCell will evaluate a certain cell and board against the rules of Conway's game of life and returns it as alive or dead
-- based on the rules.
checkCell :: Cell -> Board -> Cell
checkCell cell board = newCell
  where
    newCell = if (n == 3) || (n == 2 && cell `elem` board) then cell else (width, 0)
      where
        n = countNeighbours cell board

-- countNeighbours cell board will count the number of neighbours for a given cell.
countNeighbours :: Cell -> Board -> Int
countNeighbours cell board = length $ filter (\ x -> x `elem` board) $ neighbours cell adjacentCells
