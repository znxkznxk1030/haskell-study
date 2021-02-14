module Board (
  Cell(..),
  Row,
  Board,
  initboard,
  showBoard,
  evalBoard,
  width,
  height
) where

import Data.List

-- Occupied to Colors
data Cell = Moving | Occupied | Wall | Celling | Floor | Empty deriving(Eq)
type Row = [Cell]
type Board = [Row]

instance Show Cell where
  show Moving = "□"
  show Occupied = "■"
  show Wall = "|"
  show Celling = "_"
  show Floor = "─"
  show Empty = " "

width :: Int
width = 15

height :: Int
height = 20

emptyRow :: Row
emptyRow = [Wall] ++ replicate width Empty ++ [Wall]

celling :: [Row]
celling = [replicate (width + 2) Celling]

ground :: [Row]
ground = [replicate (width + 2) Floor]

initboard :: Board
initboard = celling ++ (replicate height $ emptyRow) ++ ground

evalBoard :: Board -> (Int, Board)
evalBoard board = (count, nBoard)
          where full line = all (\block -> Occupied == block || Wall == block || Celling == block || Floor == block) line
                remained = filter (not.full) board
                count = height - length remained
                nBoard = celling ++ replicate count emptyRow ++ remained ++ ground


showRow :: Row -> String
showRow row = intercalate "" $ map show row

showBoard :: Board -> String
showBoard board = unlines $ map showRow board
