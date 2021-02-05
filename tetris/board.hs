module Board (
  Cell(..),
  Row,
  Board,
  initboard,
  showBoard,
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

initboard :: Board
initboard = [replicate (width + 2) Celling] ++ (replicate height  $ ([Wall] ++ replicate width Empty ++ [Wall])) ++ [replicate (width + 2) Floor]

showRow :: Row -> String
showRow row = intercalate "" $ map show row

showBoard :: Board -> String
showBoard board = unlines $ map showRow board
