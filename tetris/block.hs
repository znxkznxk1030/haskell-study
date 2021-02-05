{-# LANGUAGE RecordWildCards #-}
module Block (
  Shape(..),
  Block(..),
  genblock,
  getCoord,
  block2coords,
  randomShape,
  downward
) where
import System.Random
import Board (width, height)
import Data.Maybe

data Shape = I | L | J | O | S | Z | T deriving(Eq)
data Block = Block {shape :: Shape, rotate :: Int, coord :: (Int, Int)} deriving(Eq)


genblock :: Shape -> Block
genblock shape = Block shape 0 ( 7, 2)


randomShape :: IO Shape
randomShape = do
  rand <- randomRIO (0, 6) 
  return ([I, L, J, O, S, Z, T] !! rand)

getShape :: Block -> Shape
getShape Block{..} = shape

getRotate :: Block -> Int
getRotate Block{..} = rotate

getCoord :: Block -> (Int, Int)
getCoord Block{..} = coord

downward :: [(Int, Int)] -> [(Int, Int)]
downward = map (\(x, y) -> (x, y + 1))

block2coords :: Block -> [(Int, Int)]
block2coords block 
  | _shape == I && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == I && _rotate == 1 = [ (x - 1, y),   (x, y),   (x + 1, y),   (x + 1, y) ]

  | _shape == L && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x + 1, y + 1) ]
  | _shape == L && _rotate == 1 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == L && _rotate == 2 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == L && _rotate == 3 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]

  | _shape == J && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x - 1, y + 1) ]
  | _shape == J && _rotate == 1 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == J && _rotate == 2 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == J && _rotate == 3 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]

  | _shape == O && _rotate == 0 = [ (x, y - 1),   (x + 1, y - 1),   (x, y),   (x + 1, y) ]

  | _shape == S && _rotate == 0 = [ (x, y - 1),   (x + 1, y - 1),   (x, y),   (x - 1, y) ]
  | _shape == S && _rotate == 1 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]

  | _shape == Z && _rotate == 0 = [ (x - 1, y - 1),   (x, y - 1),   (x, y),   (x + 1, y) ]
  | _shape == Z && _rotate == 1 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]

  | _shape == T && _rotate == 0 = [ (x, y - 1),   (x, y),   (x - 1, y),   (x + 1, y) ]
  | _shape == T && _rotate == 1 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == T && _rotate == 2 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == T && _rotate == 3 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]

  | otherwise = []
  where _shape = shape block
        _rotate = rotate block
        (x, y) = coord block
