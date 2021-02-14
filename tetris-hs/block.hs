{-# LANGUAGE RecordWildCards #-}
module Block (
  Shape(..),
  Block(..),
  genblock,
  getCoord,
  block2coords,
  randomShape,
  downward,
  rotateBlock
) where
import System.Random
import Board (width, height)
import Data.Maybe
import UI.NCurses

data Shape = I | L | J | O | S | Z | T deriving(Eq)
data Block = Block {shape :: Shape, rotate :: Int, coord :: (Int, Int)} deriving(Eq)


genblock :: Shape -> Block
genblock shape = Block shape 0 ( 7, 2)


randomShape :: Curses Shape
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

rotateBlock :: Block -> Block
rotateBlock block
  | _shape == I = block { rotate = (_rotate + 1) `mod` 2 }
  | _shape == L = block { rotate = (_rotate + 1) `mod` 4 }
  | _shape == J = block { rotate = (_rotate + 1) `mod` 4 }
  | _shape == O = block { rotate = (_rotate + 1) `mod` 1 }
  | _shape == S = block { rotate = (_rotate + 1) `mod` 2 }
  | _shape == Z = block { rotate = (_rotate + 1) `mod` 2 }
  | _shape == T = block { rotate = (_rotate + 1) `mod` 4 }
  where _shape = shape block
        _rotate = rotate block

block2coords :: Block -> [(Int, Int)]
block2coords block 
  | _shape == I && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | _shape == I && _rotate == 1 = [ (x - 1, y),   (x, y),   (x + 1, y),   (x + 2, y) ]

  | _shape == L && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x + 1, y + 1) ]
  | _shape == L && _rotate == 1 = [ (x - 1, y),   (x, y),   (x + 1, y),   (x + 1, y - 1) ]
  | _shape == L && _rotate == 2 = [ (x - 1, y - 1),   (x, y - 1),   (x, y),   (x, y + 1) ]
  | _shape == L && _rotate == 3 = [ (x - 1, y + 1),   (x - 1, y),   (x, y),   (x + 1, y) ]

  | _shape == J && _rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x - 1, y + 1) ]
  | _shape == J && _rotate == 1 = [ (x - 1, y),   (x, y),   (x + 1, y),   (x + 1, y + 1) ]
  | _shape == J && _rotate == 2 = [ (x + 1, y - 1),   (x, y - 1),   (x, y),   (x, y + 1) ]
  | _shape == J && _rotate == 3 = [ (x - 1, y - 1),   (x - 1, y),   (x, y),   (x + 1, y) ]

  | _shape == O && _rotate == 0 = [ (x, y - 1),   (x + 1, y - 1),   (x, y),   (x + 1, y) ]

  | _shape == S && _rotate == 0 = [ (x, y - 1),   (x + 1, y - 1),   (x, y),   (x - 1, y) ]
  | _shape == S && _rotate == 1 = [ (x, y - 1),   (x, y),   (x + 1, y),   (x + 1, y + 1) ]

  | _shape == Z && _rotate == 0 = [ (x - 1, y - 1),   (x, y - 1),   (x, y),   (x + 1, y) ]
  | _shape == Z && _rotate == 1 = [ (x + 1, y - 1),   (x + 1, y),   (x, y),   (x, y + 1) ]

  | _shape == T && _rotate == 0 = [ (x, y - 1),   (x, y),   (x - 1, y),   (x + 1, y) ]
  | _shape == T && _rotate == 1 = [ (x, y - 1),   (x, y),   (x + 1, y),   (x, y + 1) ]
  | _shape == T && _rotate == 2 = [ (x, y - 1),   (x - 1, y),   (x, y),   (x + 1, y) ]
  | _shape == T && _rotate == 3 = [ (x - 1, y),   (x, y),   (x, y - 1),   (x, y + 1) ]

  | otherwise = []
  where _shape = shape block
        _rotate = rotate block
        (x, y) = coord block
