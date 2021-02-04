{-# LANGUAGE RecordWildCards #-}
module Block (
  Shape(..),
  Block(..),
  genblock,
  getCoord,
  block2coords,
  downward
) where

data Shape = I | L | J | O | S | Z | T deriving(Eq)
data Block = Block {shape :: Shape, rotate :: Int, coord :: (Int, Int)} | NONE deriving(Eq)


genblock :: Block
genblock = Block I 0 (5, 2)

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
  | shape == I && rotate == 0 = [ (x, y - 1),   (x, y),   (x, y + 1),   (x, y + 2) ]
  | shape == I && rotate == 1 = [ (x - 1, y),   (x, y),   (x + 1, y),   (x + 1, y) ]
  | otherwise = []
  where shape = getShape block
        rotate = getRotate block
        (x, y) = getCoord block
