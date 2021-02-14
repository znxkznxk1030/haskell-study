module Engine
  ( rungame,
    startgame,
  )
where

import Block
  ( Block (coord),
    block2coords,
    genblock,
    getCoord,
    randomShape,
  )
import Board
  ( Board,
    Cell (Empty, Floor, Moving, Occupied, Wall),
    initboard,
    showBoard,
  )
import Data.Maybe (fromJust)
import System.Console.ANSI (setCursorPosition)
import qualified System.Process as SP
import UI.NCurses

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()

data State = IDLE | MOVE | NEW | DONE

data GameState = GameState {board :: Board, state :: State, block :: Maybe Block}

startgame :: IO ()
startgame = runCurses $ do
  w <- defaultWindow
  let game = GameState initboard NEW Nothing
  rungame w game 0

rungame :: Window -> GameState -> Int -> Curses ()
rungame w game nonce = do
  let pBoard = board game
      _Board = uprootMovingBlock pBoard
  -- setCursorPosition 0 0
  updateWindow w $ renderBoard $ board game
  render
  case state game of
    NEW -> makeNewBlock w game nonce
    MOVE -> moveCurrBlock w game nonce
    IDLE -> idleCurrState w game nonce

renderBoard :: Board -> Update ()
renderBoard board = do
  moveCursor 0 0
  drawString $ showBoard board

implantCoord :: Cell -> Board -> (Int, Int) -> Board
implantCoord cell board (x, y) = top ++ [_mid_] ++ bottom
  where
    (top, mid : bottom) = splitAt y board
    (left, _mid : right) = splitAt x mid
    _mid_ = left ++ [cell] ++ right

implantMovingBlock :: Block -> Board -> Board
implantMovingBlock block board = foldl (implantCoord Moving) board $ block2coords block

rootMovingBlock :: Board -> Board
rootMovingBlock = (map . map) root
  where
    root cell = if cell == Moving then Occupied else cell

uprootMovingBlock :: Board -> Board
uprootMovingBlock = (map . map) uproot
  where
    uproot cell = if cell == Moving then Empty else cell

collideAt :: Block -> Board -> Cell -> Bool
collideAt block board celltype = any (\coord@(x, y) -> board !! y !! x == celltype) $ block2coords block

collideAtWall :: Block -> Board -> Bool
collideAtWall block board = any (collideAt block board) [Wall, Floor]

makeNewBlock :: Window -> GameState  -> Int -> Curses ()
makeNewBlock w game nonce = do
  shape <- randomShape
  let pBoard = board game
      _Board = uprootMovingBlock pBoard
      nBlock = genblock shape
      nBoard = implantMovingBlock nBlock pBoard
      nGameState = GameState nBoard IDLE (Just nBlock)

  if collideAt nBlock _Board Occupied
    then do
      -- setCursorPosition 0 0
      updateWindow w $ renderBoard $ rootMovingBlock nBoard
      return ()
    else rungame w nGameState (nonce + 1)

moveCurrBlock :: Window ->  GameState -> Int -> Curses ()
moveCurrBlock w game nonce = do
      let pBoard = board game
          pBlock = fromJust (block game)
          _Board = uprootMovingBlock pBoard
          nCoord@(nx, ny) = getCoord pBlock
          nBlock = pBlock {coord = (nx, ny + 1)}
      if collideAtWall nBlock _Board || collideAt nBlock _Board Occupied
        then do
          let nBoard = rootMovingBlock pBoard
              nGameState = GameState nBoard NEW Nothing
          rungame w nGameState 0
        else do
          let nBoard = implantMovingBlock nBlock _Board
              nGameState = GameState nBoard IDLE (Just nBlock)
          rungame w nGameState 0 

idleCurrState :: Window -> GameState -> Int -> Curses()
idleCurrState w game nonce = do
        let pBoard = board game
        if nonce < 1000
        then do
          let nGameState = GameState pBoard IDLE $ block game
          rungame w nGameState (nonce + 1)
        else do
          let nGameState = GameState pBoard MOVE $ block game
          rungame w nGameState 0