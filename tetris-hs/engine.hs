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
    rotateBlock
  )
import Board
  ( Board,
    Cell (Empty, Floor, Moving, Occupied, Wall),
    initboard,
    showBoard,
    evalBoard,
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
  setEcho False
  let game = GameState initboard NEW Nothing
  rungame w game 0

rungame :: Window -> GameState -> Int -> Curses ()
rungame w game nonce = do
  let pBoard = board game
      _Board = uprootMovingBlock pBoard
      (count, eBoard) = evalBoard pBoard
      ngame = game {board = eBoard}
  -- setCursorPosition 0 0
  updateWindow w $ renderBoard $ board game
  render

  case state ngame of
    NEW -> makeNewBlock w ngame nonce
    MOVE -> moveCurrBlock w ngame nonce
    IDLE -> idleCurrState w ngame nonce
    DONE -> do
        updateWindow w $ drawString "done"
        render
        closeWindow w

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

makeNewBlock :: Window -> GameState -> Int -> Curses ()
makeNewBlock w game nonce = do
  shape <- randomShape
  let pBlock = fromJust (block game)
      pBoard = board game
      _Board = uprootMovingBlock pBoard
      nBlock = genblock shape
      nBoard = implantMovingBlock nBlock pBoard
      nGameState = GameState nBoard IDLE (Just nBlock)

  if collideAt nBlock _Board Occupied
    then do
      rungame w (game {state  = DONE, board = nBoard}) nonce
    else rungame w nGameState (nonce + 1)

moveCurrBlock :: Window -> GameState -> Int -> Curses ()
moveCurrBlock w game nonce = do
  let pBoard = board game
      pBlock = fromJust (block game)
      _Board = uprootMovingBlock pBoard
      nCoord@(nx, ny) = getCoord pBlock
      nBlock = pBlock {coord = (nx, ny + 1)}
  if collideAt nBlock _Board Floor || collideAt nBlock _Board Occupied
    then do
      let nBoard = rootMovingBlock pBoard
          nGameState = GameState nBoard NEW Nothing
      rungame w nGameState 0
    else do
      let nBoard = implantMovingBlock nBlock _Board
          nGameState = GameState nBoard IDLE (Just nBlock)
      rungame w nGameState 0

idleCurrState :: Window -> GameState -> Int -> Curses ()
idleCurrState w game nonce = do
  let pBoard = board game
      pBlock = fromJust (block game)
      _Board = uprootMovingBlock pBoard
      nBlockInfo@(nx, ny) = getCoord pBlock
  if nonce >= 1000
    then do
      let nGameState = GameState pBoard MOVE $ block game
      rungame w nGameState 0
    else do
      let nGameState = GameState pBoard IDLE $ block game
      ev <- getEvent w $ Just 0
      case ev of
        Nothing -> rungame w nGameState (nonce + 1)
        Just ev'
            | ev' == EventSpecialKey KeyLeftArrow -> do
                                                  let nBlock = pBlock {coord = (nx - 1, ny)}
                                                      nBoard = implantMovingBlock nBlock _Board
                                                      nGameState = GameState nBoard IDLE (Just nBlock)
                                                  if collideAt nBlock _Board Wall
                                                    then rungame w game (nonce + 1)
                                                    else rungame w nGameState (nonce + 1)
            | ev' == EventSpecialKey KeyRightArrow -> do
                                                  let nBlock = pBlock {coord = (nx + 1, ny)}
                                                      nBoard = implantMovingBlock nBlock _Board
                                                      nGameState = GameState nBoard IDLE (Just nBlock)
                                                  if collideAt nBlock _Board Wall
                                                    then rungame w game (nonce + 1)
                                                    else rungame w nGameState (nonce + 1)
            | ev' == EventSpecialKey KeyUpArrow -> do
                                                let nBlock = rotateBlock pBlock
                                                    nBoard = implantMovingBlock nBlock _Board
                                                    nGameState = GameState nBoard IDLE (Just nBlock)
                                                if collideAt nBlock _Board Wall || collideAt nBlock _Board Occupied
                                                    then rungame w game (nonce + 1)
                                                    else rungame w nGameState (nonce + 1)
            | ev' == EventSpecialKey KeyDownArrow -> do
                                                let nGameState = GameState pBoard MOVE $ block game
                                                rungame w nGameState 0
            | otherwise -> rungame w nGameState (nonce + 1)
      rungame w nGameState (nonce + 1)

--  ev' == EventSpecialKey KeyDownArrow -> updateScreen (speedUp state) newScore gen' lvl newHighScores newUpd
--  ev' == EventSpecialKey KeyUpArrow -> updateScreen (rotate state) newScore gen' lvl newHighScores newUpd

