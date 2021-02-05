module Engine (
    rungame,
    startgame
) where

import Block
import Board
import Data.Maybe

import qualified System.Process as SP
import System.Console.ANSI


clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()


data State = STAY | MOVE | NEW | DONE
data GameState = GameState {board :: Board, state :: State, block :: Maybe Block}

render :: Board -> IO()
render board = do
    putStr $ showBoard board

implantCoord :: Cell -> Board -> (Int, Int) -> Board
implantCoord cell board (x, y) = top ++ [_mid_] ++ bottom
                        where 
                            (top, mid:bottom) = splitAt y board
                            (left, _mid: right) = splitAt x mid
                            _mid_ = left ++ [cell] ++ right


implantMovingBlock :: Block -> Board -> Board
implantMovingBlock block board = foldl (implantCoord Moving) board $ block2coords block

rootMovingBlock :: Board -> Board
rootMovingBlock = (map.map) root
                where
                    root cell = if cell == Moving then Occupied else cell

uprootMovingBlock :: Board -> Board
uprootMovingBlock = (map.map) uproot
                where
                    uproot cell = if cell == Moving then Empty else cell


collideAt :: Block -> Board -> Cell -> Bool
collideAt block board celltype = any (\coord@(x,y) -> board !! y !! x == celltype) $ block2coords block

collideAtWall :: Block -> Board -> Bool
collideAtWall block board = any (collideAt block board) [Wall, Floor]

startgame :: IO()
startgame = do
    let game = GameState initboard NEW Nothing 
    rungame game 0 
    return ()

rungame :: GameState -> Int -> IO()
rungame game nonce = do
    let pBoard = board game
        _Board = uprootMovingBlock pBoard
    setCursorPosition 0 0
    render $ board game
    case state game of
                NEW -> do
                    shape <- randomShape
                    let nBlock = genblock shape
                        nBoard = implantMovingBlock nBlock pBoard
                        nGameState = GameState nBoard STAY (Just nBlock)

                    if collideAt nBlock _Board Occupied 
                      then do
                        setCursorPosition 0 0
                        render $ rootMovingBlock nBoard
                        return ()
                      else rungame nGameState (nonce + 1)
                MOVE -> do
                    let pBlock = fromJust (block game)
                        nCoord@(nx, ny) = getCoord pBlock
                        nBlock = pBlock {coord = (nx, ny+1)}
                    if collideAtWall nBlock _Board || collideAt nBlock _Board Occupied 
                      then do
                        let nBoard = rootMovingBlock pBoard
                            nGameState = GameState nBoard NEW Nothing
                        rungame nGameState 0
                      else do
                        let nBoard = implantMovingBlock nBlock _Board
                            nGameState = GameState nBoard MOVE (Just nBlock)
                        rungame nGameState 0
                STAY -> 
                  if nonce < 20
                  then do
                    let nGameState = GameState pBoard STAY $ block game
                    rungame nGameState (nonce + 1)
                  else do
                    let nGameState = GameState pBoard MOVE $ block game
                    rungame nGameState 0
