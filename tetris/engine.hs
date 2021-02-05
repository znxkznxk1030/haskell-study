module Engine (
    rungame,
    startgame
) where

import Block
import Board
import Data.Maybe

data State = MOVE | NEW | DONE
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
    rungame game
    return ()


rungame :: GameState -> IO()
rungame game = do
    let pBoard = board game
        _Board = uprootMovingBlock pBoard
    render $ board game
    case state game of
                NEW -> do
                    shape <- randomShape
                    let nBlock = genblock shape
                        nBoard = implantMovingBlock nBlock pBoard
                        nGameState = GameState nBoard MOVE (Just nBlock)
                    -- render nBoard

                    if collideAt nBlock _Board Occupied 
                      then do
                        render $ rootMovingBlock pBoard
                        return ()
                      else rungame nGameState
                MOVE -> do
                    let pBlock = fromJust (block game)
                        nCoord@(nx, ny) = getCoord pBlock
                        nBlock = pBlock {coord = (nx, ny+1)}
                    -- render nBoard
                    if collideAtWall nBlock _Board || collideAt nBlock _Board Occupied 
                      then do
                        putStrLn "Colide"
                        let nBoard = rootMovingBlock pBoard
                            nGameState = GameState nBoard NEW Nothing
                        rungame nGameState
                      else do
                        putStrLn "Moving"
                        let nBoard = implantMovingBlock nBlock _Board
                            nGameState = GameState nBoard MOVE (Just nBlock)
                        rungame nGameState
