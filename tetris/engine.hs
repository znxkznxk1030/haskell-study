module Engine (
    rungame,
    startgame
) where

import Block
import Board

data State = MOVE | NEW | DONE
data GameState = GameState {board :: Board, state :: State, block :: Block}

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

uprootMovingBlock :: Board -> Board
uprootMovingBlock = (map.map) uproot
                where
                    uproot cell = if cell == Moving then Empty else cell

startgame :: IO()
startgame = do
    let game = GameState initboard NEW NONE
    rungame game
    return ()


rungame :: GameState -> IO()
rungame game = do
    render $ board game
    case state game of
                NEW -> do
                    let pBoard = board game
                        nBlock = genblock
                        nBoard = implantMovingBlock nBlock pBoard
                        nGameState = GameState nBoard MOVE nBlock
                    render $ nBoard
                    rungame nGameState
                MOVE -> do
                    let pBoard = board game
                        pBlock = block game
                        nCoord@(nx, ny) = getCoord pBlock
                        nBlock = pBlock {coord = (nx, ny+1)}
                        nBoard = implantMovingBlock nBlock $ uprootMovingBlock pBoard
                        nGameState = GameState nBoard MOVE nBlock
                    render $ nBoard
                    if ny > 30 then return () else rungame nGameState

    -- -- initial
    -- let state1 = board game
    -- render state1

    -- -- new
    -- let nb = genblock 
    --     state2 = implantMovingBlock nb state1
    -- render state2
    
    -- -- gravity
    -- let state3 = uprootMovingBlock state2
    -- let nCoord@(nx, ny) = getCoord nb
    --     state4 = implantMovingBlock nb {coord = (nx, ny+1)} state3
    -- render state4