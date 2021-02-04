import System.IO
import System.Directory
import Data.List
import Control.Exception


main = do
    handle <- openFile "todo.txt" ReadMode
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - "++ line) [0..] todoTasks
    putStrLn $ unlines numberedTasks
    putStrLn $ "which task do you remove? :"
    number <- getLine
    let newTodoTasks = unlines $ delete (todoTasks !! read number) todoTasks
    bracketOnError
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newTodoTasks
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"
