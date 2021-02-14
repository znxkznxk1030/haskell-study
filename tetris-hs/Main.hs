import Engine
import qualified System.Process as SP

clearScreen :: IO ()
clearScreen = do
  _ <- SP.system "reset"
  return ()

main = do
  clearScreen
  startgame
