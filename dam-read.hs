import Text.Parsec
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class (lift)
import Dam

-- the following are for the reader
import System.Directory
import Control.Monad (join)

readDeckFile :: FilePath -> ExceptT ParseError IO [Card]
readDeckFile path = do
  c <- lift (readFile path)
  except (parseDeck path c)

defaultEmpty :: ExceptT ParseError IO [Card] -> IO [Card]
defaultEmpty ex = do
  ei <- runExceptT ex
  return (either (const []) id ei)

printDeckFile :: FilePath -> IO ()
printDeckFile path = do
  deck <- defaultEmpty (readDeckFile path)
  putStr (showCards deck)

main = return ()

