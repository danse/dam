
import Dam

import System.Environment (getArgs)

cycle :: FilePath -> [Card] -> IO ()
cycle _ [] = print "No cards"
cycle path (c:cs) = do
  putStrLn $ showCard c
  let rotated = cs <> [c]
  writeFile path (Dam.showCards rotated)

main :: IO ()
main = do
  [path] <- getArgs
  doParsed (Main.cycle path) path
