import Dam
import System.Environment (getArgs)

main = do
  (fileName:noNewLine) <- getArgs
  let p = if null noNewLine then putStrLn else (\ s -> putStr (s <> " "))
  doParsed (p . show . length) fileName
