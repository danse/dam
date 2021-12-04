import Dam
import System.Environment (getArgs)

maybeReverse ["-r"] = reverse
maybeReverse _      = id

main = do
  args <- getArgs
  contents <- getContents
  putStr ((onParsed (maybeReverse args . sortBySizeChars)) contents)
