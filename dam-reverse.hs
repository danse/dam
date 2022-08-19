import Dam

main = do
  contents <- getContents
  putStr (onParsed reverse contents)
