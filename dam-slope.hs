import Dam

profile = countExpressions . sortBySizeChars

main = do
  contents <- getContents
  putStr (onParsed profile contents)
