import Dam

main = do
  contents <- getContents
  putStr ((onParsed countExpressions) contents)
