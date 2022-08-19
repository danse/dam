{-# LANGUAGE FlexibleContexts #-}
module Dam where

import Text.Parsec
import Data.List
import Control.Monad
import Data.Either

import Data.Foldable (fold)

newtype Expression = Expression { getExpression :: String } deriving (Eq, Show)
newtype Card       = Card       { getCard :: [Expression] } deriving (Eq, Show)

sep :: String
sep = "\n"

showCard :: Card -> String
showCard (Card c) = intercalate sep $ map getExpression c

showPadded :: Card -> String
showPadded c = sep <> sep <> showCard c <> sep <> sep

showCards :: [Card] -> String
showCards n = intercalate (sep ++ sep) (map showCard n) ++ sep

cardSizeChars :: Card -> Int
cardSizeChars (Card e) = length (filter (/=' ') (join (map getExpression e)))

sortBySizeChars :: [Card] -> [Card]
sortBySizeChars = sortOn cardSizeChars

sortByLength :: [Card] -> [Card]
sortByLength =
  let cardLength = length . getCard
  in sortOn cardLength

addExpressions :: Card -> [Expression] -> Card
addExpressions (Card e1) e2 = Card (e1 ++ e2)

countExpressions :: [Card] -> [Card]
countExpressions = map c
  where c (Card e) = Card [Expression $ show $ length e]

line :: Stream s m Char => ParsecT s u m Expression
line = fmap Expression $ many1 $ noneOf sep

card :: Stream s m Char => ParsecT s u m Card
card = fmap Card $ line `endBy` string sep

deck :: Stream s m Char => ParsecT s u m [Card]
deck = card `sepBy` many1 (string sep)

parseDeck :: String -> String -> Either ParseError [Card]
parseDeck = parse deck

deckFromFile :: FilePath -> IO (Either ParseError [Card])
deckFromFile path = parseDeck path <$> readFile path

deckFromFileWithReport :: FilePath -> IO (Either ParseError [Card])
deckFromFileWithReport p = do
  e <- deckFromFile p
  let onLeft error = "parsing error: " <> show error
      onRight cards = "parsed " <> show (length cards) <> " cards"
  putStr $ p <> " "
  putStrLn $ either onLeft onRight e
  return e

onParsed :: ([Card] -> [Card]) -> String -> String
onParsed f = either show (showCards . f) . parseDeck "standard input"

doParsed :: ([Card] -> IO ()) -> FilePath -> IO ()
doParsed op pa = do
  contents <- readFile pa
  case parseDeck pa contents of
    Left error -> print error
    Right deck -> op deck
