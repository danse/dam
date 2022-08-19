
import qualified Data.List as List
import qualified Dam

import Dam (Card)
import Control.Exception (try)
import Safe (atMay)
import Text.Read (readMaybe)
import System.Directory (listDirectory)
import Control.Monad (join)
import Data.Function ((&))
import Data.List.NonEmpty (nonEmpty, NonEmpty, toList)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

uniq :: Eq a => [a] -> [a]
uniq =
  let folding :: Eq a => [a] -> a -> [a]
      folding known new
        | new `elem` known = known
        | otherwise        = new:known
  in foldl folding []

readTags :: IO [String]
readTags = do
  paths <- listDirectory "."
  return (fmap words paths & join & filter visible & uniq & List.sortOn length)
  where visible :: String -> Bool
        visible "" = False
        visible (c:_) = c /= '.'

selectMultiple :: [String] -> [String] -> IO (Maybe [String])
selectMultiple options selected =
  let
    fromOptions :: String -> Maybe String
    fromOptions s = readMaybe s >>= atMay options
    selection s = fromMaybe s (fromOptions s)
    continue s = selectMultiple (List.delete s options) (s:selected)
  in do
    print (zip [0..] options :: [(Int, String)])
    putStrLn (List.intercalate " < " (reverse selected))
    userLine <- try getLine :: IO (Either IOError String)
    case userLine of
      Left _ -> return Nothing
      Right "" -> return (Just selected)
      Right s -> continue (selection s)
    
fileShowCard :: FilePath -> IO (Maybe (Card, [Card]))
fileShowCard p = do
  contents <- readFile p
  return (showCard contents)
  where showCard :: String -> Maybe (Card, [Card])
        showCard c = case Dam.parseDeck p c of
          Left _ -> Nothing
          Right d -> if null d then Nothing else Just (head d, tail d)

fileAppendCard :: NonEmpty String -> Card -> IO ()
fileAppendCard selection card =
  let path = unwords (toList selection)
  in appendFile path (Dam.showPadded card)

dispatch :: FilePath -> IO Int
dispatch path = do
  maybeDam <- fileShowCard path
  case maybeDam of
    Nothing -> do
      putStrLn ("no cards found in " ++ path)
      return 0
    Just (card, newDeck) -> do
      putStrLn (Dam.showCard card)
      tags <- readTags
      maybeSelection <- selectMultiple tags []
      case maybeSelection of
        Nothing -> return 0
        Just selection -> do
          let maybeNonEmptySelection = nonEmpty selection
              nonEmptySelection = fromMaybe (pure path) maybeNonEmptySelection
          writeFile path (Dam.showCards newDeck)
          fileAppendCard nonEmptySelection card
          p <- dispatch path
          return (1 + p)

main :: IO ()
main = do
  [source] <- getArgs
  done <- dispatch source
  putStrLn ("dispatched " <> show done <> " cards")
