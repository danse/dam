
import qualified Data.List as List
import qualified Dam
import qualified Seline

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

readTags :: IO [String]
readTags = do
  paths <- listDirectory "."
  return (paths >>= words & filter visible & List.nub & List.sortOn length)
  where visible :: String -> Bool
        visible "" = False
        visible (c:_) = c /= '.'

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
  parsed <- fileShowCard path
  maybe noCards dispatchToNewDeck parsed
  where
    noCards = do
      putStrLn ("no cards found in " ++ path)
      return 0
    dispatchToNewDeck (card, newDeck) = do
      putStrLn (Dam.showCard card)
      tags <- readTags
      selection <- Seline.seline Nothing tags []
      maybe (return 0) (dispatchRepeat card newDeck) selection
    dispatchRepeat card newDeck selection = do
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
