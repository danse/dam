{-# LANGUAGE FlexibleContexts #-}
module DamFactor where

import Data.Foldable (fold)
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory, setCurrentDirectory)

import qualified Data.Set as Set
import qualified Data.Text as Text

import Dam

type Content = Text.Text
type Tags    = Set.Set String

cardToText :: Dam.Card -> Text.Text
cardToText = Text.unlines . fmap (Text.pack . Dam.getExpression) . Dam.getCard

pathToTags :: FilePath -> Tags
pathToTags = Set.fromList . Prelude.words

damToFactor :: (FilePath, [Dam.Card]) -> (Tags, [Content])
damToFactor (p, cc) = (pathToTags p, cardToText <$> cc)

partitionEithersSnd :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionEithersSnd [] = ([], [])
partitionEithersSnd ((a, e):rest) =
  let (l, r) = partitionEithersSnd rest
      onLeft  b = ((a, b):l, r)
      onRight c = (l, (a, c):r)
  in either onLeft onRight e

parseDirectory :: Bool -> FilePath -> IO (Maybe [(FilePath, [Dam.Card])])
parseDirectory verbose path = do
  pathsWithHidden <- listDirectory path
  let hidden ('.':_) = True
      hidden _       = False
      paths = filter (not . hidden) pathsWithHidden
  let d = if verbose then Dam.deckFromFileWithReport else Dam.deckFromFile
  results <- mapM d paths
  let (l, r) = partitionEithersSnd $ zip paths results
  if null l
    then pure $ Just r
    else do
      putStrLn "errors parsing files, canceling"
      pure Nothing

hasTag :: String -> [(Tags, b)] -> [(Tags, b)]
hasTag t = filter (elem t . fst)

allTags :: [(Tags, b)] -> Tags
allTags f = fold $ fst <$> f

tagGroupLength :: [(Tags, b)] -> String -> Int
tagGroupLength = flip (\ t -> length . hasTag t)

tagLength :: [(Tags, [b])] -> String -> Int
tagLength = flip (\ t -> contentLength . hasTag t)

contentLength :: [(a, [b])] -> Int
contentLength t = sum $ (length . snd) <$> t

tagsByLength :: [(Tags, [b])] -> [(String, Int)]
tagsByLength f = reverse $ sortOn snd $ zip a $ tagLength f <$> a
  where a = Set.toList $ allTags f

parseDirectoryTagsOrEmpty d = do
  setCurrentDirectory d
  c <- parseDirectory True "."
  pure $ damToFactor <$> fromMaybe [] c
