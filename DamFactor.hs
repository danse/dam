{-# LANGUAGE FlexibleContexts #-}
module DamFactor where

import Data.Foldable (fold)
import Data.List (groupBy, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import System.Directory (listDirectory, setCurrentDirectory)

import qualified Data.Ord
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Dam
import Data.Hypergraph
import Lens.Micro

type Content = Text.Text
type Tag = String
type Tagged = Hypergraph Tag

cardToText :: Dam.Card -> Text.Text
cardToText = Text.unlines . fmap (Text.pack . Dam.getExpression) . Dam.getCard

pathToTags :: FilePath -> Set Tag
pathToTags = Set.fromList . Prelude.words

damToFactor :: (FilePath, [Dam.Card]) -> (Set Tag, Set Content)
damToFactor (p, cc) = (pathToTags p, Set.fromList $ cardToText <$> cc)

damToTagged :: [(FilePath, [Dam.Card])] -> Tagged Content
damToTagged = Hypergraph . Map.fromList . foldMap toNodes . fmap damToFactor
  where
    toNodes :: (Set Tag, Set Content) -> [(Content, Set Tag)]
    toNodes (tags, contents) = zip (Set.toList contents) (repeat tags)

partitionEithersSnd :: [(a, Either b c)] -> ([(a, b)], [(a, c)])
partitionEithersSnd [] = ([], [])
partitionEithersSnd ((a, e):rest) =
  let (l, r) = partitionEithersSnd rest
      onLeft  b = ((a, b):l, r)
      onRight c = (l, (a, c):r)
  in either onLeft onRight e

parseDirectory :: Bool -> FilePath -> IO [(FilePath, [Dam.Card])]
parseDirectory verbose path = do
  pathsWithHidden <- listDirectory path
  let hidden ('.':_) = True
      hidden _       = False
      paths = filter (not . hidden) pathsWithHidden
  let d = if verbose then Dam.deckFromFileWithReport else Dam.deckFromFile
  results <- mapM d paths
  let (l, r) = partitionEithersSnd $ zip paths results
  if null l
    then pure r
    else do
      putStrLn "errors parsing files, canceling"
      pure []

hasTag :: String -> [(Set Tag, b)] -> [(Set Tag, b)]
hasTag t = filter (elem t . fst)

allTags :: [(Set Tag, b)] -> Set Tag
allTags f = fold $ fst <$> f

tagGroupLength :: [(Set Tag, b)] -> String -> Int
tagGroupLength = flip (\ t -> length . hasTag t)

tagLength :: [(Set Tag, [b])] -> String -> Int
tagLength = flip (\ t -> contentLength . hasTag t)

contentLength :: [(a, [b])] -> Int
contentLength t = sum $ length . snd <$> t

tagsByLength :: [(Set Tag, [b])] -> [(String, Int)]
tagsByLength f = sortOn (Data.Ord.Down . snd) (zip a $ tagLength f <$> a)
  where a = Set.toList $ allTags f

parseDirectoryTagsOrEmpty d = do
  setCurrentDirectory d
  c <- parseDirectory True "."
  pure $ damToFactor <$> c

factor :: Tagged Content -> [(Set Tag, Set Content)]
factor = edgeSets
