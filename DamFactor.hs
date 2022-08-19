{-# LANGUAGE FlexibleContexts #-}
module DamFactor where

import Data.Foldable (fold)
import Data.List (groupBy, sortBy, sortOn)
import Data.Maybe (fromMaybe)
import System.Directory (listDirectory, setCurrentDirectory)

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Dam
import Lens.Micro

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
  pure $ damToFactor <$> c

{-

We want to find the set of tags shared between a set of contents.

In @factor@, first the contents are separated from each other,
then for every content we collect an union of tags.
Then we group the different tag sets.

-}

-- | @g@
-- >>> g [(1, "some"), (2, "strings")]
-- [(1,'s'),(1,'o'),(1,'m'),(1,'e'),(2,'s'),(2,'t'),(2,'r'),(2,'i'),(2,'n'),(2,'g'),(2,'s')]
g :: [(a, [b])] -> [(a, b)]
g = concatMap (\(a, bb) -> zip (repeat a) bb)

-- | @p@
-- >>> p [(1, 1), (2, 2), (2, 1)]
-- [[(1,1),(2,1)],[(2,2)]]
p :: (Ord b, Eq b) => [(a, b)] -> [[(a, b)]]
p = let equalOn a b c = a b == a c
        compareOn a b c = compare (a b) (a c)
    in groupBy (equalOn snd) . sortBy (compareOn snd)

-- | @byContents@
--
-- >>> :set -XOverloadedStrings
-- >>> p $ g [(Set.fromList ["a"],["a\nb\n","c\n","d\n"]),(Set.fromList ["b"],["a\nb\n","d\n","e\n"])]
-- [[(fromList ["a"],"a\nb\n"),(fromList ["b"],"a\nb\n")],[(fromList ["a"],"c\n")],[(fromList ["a"],"d\n"),(fromList ["b"],"d\n")],[(fromList ["b"],"e\n")]]
-- >>> byContents [(Set.fromList ["a"],["a\nb\n","c\n","d\n"]),(Set.fromList ["b"],["a\nb\n","d\n","e\n"])]
-- [(fromList ["a","b"],"a\nb\n"),(fromList ["a"],"c\n"),(fromList ["a","b"],"d\n"),(fromList ["b"],"e\n")]
byContents :: [(Tags, [Content])] -> [(Tags, Content)]
byContents =
  let
    concatTags h (a1, b1) (a2, _) = (h a1 a2, b1)
    f :: (a -> a -> a) ->  [(a, b)] -> (a, b)
    f h = foldl1 (concatTags h)
  in fmap (f Set.union) . p . g

-- | @factor@
--
-- >>> :set -XOverloadedStrings
-- >>> factor [(Set.fromList ["a"],["a\nb\n","c\n","d\n"]),(Set.fromList ["b"],["a\nb\n","d\n","e\n"])]
-- [(fromList ["a"],["c\n"]),(fromList ["a","b"],["d\n","a\nb\n"]),(fromList ["b"],["e\n"])]

factor :: [(Tags, [Content])] -> [(Tags, [Content])]
factor =
  let
    l :: Applicative c => [(a, b)] -> [(a, c b)]
    l = fmap (over _2 pure)
    byTags = Map.fromListWith (<>)
  in Map.toList . byTags . l . byContents
