
dam-factor factors dam tag files so that their contents are not repeated

have a look into `dam-factor-test` for input and output instances

\begin{code}

import Control.Monad (when)
import Data.Either (lefts, rights)
import Data.Function ((&))
import Data.List (groupBy, sortBy, sortOn)
import Data.Text (Text)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>))

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import Dam
import DamFactor
import Lens.Micro

\end{code}

Dam gives us cards and expressions. Here we want to consider cards, like filepaths, just as text objects

\begin{code}

textToFile :: FilePath -> [Text] -> IO ()
textToFile path texts = 
  let contents = Text.intercalate (Text.pack "\n") texts
  in writeFile path (Text.unpack contents)

\end{code}

We want to remove duplicated content. We want content to point to tags.

Every content is unique and can point to multiple tags like in a dictionary.

\begin{code}

tagsToPath :: Tags -> FilePath
tagsToPath = Prelude.unwords . Set.toList

\end{code}

We want to find the set of tags shared between a set of contents.

In @factor@, first the contents are separated from each other, then for every content we collect an union of tags. Then we group the different tag sets.

\begin{code}

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

\end{code}

Then we want some functions to read and write directories

\begin{code}

writeDirectory :: FilePath -> [(Tags, [Content])] -> IO ()
writeDirectory dirPath =
  let textToFile' tags = textToFile (dirPath </> (tagsToPath tags))
  in mapM_ (uncurry textToFile')

main = do
  (outputDir:rest) <- getArgs
  let verbose = not $ null rest
  c <- parseDirectory verbose "."
  let
    factorAndWrite :: [(FilePath, [Dam.Card])] -> IO ()
    factorAndWrite contents = do
      let
        factorContents = damToFactor <$> contents
        factoredContents = factor factorContents
      when verbose (print factorContents)
      writeDirectory outputDir factoredContents
  factorAndWrite c

\end{code}
