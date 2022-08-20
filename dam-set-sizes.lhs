
`dam-set-sizes` shows set sizes in a folder full of tagged dam files.

Imports

\begin{code}

import Dam
import DamFactor
import Data.List (foldl, nub)
import Data.Text (Text)

import qualified Data.Map as Map
import qualified Data.Set as Set

\end{code}

We want to get data in a form described by ByTag,
so that cards can be counted.

In order to do that, we go through an intermediate structure
to eliminate duplicates.

\begin{code}

type Received = [(Tags, [Text])]
type ByTag = Map.Map Tag [Text]
type Unique = (Tag, Text)

deduplication :: Received -> [Unique]
deduplication = nub . foldMap unpack
  where unpack :: (Tags, [Text]) -> [(Tag, Text)]
        unpack (tags, texts) = do
          tag <- Set.toList tags
          text <- texts
          pure (tag, text)

assign :: ByTag -> [Unique] -> ByTag
assign = foldl folding
  where folding :: ByTag -> (Tag, Text) -> ByTag
        folding byTag (tag, text) = Map.insertWith (<>) tag [text] byTag

count :: ByTag -> Map.Map Tag Int
count = fmap length

setSizes = count . assign Map.empty . deduplication

\end{code}

Input/Output and main

\begin{code}

rowFormat (tag, int) = show int <> ", " <> tag

main = do
  contents <- fmap damToFactor <$> parseDirectory False "."
  let counts = setSizes contents
  mapM (putStrLn . rowFormat) (Map.toList counts)

\end{code}
