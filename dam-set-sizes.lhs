
`dam-set-sizes` shows set sizes in a folder full of tagged dam files.

Imports

\begin{code}

import Dam
import DamFactor
import Data.Hypergraph
import Data.List (foldl, nub)
import Data.Set (Set)
import Data.Text (Text)
import Lens.Micro.Extras (view)

import qualified Data.Map as Map
import qualified Data.Set as Set

\end{code}

Input/Output and main

\begin{code}

rowFormat (tag, int) = show int <> ", " <> tag

main = do
  graph <- damToTagged <$> parseDirectory False "."
  let counts = Set.size <$> view edges graph
  mapM (putStrLn . rowFormat) (Map.toList counts)

\end{code}
