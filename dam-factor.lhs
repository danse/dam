
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

import qualified Data.Set as Set
import qualified Data.Text as Text

import Dam
import DamFactor
import Lens.Micro

\end{code}

Dam gives us cards and expressions. Here we want to consider cards,
like filepaths, just as text objects

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

Then we want some functions to read and write directories

\begin{code}

writeDirectory :: FilePath -> [(Tags, [Content])] -> IO ()
writeDirectory dirPath =
  let textToFile' tags = textToFile (dirPath </> tagsToPath tags)
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
