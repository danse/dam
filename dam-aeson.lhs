
dam-aeson transforms a dam folder into JSON data

\begin{code}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (encode)

import qualified DamFactor
import qualified Data.ByteString.Lazy.Char8

main = do
  dam <- DamFactor.parseDirectory True "."
  Data.ByteString.Lazy.Char8.putStrLn $ encode $ DamFactor.damToFactor <$> dam

\end{code}

