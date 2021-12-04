
dam-aeson transforms a dam folder into JSON data

\begin{code}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson (encode)

import qualified DamFactor
import qualified Data.ByteString.Lazy

main = do
  dam <- DamFactor.parseDirectory True "."
  Data.ByteString.Lazy.putStrLn $ encode $ DamFactor.damToFactor <$> dam

\end{code}

