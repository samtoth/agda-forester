module Forester.Main (main) where

import Agda.Main (runAgda)

import qualified Forester.Backend as Forester

-- | Invokes the agda-compiler with the additional backend.
main :: IO ()
main = runAgda [Forester.foresterBackend]