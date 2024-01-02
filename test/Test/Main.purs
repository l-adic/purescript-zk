module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.ProofSpec as ProofSpec

main :: Effect Unit
main =
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    void $ join
      $ runSpecT cfg [ consoleReporter ] do
          ProofSpec.spec
