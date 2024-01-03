module Test.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Maybe (Maybe(..))
import Deploy.Deploy (deployScript)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)
import Test.TestVerifierSpec as TestVerifierSpec
import Test.VerifierSpec as VerifierSpec

main :: Effect Unit
main =
  launchAff_ do
    let
      cfg = defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
    testCfg <- buildTestConfig nodeUrl 60 $ deployScript
    void $ join $ runSpecT cfg [ consoleReporter ] $ do
      TestVerifierSpec.spec testCfg
      VerifierSpec.spec testCfg

nodeUrl :: String
nodeUrl = "http://localhost:8545"