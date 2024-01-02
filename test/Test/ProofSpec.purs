module Test.ProofSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig)
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Deploy.Deploy (deployScript)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Ethereum.Web3 (_from, _to, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)
import Proof (Inputs, VerifyingKey, readInputsFromFile, readProofFromFile, readVerifyingKeyFromFile, verifyWithTestVerifier)
import Test.Spec (SpecT, beforeAll, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: SpecT Aff Unit Aff Unit
spec =
  describe "Proof Spec"
    $ beforeAll (buildTestConfig nodeUrl 60 $ deployScript)
    $ it "Can verify a proof of a simple program"
    $ \{ verifier, accounts, provider } -> do
        let primaryAccount = unsafePartial $ head accounts
        Console.log "Parsing proof file"
        proof <- readProofFromFile "proof-data/prog-proof-eth.json"
        Console.log "Parsing verifying key file"
        vk :: VerifyingKey 2 <- readVerifyingKeyFromFile "proof-data/prog-vk-eth.json"
        Console.log "Parsing inputs file"
        inputs :: Inputs 1 <- readInputsFromFile "proof-data/prog-inputs.jsonl"
        let
          txOpts =
            defaultTransactionOptions
              # _from ?~ primaryAccount
              # _to ?~ verifier
        res <- assertWeb3 provider $ verifyWithTestVerifier txOpts { proof, vk, inputs }
        res `shouldEqual` Right true

nodeUrl :: String
nodeUrl = "http://localhost:8545"