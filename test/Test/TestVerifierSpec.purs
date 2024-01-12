module Test.TestVerifierSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Ethereum.Web3 (Address, Provider, _from, _to, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)
import Proof (Inputs, readInputsFromFile, readProofFromFile, readVerifyingKeyFromFile, verifyWithTestVerifier)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec
  :: forall r
   . { provider :: Provider, accounts :: Array Address, testVerifier :: Address | r }
  -> SpecT Aff Unit Aff Unit
spec { testVerifier, accounts, provider } =
  describe "TestVerifier Spec"
    $ it "Can verify a proof of a simple program"
    $ do
        let primaryAccount = unsafePartial $ head accounts
        Console.log "Parsing proof file"
        proof <- readProofFromFile "proof-data/sudoku-proof-eth.json"
        Console.log "Parsing verifying key file"
        vk <- readVerifyingKeyFromFile "proof-data/sudoku-vk-eth.json"
        Console.log "Parsing inputs file"
        inputs :: Inputs 81 <- readInputsFromFile "proof-data/sudoku-inputs.jsonl"
        let
          txOpts =
            defaultTransactionOptions
              # _from ?~ primaryAccount
              # _to ?~ testVerifier
        res <- assertWeb3 provider $ verifyWithTestVerifier txOpts { proof, vk, inputs }
        res `shouldEqual` Right true