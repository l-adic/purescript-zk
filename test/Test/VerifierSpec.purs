module Test.VerifierSpec (spec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array.Partial (head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Network.Ethereum.Web3 (Address, Provider, _from, _gas, _to, defaultTransactionOptions, fromInt)
import Partial.Unsafe (unsafePartial)
import Proof (Inputs, readInputsFromFile, readProofFromFile, readVerifyingKeyFromFile, verifyWithVerifier)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec
  :: forall r
   . { provider :: Provider, accounts :: Array Address, verifier :: Address | r }
  -> SpecT Aff Unit Aff Unit
spec { verifier, accounts, provider } =
  describe "Verifier Spec"
    $ it "Can verify a proof of a simple program"
    $ do
        let primaryAccount = unsafePartial $ head accounts
        Console.log "Parsing proof file"
        proof <- readProofFromFile "proof-data/sudoku-proof-eth.json"
        Console.log "Parsing verifying key file"
        vk <- readVerifyingKeyFromFile "proof-data/sudoku-vk-eth.json"
        Console.log "Parsing inputs file"
        inputs :: Inputs 76 <- readInputsFromFile "proof-data/sudoku-inputs.jsonl"
        let
          txOpts =
            defaultTransactionOptions
              # _from ?~ primaryAccount
              # _to ?~ verifier
              # _gas ?~ fromInt 2147483647
        res <- assertWeb3 provider $ verifyWithVerifier txOpts { proof, vk, inputs }
        res `shouldEqual` Right true
