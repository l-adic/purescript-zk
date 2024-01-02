module Proof.Contract (verifyWithTestVerifier, verifyWithVerifier) where

import Prelude

import Contracts.TestVerifier as TestVerifier
import Contracts.Verifier as Verifier
import Data.Either (Either)
import Network.Ethereum.Web3 (CallError, ChainCursor(..), TransactionOptions, Web3, nilVector, vCons)
import Network.Ethereum.Web3.Solidity (unVector)
import Network.Ethereum.Web3.Types (NoPay)
import Proof.Types (Proof(..), VerifyingKey(..), G1(..), G2(..), fp2ForEth, Inputs(..))

verifyWithTestVerifier
  :: forall n m r
   . TransactionOptions NoPay
  -> { proof :: Proof
     , vk :: VerifyingKey m
     , inputs :: Inputs n
     | r
     }
  -> Web3 (Either CallError Boolean)
verifyWithTestVerifier txOpts arg@{ inputs: Inputs inputs } =
  TestVerifier.verify txOpts Latest { proof: proofForContract arg.proof, vk: verifyingKeyForContract arg.vk, input: unVector inputs }
  where
  proofForContract (Proof { a: G1 a, b: G2 b, c: G1 c }) =
    { "A": { "X": a.x, "Y": a.y }
    , "B": { "X": fp2ForEth b.x, "Y": fp2ForEth b.y }
    , "C": { "X": c.x, "Y": c.y }
    }
  verifyingKeyForContract vk =
    let
      VerifyingKey
        { alpha1: G1 alpha1
        , beta2: G2 beta2
        , gamma2: G2 gamma2
        , delta2: G2 delta2
        , ic
        } = vk
    in
      { alfa1: { "X": alpha1.x, "Y": alpha1.y }
      , beta2: { "X": fp2ForEth beta2.x, "Y": fp2ForEth beta2.y }
      , gamma2: { "X": fp2ForEth gamma2.x, "Y": fp2ForEth gamma2.y }
      , delta2: { "X": fp2ForEth delta2.x, "Y": fp2ForEth delta2.y }
      , "IC": (\(G1 { x, y }) -> { "X": x, "Y": y }) <$> unVector ic
      }

verifyWithVerifier
  :: forall r
   . TransactionOptions NoPay
  -> { proof :: Proof
     , inputs :: Inputs 1
     | r
     }
  -> Web3 (Either CallError Boolean)
verifyWithVerifier txOpts { proof, inputs } =
  Verifier.verifyProof txOpts Latest (proofForContract inputs proof)
  where
  proofForContract (Inputs _inputs) (Proof { a: G1 a, b: G2 b, c: G1 c }) =
    { _pA: vCons a.x $ vCons a.y nilVector
    , _pB: vCons (fp2ForEth b.x) $ vCons (fp2ForEth b.y) nilVector
    , _pC: vCons c.x $ vCons c.y nilVector
    , _pubSignals: _inputs
    }