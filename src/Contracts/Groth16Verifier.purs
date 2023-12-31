module Contracts.Groth16Verifier where

import Prelude

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Identity (Identity)
import Network.Ethereum.Web3 (Vector, Web3, call)
import Network.Ethereum.Web3.Solidity (Tuple1, Tuple4, UIntN, fromRecord, unTuple1)
import Network.Ethereum.Web3.Types (CallError, ChainCursor, NoPay, TransactionOptions)

type FnVerifyProofInput = Tagged "verifyProof(uint256[2],uint256[2][2],uint256[2],uint256[1])"
  ( Tuple4 (Tagged "_pA" (Identity (Vector 2 (UIntN 256))))
      (Tagged "_pB" (Identity (Vector 2 (Vector 2 (UIntN 256)))))
      (Tagged "_pC" (Identity (Vector 2 (UIntN 256))))
      (Tagged "_pubSignals" (Identity (Vector 1 (UIntN 256))))
  )

type FnVerifyProofOutput = Tuple1 Boolean

verifyProof
  :: TransactionOptions NoPay
  -> ChainCursor
  -> { _pA :: Vector 2 (UIntN 256)
     , _pB :: Vector 2 (Vector 2 (UIntN 256))
     , _pC :: Vector 2 (UIntN 256)
     , _pubSignals :: Vector 1 (UIntN 256)
     }
  -> Web3 (Either CallError Boolean)
verifyProof txOpts chainCursor x = map unTuple1 <$> call txOpts chainCursor
  (tagged (fromRecord x) :: FnVerifyProofInput)
