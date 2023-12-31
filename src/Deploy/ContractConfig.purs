module Deploy.ContractConfig
  ( verifierCfg
  ) where

import Prelude

import Chanterelle.Types.Deploy (ContractConfig, LibraryConfig, NoArgs, constructorNoArgs, noArgs, validateWithError)
import Data.Int (fromString)
import Network.Ethereum.Web3 (Address, BytesN, UIntN, fromInt, uIntNFromBigNumber)
import Type.Proxy (Proxy(..))

verifierCfg :: ContractConfig NoArgs
verifierCfg =
  { filepath: "build/contracts/Groth16Verifier.json"
  , name: "Groth16Verifier"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }