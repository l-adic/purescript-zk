module Deploy.ContractConfig
  ( verifierCfg
  , testVerifierCfg
  ) where

import Chanterelle.Types.Deploy (ContractConfig, NoArgs, constructorNoArgs, noArgs)

testVerifierCfg :: ContractConfig NoArgs
testVerifierCfg =
  { filepath: "build/contracts/TestVerifier.json"
  , name: "TestVerifier"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

verifierCfg :: ContractConfig NoArgs
verifierCfg =
  { filepath: "build/contracts/Verifier.json"
  , name: "Verifier"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }