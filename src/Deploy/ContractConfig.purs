module Deploy.ContractConfig
  ( verifierCfg
  ) where

import Chanterelle.Types.Deploy (ContractConfig, NoArgs, constructorNoArgs, noArgs)

verifierCfg :: ContractConfig NoArgs
verifierCfg =
  { filepath: "build/contracts/TestVerifier.json"
  , name: "TestVerifier"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }