module Deploy.Deploy
  ( DeployResults
  , deployScript
  ) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Types.Deploy (DeployConfig(..), DeployM)
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Deploy.ContractConfig (verifierCfg)
import Network.Ethereum.Web3 (Address, _from, defaultTransactionOptions)

type DeployResults =
  { verifier :: Address
  }

deployScript :: DeployM DeployResults
deployScript = do
  (DeployConfig { primaryAccount }) <- ask
  let txOpts = defaultTransactionOptions # _from ?~ primaryAccount
  { deployAddress: verifier } <- deployContract txOpts verifierCfg
  pure
    { verifier }