module Deploy.Main (main, mkEnv) where

import Prelude

import Chanterelle.Deploy (deploy)
import Chanterelle.Logging (LogLevel(..), log)
import Control.Monad.Except (runExceptT)
import Data.Either (either)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Deploy.Deploy (deployScript)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Web3 (unAddress)
import Node.Process as Env

main :: Effect Unit
main = do
  { nodeURL, timeoutSeconds } <- mkEnv
  launchAff_ $ do
    res <- deploy nodeURL timeoutSeconds deployScript
    log Info $ "Deployment Successful!"
    log Info $ "Verifier Address: " <> show (unAddress res.verifier)

mkEnv :: Effect { nodeURL :: String, timeoutSeconds :: Int }
mkEnv = do
  eEnv <- runExceptT do
    nodeURL <- liftEffect do
      mNodeURL <- Env.lookupEnv "NODE_URL"
      pure $ fromMaybe "http://localhost:8545" mNodeURL
    timeoutSeconds <- liftEffect do
      mTimeout <- Env.lookupEnv "TIMEOUT"
      pure $ fromMaybe 60 (fromString =<< mTimeout)
    pure { nodeURL, timeoutSeconds }
  either throw pure eEnv