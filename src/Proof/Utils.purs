module Proof.Utils
  ( readProofFromFile
  , readInputsFromFile
  , readVerifyingKeyFromFile
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (parseJson, printJsonDecodeError)
import Data.Argonaut as A
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (filter, length)
import Data.Either (either)
import Data.Maybe (maybe)
import Data.Reflectable (class Reflectable, reflectType)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Effect.Aff (Aff, error)
import Network.Ethereum.Web3.Solidity (toVector)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Proof.Types (Input(..), InputType(..), Inputs(..), Proof, VerifyingKey)
import Type.Proxy (Proxy(..))

readProofFromFile :: FilePath -> Aff Proof
readProofFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents

readInputsFromFile :: forall n. Reflectable n Int => FilePath -> Aff (Inputs n)
readInputsFromFile fp = Inputs <$> do
  file <- readTextFile UTF8 fp
  let lines = split (Pattern "\n") file
  allInputs :: Array Input <- either (throwError <<< error <<< printJsonDecodeError) pure $ traverse (parseJson >=> A.decodeJson) lines
  let publicInputs = filter (\(Input { tag }) -> tag /= Private) allInputs
  maybe (throwError $ error $ "incorrect number of inputs: got " <> show (length publicInputs) <> " expected " <> show (reflectType (Proxy @n)))
    pure $ toVector (Proxy @n) publicInputs

readVerifyingKeyFromFile :: forall n. Reflectable n Int => FilePath -> Aff (VerifyingKey n)
readVerifyingKeyFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents