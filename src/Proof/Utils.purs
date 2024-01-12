module Proof.Utils
  ( readProofFromFile
  , readInputsFromFile
  , readVerifyingKeyFromFile
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError(..), parseJson, printJsonDecodeError)
import Data.Argonaut as A
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (filter, sortWith)
import Data.Either (Either, either)
import Data.Maybe (maybe)
import Data.Reflectable (class Reflectable)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, error)
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs)
import Network.Ethereum.Web3.Solidity (UIntN, toVector, uIntNFromBigNumber)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Proof.Types (Inputs(..), Proof, VerifyingKey)
import Type.Proxy (Proxy(..))

readProofFromFile :: FilePath -> Aff Proof
readProofFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents

readInputsFromFile :: forall n. Reflectable n Int => FilePath -> Aff (Inputs n)
readInputsFromFile fp = Inputs <$> do
  file <- readTextFile UTF8 fp
  let lines = filter (not (_ == "")) $ split (Pattern "\n") file
  inputs <- either (throwError <<< error <<< printJsonDecodeError) pure $ traverse (parseJson >=> jsonParser) lines
  let
    sortedInputs :: Array (UIntN 256)
    sortedInputs = snd <$> sortWith fst inputs
  maybe (throwError $ error "incorrect number of inputs") pure $ toVector (Proxy @n) sortedInputs
  where
  jsonParser :: A.Json -> Either JsonDecodeError (Tuple Int (UIntN 256))
  jsonParser json = do
    Tuple i str <- A.decodeJson json
    v <- decodeInputBn str
    pure $ Tuple i v

  decodeInputBn :: String -> Either JsonDecodeError (UIntN 256)
  decodeInputBn str = do
    bn <- maybe (throwError $ TypeMismatch "not a base 10 BigNumber") pure $ fromStringAs decimal str
    maybe (throwError $ TypeMismatch "not a uint") pure $ uIntNFromBigNumber (Proxy @256) bn

readVerifyingKeyFromFile :: FilePath -> Aff VerifyingKey
readVerifyingKeyFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents