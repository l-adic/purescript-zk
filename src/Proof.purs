module Proof
  ( G1(..)
  , G2(..)
  , Proof(..)
  , Inputs(..)
  , VerifyingKey(..)
  , proofForContract
  , verifyingKeyForContract
  , readProofFromFile
  , readInputsFromFile
  , readVerifyingKeyFromFile
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError(..), jsonEmptyObject, parseJson, printJsonDecodeError, (~>))
import Data.Argonaut as A
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (sortWith, (!!))
import Data.Either (Either, either)
import Data.Maybe (fromJust, maybe)
import Data.Reflectable (class Reflectable)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff, error)
import Network.Ethereum.Core.BigNumber (decimal, fromStringAs, toString)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3.Solidity (UIntN, Vector, nilVector, toVector, uIntNFromBigNumber, unUIntN, unVector, vCons)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (FilePath)
import Partial.Unsafe (unsafePartial)
import Type.Proxy (Proxy(..))

newtype G1 = G1 { x :: UIntN 256, y :: UIntN 256 }

derive newtype instance Show G1
derive newtype instance Eq G1

instance A.DecodeJson G1 where
  decodeJson json = do
    obj <- A.decodeJson json
    x <- obj A..: "x" >>= decodeUInt
    y <- obj A..: "y" >>= decodeUInt
    pure $ G1 { x, y }

instance A.EncodeJson G1 where
  encodeJson (G1 { x, y }) =
    "x" A.:= encodeUInt x
      ~> "y" A.:= encodeUInt y
      ~> jsonEmptyObject

newtype G2 = G2 { x :: Vector 2 (UIntN 256), y :: Vector 2 (UIntN 256) }

derive newtype instance Show G2
derive newtype instance Eq G2

instance A.DecodeJson G2 where
  decodeJson json = do
    obj <- A.decodeJson json
    x <- do
      Tuple a b <- obj A..: "x"
      _a <- decodeUInt a
      _b <- decodeUInt b
      pure $ vCons _b $ vCons _a nilVector
    y <- do
      Tuple a b <- obj A..: "y"
      _a <- decodeUInt a
      _b <- decodeUInt b
      pure $ vCons _b $ vCons _a nilVector
    pure $ G2 { x, y }

instance A.EncodeJson G2 where
  encodeJson (G2 { x, y }) =
    let
      x_list = encodeUInt <$> unVector x
      y_list = encodeUInt <$> unVector y
    in
      "x" A.:= Tuple (x_list !! 1) (x_list !! 0)
        ~> "y" A.:= Tuple (y_list !! 1) (y_list !! 0)
        ~> jsonEmptyObject

newtype Proof = Proof
  { a :: G1
  , b :: G2
  , c :: G1
  }

derive newtype instance Show Proof
derive newtype instance Eq Proof

instance A.DecodeJson Proof where
  decodeJson json = do
    obj <- A.decodeJson json
    a <- obj A..: "a"
    b <- obj A..: "b"
    c <- obj A..: "c"
    pure $ Proof { a, b, c }

instance A.EncodeJson Proof where
  encodeJson (Proof { a, b, c }) =
    "a" A.:= a
      ~> "b" A.:= b
      ~> "c" A.:= c
      ~> jsonEmptyObject

proofForContract
  :: Proof
  -> { "A" :: { "X" :: UIntN 256, "Y" :: UIntN 256 }
     , "B" :: { "X" :: Vector 2 (UIntN 256), "Y" :: Vector 2 (UIntN 256) }
     , "C" :: { "X" :: UIntN 256, "Y" :: UIntN 256 }
     }
proofForContract (Proof { a: G1 a, b: G2 b, c: G1 c }) =
  { "A": { "X": a.x, "Y": a.y }
  , "B": { "X": b.x, "Y": b.y }
  , "C": { "X": c.x, "Y": c.y }
  }

newtype VerifyingKey n = VerifyingKey
  { alpha1 :: G1
  , beta2 :: G2
  , gamma2 :: G2
  , delta2 :: G2
  , ic :: Vector n (G1)
  }

derive newtype instance Show (VerifyingKey n)
derive newtype instance Eq (VerifyingKey n)

instance A.EncodeJson (VerifyingKey n) where
  encodeJson (VerifyingKey { alpha1, beta2, gamma2, delta2, ic }) =
    "alpha1" A.:= alpha1
      ~> "beta2" A.:= beta2
      ~> "gamma2" A.:= gamma2
      ~> "delta2" A.:= delta2
      ~> "ic" A.:= unVector ic
      ~> jsonEmptyObject

instance Reflectable n Int => A.DecodeJson (VerifyingKey n) where
  decodeJson json = do
    obj <- A.decodeJson json
    alpha1 <- obj A..: "alpha1"
    beta2 <- obj A..: "beta2"
    gamma2 <- obj A..: "gamma2"
    delta2 <- obj A..: "delta2"
    _ic <- obj A..: "ic"
    ic <- maybe (throwError $ TypeMismatch "wrong length ic") pure $ toVector (Proxy @n) _ic
    pure $ VerifyingKey { alpha1, beta2, gamma2, delta2, ic }

verifyingKeyForContract
  :: forall n
   . VerifyingKey n
  -> { alfa1 :: { "X" :: UIntN 256, "Y" :: UIntN 256 }
     , beta2 :: { "X" :: Vector 2 (UIntN 256), "Y" :: Vector 2 (UIntN 256) }
     , gamma2 :: { "X" :: Vector 2 (UIntN 256), "Y" :: Vector 2 (UIntN 256) }
     , delta2 :: { "X" :: Vector 2 (UIntN 256), "Y" :: Vector 2 (UIntN 256) }
     , "IC" :: Array { "X" :: UIntN 256, "Y" :: UIntN 256 }
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
    , beta2: { "X": beta2.x, "Y": beta2.y }
    , gamma2: { "X": gamma2.x, "Y": gamma2.y }
    , delta2: { "X": delta2.x, "Y": delta2.y }
    , "IC": (\(G1 { x, y }) -> { "X": x, "Y": y }) <$> unVector ic
    }

newtype Inputs n = Inputs (Vector n (UIntN 256))

derive newtype instance Show (Inputs n)
derive newtype instance Eq (Inputs n)

readProofFromFile :: FilePath -> Aff Proof
readProofFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents

readInputsFromFile :: forall n. Reflectable n Int => FilePath -> Aff (Inputs n)
readInputsFromFile fp = Inputs <$> do
  file <- readTextFile UTF8 fp
  let lines = split (Pattern "\n") file
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

readVerifyingKeyFromFile :: forall n. Reflectable n Int => FilePath -> Aff (VerifyingKey n)
readVerifyingKeyFromFile fp = do
  contents <- readTextFile UTF8 fp
  either (throwError <<< error <<< printJsonDecodeError) pure $ fromJsonString contents

decodeUInt :: A.Json -> Either JsonDecodeError (UIntN 256)
decodeUInt json = do
  bn <- A.decodeJson json
  maybe (throwError $ TypeMismatch "not a uint") pure $ uIntNFromBigNumber (Proxy @256) bn

encodeUInt :: UIntN 256 -> A.Json
encodeUInt = unsafePartial $ A.encodeJson <<< fromJust <<< mkHexString <<< toString <<< unUIntN