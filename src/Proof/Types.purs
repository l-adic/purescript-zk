module Proof.Types
  ( G1(..)
  , G2(..)
  , Fp2(..)
  , Proof(..)
  , Inputs(..)
  , VerifyingKey(..)
  , fp2ForEth
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (JsonDecodeError(..), jsonEmptyObject, (~>))
import Data.Argonaut as A
import Data.Either (Either)
import Data.Maybe (fromJust, maybe)
import Data.Reflectable (class Reflectable)
import Data.Tuple (Tuple(..))
import Network.Ethereum.Core.BigNumber (toString)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Web3.Solidity (UIntN, Vector, nilVector, toVector, uIntNFromBigNumber, unUIntN, unVector, vCons)
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

newtype Fp2 = Fp2 { real :: UIntN 256, imag :: UIntN 256 }

derive newtype instance Show Fp2
derive newtype instance Eq Fp2

instance A.DecodeJson Fp2 where
  decodeJson json = do
    Tuple _real _imag <- A.decodeJson json
    real <- decodeUInt _real
    imag <- decodeUInt _imag
    pure $ Fp2 { real, imag }

instance A.EncodeJson Fp2 where
  encodeJson (Fp2 { real, imag }) = A.encodeJson $
    Tuple (encodeUInt real) (encodeUInt imag)

fp2ForEth :: Fp2 -> Vector 2 (UIntN 256)
fp2ForEth (Fp2 { real, imag }) = vCons imag $ vCons real nilVector

{-
NOTE:

In arkworks, all elements of F_{p^2} are represented as [a,b] == a + b * u where u^2 = -1
Thus G2 == [[x_real,x_imag], [y_real, y_imag]]

This is also the convention of circom:
https://github.com/yi-sun/circom-pairing/blob/master/docs/README.md#fp2-element

However, in ethereum the convention is the opposite (of course):
https://github.com/ethereum/EIPs/blob/master/EIPS/eip-197.md#encoding


Thus we assume that the json representation of G2 is:
{ "x": [x_real, x_imag], "y": [y_real, y_imag] }

-}

newtype G2 = G2 { x :: Fp2, y :: Fp2 }

derive newtype instance Show G2
derive newtype instance Eq G2

instance A.DecodeJson G2 where
  decodeJson json = do
    obj <- A.decodeJson json
    x <- obj A..: "x"
    y <- obj A..: "y"
    pure $ G2 { x, y }

instance A.EncodeJson G2 where
  encodeJson (G2 { x, y }) =
    "x" A.:= x
      ~> "y" A.:= y
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

newtype VerifyingKey = VerifyingKey
  { alpha1 :: G1
  , beta2 :: G2
  , gamma2 :: G2
  , delta2 :: G2
  , ic :: Array G1
  }

derive newtype instance Show VerifyingKey
derive newtype instance Eq VerifyingKey

instance A.EncodeJson VerifyingKey where
  encodeJson (VerifyingKey { alpha1, beta2, gamma2, delta2, ic }) =
    "alpha1" A.:= alpha1
      ~> "beta2" A.:= beta2
      ~> "gamma2" A.:= gamma2
      ~> "delta2" A.:= delta2
      ~> "ic" A.:= ic
      ~> jsonEmptyObject

instance A.DecodeJson VerifyingKey where
  decodeJson json = do
    obj <- A.decodeJson json
    alpha1 <- obj A..: "alpha1"
    beta2 <- obj A..: "beta2"
    gamma2 <- obj A..: "gamma2"
    delta2 <- obj A..: "delta2"
    ic <- obj A..: "ic"
    pure $ VerifyingKey { alpha1, beta2, gamma2, delta2, ic }

newtype Inputs n = Inputs (Vector n (UIntN 256))

derive newtype instance Show (Inputs n)
derive newtype instance Eq (Inputs n)

decodeUInt :: A.Json -> Either JsonDecodeError (UIntN 256)
decodeUInt json = do
  bn <- A.decodeJson json
  maybe (throwError $ TypeMismatch "not a uint") pure $ uIntNFromBigNumber (Proxy @256) bn

encodeUInt :: UIntN 256 -> A.Json
encodeUInt = unsafePartial $ A.encodeJson <<< fromJust <<< mkHexString <<< toString <<< unUIntN