{-# LANGUAGE DeriveGeneric #-}

-- | Minimal test suite
module Main (main) where

import Data.Aeson

import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP

import Test.Tasty
import Test.Tasty.QuickCheck

import Generics.SOP.JSON

{-------------------------------------------------------------------------------
  Example type
-------------------------------------------------------------------------------}

data ExampleType =
    ExampleA Int
  | ExampleB [Bool] Char
  | ExampleC String ExampleType
  deriving (Show, Eq, GHC.Generic)

instance SOP.Generic         ExampleType
instance SOP.HasDatatypeInfo ExampleType

instance ToJSON ExampleType where
  toJSON = gtoJSON defaultJsonOptions

instance FromJSON ExampleType where
  parseJSON = gparseJSON defaultJsonOptions

instance Arbitrary ExampleType where
  arbitrary = sized go
    where
      go :: Int -> Gen ExampleType
      go 0 = oneof [genA, genB]
      go n = oneof [genA, genB, genC (n - 1)]

      genA, genB :: Gen ExampleType
      genA = ExampleA <$> arbitrary
      genB = ExampleB <$> arbitrary <*> arbitrary

      genC :: Int -> Gen ExampleType
      genC n = ExampleC <$> arbitrary <*> go n

  shrink (ExampleA x) = concat [
        ExampleA <$> shrink x
      ]
  shrink (ExampleB x y) = concat [
        ExampleB <$> shrink x <*> pure   y
      , ExampleB <$> pure   x <*> shrink y
      , pure $ ExampleA 0
      ]
  shrink (ExampleC x y) = concat [
        ExampleC <$> shrink x <*> pure   y
      , ExampleC <$> pure   x <*> shrink y
      , pure $ ExampleA 0
      , pure $ ExampleB [] '\NUL'
      ]

{-------------------------------------------------------------------------------
  Tests proper
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "Test_JSON_SOP" [
      testProperty "roundtrip" test_roundtrip
    ]

test_roundtrip :: ExampleType -> Property
test_roundtrip ex = decode (encode ex) === Just ex