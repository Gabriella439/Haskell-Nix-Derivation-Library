{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Text (Text)
import Data.Vector (Vector)
import System.FilePath
import Nix.Derivation (Derivation(..), DerivationOutput(..))
import Prelude hiding (FilePath, either)
import Test.QuickCheck (Arbitrary(..), Gen)

import qualified Control.Monad
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.List
import qualified Data.Map
import qualified Data.Set
import qualified Data.Text
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector
import qualified Nix.Derivation
import qualified System.Exit
import qualified Test.QuickCheck

import Debug.Trace

genChar :: Gen Char
genChar = Test.QuickCheck.elements $ ['a'..'z'] <> ['A'..'Z'] <> ['\n', '\r', '\t']

instance Arbitrary Text where
    arbitrary = Data.Text.pack <$> Test.QuickCheck.scale (`div`4) (Test.QuickCheck.listOf genChar)

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = Data.Vector.fromList <$> arbitrary

genFilePath :: Gen FilePath
genFilePath = do
  path <- Test.QuickCheck.scale (`div`4) $ Test.QuickCheck.listOf1 $ Test.QuickCheck.listOf1 genChar
  pure $ "/" <> Data.List.intercalate "/" path <> ".ext"

instance Arbitrary (DerivationOutput FilePath Text) where
    arbitrary = do
        path     <- genFilePath
        hashAlgo <- arbitrary
        hash     <- arbitrary
        return (DerivationOutput {..})

instance Arbitrary (Derivation FilePath Text) where
    arbitrary = do
        outputs   <- arbitrary
        inputDrvs <- Data.Map.fromList <$> Test.QuickCheck.listOf ((,) <$> genFilePath <*> arbitrary)
        inputSrcs <- Data.Set.fromList <$> Test.QuickCheck.listOf genFilePath
        platform  <- arbitrary
        builder   <- arbitrary
        args      <- arbitrary
        env       <- arbitrary
        return (Derivation {..})

property :: Derivation FilePath Text -> Bool
property derivation0 = either == Right derivation0
  where
    builder = Nix.Derivation.buildDerivation derivation0

    text = Data.Text.Lazy.Builder.toLazyText builder

    result =
        Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text

    either =
        Data.Attoparsec.Text.Lazy.eitherResult result

main :: IO ()
main = do
  result <- Test.QuickCheck.quickCheckResult property
  Control.Monad.unless
    (Test.QuickCheck.isSuccess result)
    System.Exit.exitFailure
