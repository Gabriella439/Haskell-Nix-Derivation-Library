module Main where

import qualified Nix.Derivation
import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.IO

import Data.Attoparsec.Text.Lazy (Result(..))

roundTrip :: FilePath -> IO ()
roundTrip path = do
    text0 <- Data.Text.Lazy.IO.readFile path
    derivation <- case Data.Attoparsec.Text.Lazy.parse Nix.Derivation.parseDerivation text0 of
        Fail _ _ string   -> fail (path <> ": " <> string)
        Done _ derivation -> return derivation
    let builder = Nix.Derivation.buildDerivation derivation
    let text1   = Data.Text.Lazy.Builder.toLazyText builder
    if text0 == text1
        then return ()
        else fail (path <> ": parsing and rendering the example derivation does not round-trip")

main :: IO ()
main = do
    roundTrip "tests/example0.drv"
    -- A floating content-addressed derivation (produced with
    -- `__contentAddressed = true` under the ca-derivations experimental
    -- feature): its output path is empty until realisation assigns one.
    roundTrip "tests/example-ca.drv"
