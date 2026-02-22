module Main (main) where
import Test.Hspec
import qualified NumeriSpec
main :: IO ()
main = hspec NumeriSpec.spec