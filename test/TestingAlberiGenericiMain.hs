module Main (main) where
import Test.Hspec
import qualified AlberiGenericiSpec
main :: IO ()
main = hspec AlberiGenericiSpec.spec