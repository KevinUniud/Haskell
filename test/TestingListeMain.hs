module Main (main) where
import Test.Hspec
import qualified ListeSpec
main :: IO ()
main = hspec ListeSpec.spec