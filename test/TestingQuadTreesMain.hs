module Main (main) where
import Test.Hspec
import qualified QuadTreesSpec
main :: IO ()
main = hspec QuadTreesSpec.spec