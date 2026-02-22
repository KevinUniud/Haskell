module Main (main) where
import Test.Hspec
import qualified MatriciSpec
main :: IO ()
main = hspec MatriciSpec.spec