{-# LANGUAGE ScopedTypeVariables #-}

module NumeriSpec (spec) where

import Control.Exception (evaluate)
import Data.List (genericLength)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Property, (==>))

import Numeri (allComb, binom, fact)

spec :: Spec
spec = do
  describe "fact" $ do
    it "fact 0 = 1" $
      fact (0 :: Integer) `shouldBe` 1

    it "fact 5 = 120" $
      fact (5 :: Integer) `shouldBe` 120

    it "fact 10 = 3628800" $
      fact (10 :: Integer) `shouldBe` 3628800

    it "lancia eccezione per input negativo" $
      evaluate (fact (-1 :: Integer)) `shouldThrow` anyErrorCall

  describe "binom (n su k)" $ do
    it "binom n 0 = 1 (esempio)" $
      binom (10 :: Integer) 0 `shouldBe` 1

    it "binom n n = 1 (esempio)" $
      binom (10 :: Integer) 10 `shouldBe` 1

    it "binom 5 2 = 10" $
      binom (5 :: Integer) 2 `shouldBe` 10

    it "binom 6 3 = 20" $
      binom (6 :: Integer) 3 `shouldBe` 20

    prop "simmetria: binom n k = binom n (n-k) per 0<=k<=n" $
      \(n0 :: Integer) (k0 :: Integer) ->
        let n = abs n0
            k = if n == 0 then 0 else abs k0 `mod` (n + 1)
        in binom n k == binom n (n - k)

    prop "Pascal: binom n k = binom (n-1) (k-1) + binom (n-1) k per 1<=k<=n-1" $
      \(n0 :: Integer) (k0 :: Integer) ->
        let n = abs n0
        in n >= 2 ==>
          let k = 1 + (abs k0 `mod` (n - 1)) -- k in [1..n-1]
          in binom n k == binom (n - 1) (k - 1) + binom (n - 1) k

    prop "positività: binom n k > 0 per 0<=k<=n" $
      \(n0 :: Integer) (k0 :: Integer) ->
        let n = abs n0
            k = if n == 0 then 0 else abs k0 `mod` (n + 1)
        in binom n k > 0

    prop "somma riga: sum_k binom n k = 2^n" $
      \(n0 :: Integer) ->
        let n = abs n0
            row = map (binom n) [0 .. n]
        in sum row == (2 :: Integer) ^ n

  describe "allComb n (lista delle combinazioni su n elementi)" $ do
    it "allComb 0 = [1]" $
      allComb (0 :: Integer) `shouldBe` [1]

    it "allComb 5 = [1,5,10,10,5,1]" $
      allComb (5 :: Integer) `shouldBe` [1,5,10,10,5,1]

    prop "lunghezza: length (allComb n) = n+1" $
      \(n0 :: Integer) ->
        let n = abs n0
        in genericLength (allComb n) == n + 1

    prop "bordi: head = 1 e last = 1 (per n>=0)" $
      \(n0 :: Integer) ->
        let n = abs n0
            xs = allComb n
        in not (null xs) ==> head xs == 1 && last xs == 1

    prop "coerenza: allComb n == map (binom n) [0..n]" $
      \(n0 :: Integer) ->
        let n = abs n0
        in allComb n == map (binom n) [0 .. n]

    prop "somma: sum (allComb n) = 2^n" $
      \(n0 :: Integer) ->
        let n = abs n0
        in sum (allComb n) == (2 :: Integer) ^ n