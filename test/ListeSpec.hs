{-# LANGUAGE ScopedTypeVariables #-}

module ListeSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))
import Data.List (sort)
import Liste

spec :: Spec
spec = do

  describe "removePairs" $ do
    it "rimuove elementi in posizione pari (esempio)" $
      removePairs [1..6 :: Integer] `shouldBe` [1,3,5]

    it "lista vuota" $
      removePairs ([] :: [Integer]) `shouldBe` []

    prop "lunghezza <= (n+1)/2" $
      \(xs :: [Integer]) ->
        length (removePairs xs) <= (length xs + 1) `div` 2

  describe "sumOdds" $ do
    it "somma elementi in posizione dispari (esempio)" $
      sumOdds [1..5 :: Integer] `shouldBe` (1 + 3 + 5)

    it "lista vuota = 0" $
      sumOdds ([] :: [Integer]) `shouldBe` 0

    prop "coerente con removePairs" $
      \(xs :: [Integer]) ->
        sumOdds xs == sum (removePairs xs)

  describe "quickSort" $ do
    it "ordina lista semplice" $
      quickSort [3,1,4,1,5 :: Integer] `shouldBe` [1,1,3,4,5]

    it "lista già ordinata" $
      quickSort [1..5 :: Integer] `shouldBe` [1..5]

    it "lista vuota" $
      quickSort ([] :: [Integer]) `shouldBe` []

    prop "equivalente a Data.List.sort" $
      \(xs :: [Integer]) ->
        quickSort xs == sort xs

    prop "idempotente" $
      \(xs :: [Integer]) ->
        quickSort (quickSort xs) == quickSort xs

  describe "minOdd" $ do
    it "esempio fornito" $
      minOdd [2,3,4,6,8,7,5 :: Integer] `shouldBe` [3,5]

    prop "risultato ha al massimo 2 elementi" $
      \(xs :: [Integer]) ->
        length (minOdd xs) <= 2

    prop "se due elementi presenti, sono dispari e ordinati" $
      \(xs :: [Integer]) ->
        case minOdd xs of
          [a,b] -> odd a && odd b && a <= b
          _     -> True

  describe "fun5" $ do
    it "esempio semplice" $
      fun5 [1,2,3 :: Integer] `shouldBe`
        [ (1, 2+3)
        , (2, 3)
        , (3, 0)
        ]

    it "lista vuota" $
      fun5 ([] :: [Integer]) `shouldBe` []

    prop "stessa lunghezza della lista originale" $
      \(xs :: [Integer]) ->
        length (fun5 xs) == length xs

    prop "somma totale coerente" $
      \(xs :: [Integer]) ->
        let pairs = fun5 xs
            total = sum xs
        in case pairs of
            [] -> True
            _  -> snd (head pairs) + fst (head pairs) == total

  describe "fun6" $ do
    it "lista vuota" $
      fun6 ([] :: [Integer]) `shouldBe` []

    it "esempio base" $
      fun6 [1,2,3,4 :: Integer]
        `shouldBe` [(1,0),(2,1),(3,3),(4,6)]

    it "con numeri negativi" $
      fun6 [3,-1,2 :: Integer]
        `shouldBe` [(3,0),(-1,3),(2,2)]

  describe "shiftToZero" $ do
    it "lista vuota" $
      shiftToZero ([] :: [Integer]) `shouldBe` []

    it "esempio base" $
      shiftToZero [5,4,2,6 :: Integer]
        `shouldBe` [3,2,0,4]

    it "con numeri negativi" $
      shiftToZero [3,-2,5 :: Integer]
        `shouldBe` [5,0,7]

    it "tutti uguali" $
      shiftToZero [4,4,4 :: Integer]
        `shouldBe` [0,0,0]