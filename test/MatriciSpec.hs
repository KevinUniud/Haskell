{-# LANGUAGE ScopedTypeVariables #-}

module MatriciSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>))
import Data.List (transpose)

import Matrici
  ( matrixDim
  , colsums
  , colaltsums
  , colMinMax
  , lowertriangular
  , uppertriangular
  , diagonal
  , convergent
  , trasposta
  , isSymmetric
  , matrixProduct
  )

spec :: Spec
spec = do
  describe "matrixDim" $ do
    it "matrice ben formata 2x3" $
      matrixDim ([[1,2,3],[4,5,6]] :: [[Int]]) `shouldBe` (2,3)

    it "matrice vuota (convenzione tipica 0x0)" $
      matrixDim ([] :: [[Int]]) `shouldBe` (0,0)

    it "matrice mal formata (righe di diversa lunghezza) -> (-1,-1)" $
      matrixDim ([[1,2],[3]] :: [[Int]]) `shouldBe` (-1,-1)

  describe "colsums" $ do
    it "somma colonne (2x3)" $
      colsums ([[1,2,3],[4,5,6]] :: [[Int]]) `shouldBe` [5,7,9]

    it "identità 3x3 (somma colonne = [1,1,1])" $
      colsums ([[1,0,0],[0,1,0],[0,0,1]] :: [[Int]]) `shouldBe` [1,1,1]

    prop "lunghezza risultato = numero colonne (se ben formata e non vuota)" $
      \(m :: [[Int]]) ->
        case matrixDim m of
          (r,c) | r > 0 && c > 0 -> length (colsums m) == fromIntegral c
          _                       -> True

  describe "colaltsums" $ do
    it "alternanza segni su righe: [[1,2],[3,4],[5,6]] -> [1-3+5, 2-4+6]" $
      colaltsums ([[1,2],[3,4],[5,6]] :: [[Int]]) `shouldBe` [3,4]

    it "una sola riga: coincide con la riga" $
      colaltsums ([[7,8,9]] :: [[Int]]) `shouldBe` [7,8,9]

    prop "se tutte le righe pari sono zeri, colaltsums coincide con colsums" $
      \(m :: [[Int]]) ->
        let m' = zipWith (\i row -> if even i then replicate (length row) 0 else row) [1 :: Int ..] m
        in case matrixDim m' of
            (r,c) | r > 0 && c > 0 -> colaltsums m' == colsums m'
            _                      -> True

  describe "colMinMax" $ do
    it "min/max colonne (2x3)" $
      colMinMax ([[1,5,3],[4,2,6]] :: [[Int]]) `shouldBe` [(1,4),(2,5),(3,6)]

    it "colonne con negativi" $
      colMinMax ([[0,-1],[2,-3],[4,5]] :: [[Int]]) `shouldBe` [(0,4),(-3,5)]

    prop "ogni (mn,mx) soddisfa mn <= mx" $
      \(m :: [[Int]]) ->
        case matrixDim m of
          (r,c) | r > 0 && c > 0 ->
            all (\(mn,mx) -> mn <= mx) (colMinMax m)
          _ -> True

  describe "lowertriangular / uppertriangular / diagonal" $ do
    let lt = ([[1,0,0],[2,-3,0],[4,5,6]] :: [[Int]])
        notLt = ([[0,0,1],[2,-3,0],[4,5,6]] :: [[Int]])
        ut = ([[1,2,3],[0,4,5],[0,0,6]] :: [[Int]])
        d  = ([[1,0,0],[0,2,0],[0,0,3]] :: [[Int]])

    it "lowertriangular esempio True" $
      lowertriangular lt `shouldBe` True

    it "lowertriangular esempio False" $
      lowertriangular notLt `shouldBe` False

    it "uppertriangular esempio True" $
      uppertriangular ut `shouldBe` True

    it "diagonal esempio True" $
      diagonal d `shouldBe` True

    it "diagonal implica lowertriangular e uppertriangular" $ do
      lowertriangular d `shouldBe` True
      uppertriangular d `shouldBe` True

  describe "convergent" $ do
    it "matrice diagonale è convergente per ogni r>0 (esempio)" $
      convergent ([[5,0,0],[0,1,0],[0,0,-2]] :: [[Int]]) 1 `shouldBe` True

    it "esempio non convergente per r piccolo" $
      convergent ([[1,2],[3,4]] :: [[Int]]) 1 `shouldBe` False

  describe "trasposta" $ do
    it "trasposta 2x3 -> 3x2" $
      trasposta ([[1,2,3],[4,5,6]] :: [[Int]]) `shouldBe` [[1,4],[2,5],[3,6]]

    it "involutiva: trasposta (trasposta m) = m (se ben formata)" $
      let m = ([[1,2],[3,4],[5,6]] :: [[Int]])
      in trasposta (trasposta m) `shouldBe` m

    prop "coerente con Data.List.transpose (se ben formata)" $
      \(m :: [[Int]]) ->
        case matrixDim m of
          (r,c) | r >= 0 && c >= 0 && (r == 0 || c == 0 || all ((== fromIntegral c) . length) m) ->
            trasposta m == transpose m
          _ -> True

  describe "isSymmetric" $ do
    it "simmetrica 3x3" $
      isSymmetric ([[1,2,3],[2,4,5],[3,5,6]] :: [[Int]]) `shouldBe` True

    it "non simmetrica 3x3" $
      isSymmetric ([[1,0,1],[2,3,4],[5,6,7]] :: [[Int]]) `shouldBe` False

  describe "matrixProduct" $ do
    it "prodotto 2x3 * 3x2 = 2x2" $
      matrixProduct ([[1,2,3],[4,5,6]] :: [[Int]])
                    ([[7,8],[9,10],[11,12]] :: [[Int]])
        `shouldBe` [[58,64],[139,154]]

    it "identità: I * A = A (3x3)" $
      let i = ([[1,0,0],[0,1,0],[0,0,1]] :: [[Int]])
          a = ([[2,3,4],[5,6,7],[8,9,10]] :: [[Int]])
      in matrixProduct i a `shouldBe` a

    it "identità: A * I = A (3x3)" $
      let i = ([[1,0,0],[0,1,0],[0,0,1]] :: [[Int]])
          a = ([[2,3,4],[5,6,7],[8,9,10]] :: [[Int]])
      in matrixProduct a i `shouldBe` a