{-# LANGUAGE ScopedTypeVariables #-}

module MatriceMedianteQuadTreesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((==>), Arbitrary(..), sized, frequency)

import MatriceMedianteQuadTrees
  ( QT(..)
  , Eq(..)
  , Mat(..)
  , Vec(..)
  , lowertriangular
  , uppertriangular
  , diagonal
  , matSum
  , matMul
  , zong
  , f
  , colSums
  , rowSums
  , colMinMax
  , colVar
  , colAltSums
  , transposeMat
  , isSymmetric
  , foldMat
  )

-- QuickCheck instances (test-only)

instance Arbitrary a => Arbitrary (QT a) where
  arbitrary = sized gen
    where
      gen 0 = C <$> arbitrary
      gen n =
        frequency
          [ (2, C <$> arbitrary)
          , (3, do
                let m = n `div` 2
                a <- gen m
                b <- gen m
                c <- gen m
                d <- gen m
                pure (Q a b c d)
            )
          ]

  shrink (C x) = C <$> shrink x
  shrink (Q a b c d) =
      [a, b, c, d]
    ++ [ Q a' b  c  d | a' <- shrink a ]
    ++ [ Q a  b' c  d | b' <- shrink b ]
    ++ [ Q a  b  c' d | c' <- shrink c ]
    ++ [ Q a  b  c  d' | d' <- shrink d ]

-- Helpers (solo per i test)

-- Matrici di esempio piccole (2x2, nexp=1) in forma esplicita
m22 :: QT Int -> QT Int -> QT Int -> QT Int -> Mat Int
m22 a b c d = Mat 1 (Q a b c d)

i2 :: Mat Int
i2 = m22 (C 1) (C 0) (C 0) (C 1)

z2 :: Mat Int
z2 = m22 (C 0) (C 0) (C 0) (C 0)

a2 :: Mat Int
a2 = m22 (C 1) (C 2) (C 3) (C 4)  -- [[1,2],[3,4]]

b2 :: Mat Int
b2 = m22 (C 5) (C 6) (C 7) (C 8)  -- [[5,6],[7,8]]

spec :: Spec
spec = do
  describe "lowertriangular / uppertriangular / diagonal (casi base)" $ do
    it "Mat 0 (C x) è 1x1: è sia lower che upper che diagonal" $ do
      lowertriangular (Mat 0 (C (2 :: Int))) `shouldBe` True
      uppertriangular (Mat 0 (C (2 :: Int))) `shouldBe` True
      diagonal        (Mat 0 (C (2 :: Int))) `shouldBe` True

    it "Mat 1 (C x) (2x2 omogenea) non è diagonale se x/=0 (comportamento atteso tipico)" $ do
      diagonal (Mat 1 (C (2 :: Int))) `shouldBe` False

    it "identità 2x2 è diagonale" $
      diagonal i2 `shouldBe` True

  describe "matSum" $ do
    it "somma 2x2" $
      matSum a2 b2 `shouldBe` m22 (C 6) (C 8) (C 10) (C 12)

    it "A + 0 = A" $
      matSum a2 z2 `shouldBe` a2

  describe "matMul" $ do
    it "I * A = A" $
      matMul i2 a2 `shouldBe` a2

    it "A * I = A" $
      matMul a2 i2 `shouldBe` a2

    it "prodotto 2x2 noto: [[1,2],[3,4]] * [[5,6],[7,8]] = [[19,22],[43,50]]" $
      matMul a2 b2 `shouldBe` m22 (C 19) (C 22) (C 43) (C 50)

  describe "zong (xA - yI)" $ do
    it "zong 2 3 su A=I2: 2I - 3I = -I" $
      zong 2 3 i2 `shouldBe` m22 (C (-1)) (C 0) (C 0) (C (-1))

  describe "f (vAv^T) / matVec / dot" $ do
    it "caso 1x1 (n=0)" $
      let a = Mat 0 (C 7)          -- [7]
          v = [3 :: Integer]
      in f v a `shouldBe` 63

    it "caso 2x2 (n=1) con matrice generica" $
      let a = Mat 1 (Q (C 1) (C 2)
                      (C 3) (C 4))   -- [[1,2],[3,4]]
          v = [5,6 :: Integer]
      in f v a `shouldBe` 319

    it "caso 2x2 identità: vIv^T = somma dei quadrati" $
      let i = Mat 1 (Q (C 1) (C 0)
                      (C 0) (C 1))   -- I2
          v = [3,4 :: Integer]
      in f v i `shouldBe` (3*3 + 4*4)

  describe "colSums / rowSums (2x2)" $ do
    it "colSums [[1,2],[3,4]] = [4,6]" $
      colSums a2 `shouldBe` [4,6]

    it "rowSums [[1,2],[3,4]] = [3,7]" $
      rowSums a2 `shouldBe` [3,7]

  describe "colMinMax / colVar (2x2)" $ do
    it "colMinMax [[1,2],[3,4]] = [(1,3),(2,4)]" $
      colMinMax a2 `shouldBe` [(1,3),(2,4)]

    it "colVar = max-min" $
      colVar a2 `shouldBe` [2,2]

  describe "colAltSums (2x2)" $ do
    it "colAltSums [[1,2],[3,4]] = [1-3, 2-4] = [-2,-2]" $
      colAltSums a2 `shouldBe` [-2,-2]

  describe "transposeMat / isSymmetric" $ do
    it "transpose [[1,2],[3,4]] = [[1,3],[2,4]]" $
      transposeMat a2 `shouldBe` m22 (C 1) (C 3) (C 2) (C 4)

    it "isSymmetric True per matrice simmetrica 2x2" $
      isSymmetric (m22 (C 1) (C 9) (C 9) (C 2)) `shouldBe` True

    it "isSymmetric False per matrice non simmetrica 2x2" $
      isSymmetric a2 `shouldBe` False

  describe "f (vAv^T) - smoke test su 2x2" $ do
    it "v=[1,1], A=[[1,2],[3,4]] => v A = [4,6], poi dot con v = 10" $
      f ([1,1] :: Vec Int) a2 `shouldBe` 10

  describe "foldMat (sanity)" $ do
    it "foldMat su 1x1: visita foglia con n=0" $
      foldMat
        (\_ a _ _ _ -> a)          -- se è Q, prendi solo ul (non dovrebbe capitare a n=0)
        (\n x -> (n, x :: Int))
        (Mat 0 (C 7))
      `shouldBe` (0,7)

  describe "proprietà base (QuickCheck) su dimensioni compatibili" $ do
    prop "transposeMat (transposeMat A) = A (per alcuni A 2x2)" $
      \(ul :: Int) (ur :: Int) (ll :: Int) (lr :: Int) ->
        let m = m22 (C ul) (C ur) (C ll) (C lr)
        in transposeMat (transposeMat m) == m

    prop "matSum commutativa su 2x2" $
      \(a :: Int) (b :: Int) (c :: Int) (d :: Int)
      (e :: Int) (f' :: Int) (g :: Int) (h :: Int) ->
        let m1 = m22 (C a) (C b) (C c) (C d)
            m2 = m22 (C e) (C f') (C g) (C h)
        in matSum m1 m2 == matSum m2 m1