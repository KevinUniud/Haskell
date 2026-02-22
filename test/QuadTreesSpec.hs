{-# LANGUAGE ScopedTypeVariables #-}

module QuadTreesSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import QuadTrees
  ( QT(..)
  , Border(..)
  , buildNSimplify
  , simplify
  , qtmap
  , howManyPixels
  , limitAll
  , occurrencies
  , difference
  , overColor
  , fold
  , height
  , qtLength
  , simplifyF
  , qtmapF
  , flipHorizontal
  , flipVertical
  , rotate90Right
  , rotate90Left
  , rotate180
  , isHorizontalSymmetric
  , isVerticalSymmetric
  , isCenterSymmetric
  , elemOrMele
  , isRotatedIn
  , howManyPixelsF
  , occurrenciesF
  , zipWithQT
  , insertPict
  , insertLogo
  , commonPoints
  , framed
  , frame
  )

-- Helpers (solo per i test)

isLeaf :: QT a -> Bool
isLeaf (C _) = True
isLeaf _     = False

qtEqShape :: QT a -> QT b -> Bool
qtEqShape (C _) (C _) = True
qtEqShape (Q a b c d) (Q e f g h) = qtEqShape a e && qtEqShape b f && qtEqShape c g && qtEqShape d h
qtEqShape _ _ = False

isPow4 :: Int -> Bool
isPow4 1 = True
isPow4 k
  | k <= 0        = False
  | k `mod` 4 /= 0 = False
  | otherwise     = isPow4 (k `div` 4)

-- Esempi dal testo
z, u, d2 :: QT Int
z  = C 0
u  = C 1
d2 = C 2

qText1 :: QT Int
qText1 = Q z u u u

qTextBig1 :: QT Int
qTextBig1 = Q qText1 (C 0) (C 2) qText1

qText2 :: QT Int
qText2 = Q d2 u u u

qTextBig2 :: QT Int
qTextBig2 = Q qText2 (C 0) (C 3) qText2

spec :: Spec
spec = do
  describe "buildNSimplify / simplify / simplifyF" $ do
    it "buildNSimplify collassa se tutti i quadranti sono uguali" $
      buildNSimplify (C 7) (C 7) (C 7) (C 7) `shouldBe` (C 7 :: QT Int)

    it "simplify collassa Q (C x) (C x) (C x) (C x) in C x" $
      simplify (Q (C 1) (C 1) (C 1) (C 1) :: QT Int) `shouldBe` C 1

    prop "simplifyF coincide con simplify" $
      \(t :: QT Int) ->
        simplifyF t == simplify t

  describe "qtmap / qtmapF" $ do
    it "qtmap su foglia" $
      qtmap (+1) (C 3 :: QT Int) `shouldBe` C 4

    prop "qtmapF coincide con qtmap" $
      \(t :: QT Int) ->
        qtmapF (+1) t == qtmap (+1) t

  describe "howManyPixels / howManyPixelsF" $ do
    it "esempio testo: howManyPixels (Q q (C0) (C2) q) = 16" $
      howManyPixels qTextBig1 `shouldBe` 16

    prop "howManyPixelsF coincide con howManyPixels" $
      \(t :: QT Int) ->
        howManyPixelsF t == howManyPixels t

    prop "howManyPixels è potenza di 4 (>=1) per QuadTrees non vuoti" $
      \(t :: QT Int) ->
        let n = howManyPixels (simplify t)
        in n >= 1 ==> isPow4 n

  describe "occurrencies / occurrenciesF" $ do
    it "esempio testo: occurrencies (Q q (C0) (C2) q) 0 = 6" $
      occurrencies qTextBig1 0 `shouldBe` 6

    prop "occurrenciesF coincide con occurrencies" $
      \(t :: QT Int) (c :: Int) ->
        occurrenciesF t c == occurrencies t c

    prop "occurrencies <= howManyPixels" $
      \(t :: QT Int) (c :: Int) ->
        let t' = simplify t
        in occurrencies t' c <= howManyPixels t'

  describe "difference / overColor" $ do
    it "esempio: difference = (#>c) - (#<c), gli =c non contano" $
      difference 1 qTextBig2 `shouldBe` 2

    it "esempio testo: overColor 1 (Q q (C0) (C3) q) = 6" $
      overColor 1 qTextBig2 `shouldBe` 6

  describe "limitAll" $ do
    it "limita a c (esempio foglie)" $
      limitAll 2 [C 1, C 2, C 3 :: QT Int] `shouldBe` [C 1, C 2, C 2]

  describe "fold / height / qtLength" $ do
    it "height foglia = 0" $
      height (C True) `shouldBe` 0

    it "qtLength foglia = 1" $
      qtLength (C True) `shouldBe` 1

    it "height di un Q di foglie = 1" $
      height (Q (C 1) (C 2) (C 3) (C 4) :: QT Int) `shouldBe` 1

    prop "qtLength >= 1" $
      \(t :: QT Int) ->
        qtLength t >= 1

    it "fold riproduce qtLength (sanity)" $
      let t = Q (C 1) (C 2) (C 3) (C 4) :: QT Int
      in fold (\a b c d -> 1 + a + b + c + d) (const 1) t `shouldBe` qtLength t

  describe "flip / rotate + predicati di simmetria" $ do
    it "flipHorizontal due volte = identità (su un esempio)" $
      let t = Q (C 1) (C 2) (C 3) (C 4) :: QT Int
      in flipHorizontal (flipHorizontal t) `shouldBe` t

    it "flipVertical due volte = identità (su un esempio)" $
      let t = Q (C 1) (C 2) (C 3) (C 4) :: QT Int
      in flipVertical (flipVertical t) `shouldBe` t

    it "rotate180 = rotate90Right . rotate90Right (su un esempio)" $
      let t = Q (C 1) (C 2) (C 3) (C 4) :: QT Int
      in rotate180 t `shouldBe` rotate90Right (rotate90Right t)

    it "isCenterSymmetric True su foglia" $
      isCenterSymmetric (C 9 :: QT Int) `shouldBe` True

    it "isHorizontalSymmetric True su foglia" $
      isHorizontalSymmetric (C 9 :: QT Int) `shouldBe` True

    it "isVerticalSymmetric True su foglia" $
      isVerticalSymmetric (C 9 :: QT Int) `shouldBe` True

  describe "rotate90Right / rotate90Left / rotate180 (QT)" $ do
    it "rotazioni su foglia: invarianti" $
      let t = C (7 :: Integer)
      in ( rotate90Right t
          , rotate90Left  t
          , rotate180     t
          ) `shouldBe` (C 7, C 7, C 7)

    it "rotate90Right: riordina correttamente i quadranti" $
      let t = Q (C (1::Integer)) (C 2) (C 3) (C 4)   -- ul=1 ur=2 ll=3 lr=4
      in rotate90Right t `shouldBe`
          Q (C 3) (C 1) (C 4) (C 2)

    it "rotate90Left: riordina correttamente i quadranti" $
      let t = Q (C (1::Integer)) (C 2) (C 3) (C 4)
      in rotate90Left t `shouldBe`
          Q (C 2) (C 4) (C 1) (C 3)

    it "rotate180: riordina correttamente i quadranti" $
      let t = Q (C (1::Integer)) (C 2) (C 3) (C 4)
      in rotate180 t `shouldBe`
          Q (C 4) (C 3) (C 2) (C 1)

    it "coerenza: rotate90Right . rotate90Left = id (su QT semplice)" $
      let t = Q (C (1::Integer)) (C 2) (C 3) (C 4)
      in (rotate90Right (rotate90Left t)) `shouldBe` t

    it "coerenza: rotate180 = rotate90Right . rotate90Right (su QT semplice)" $
      let t = Q (C (1::Integer)) (C 2) (C 3) (C 4)
      in (rotate90Right (rotate90Right t)) `shouldBe` rotate180 t

  describe "elemOrMele / isRotatedIn" $ do
    it "elemOrMele: t presente direttamente" $
      elemOrMele (C 1 :: QT Int) [C 0, C 1, C 2] `shouldBe` True

    it "isRotatedIn: foglia è presente (tutte le rotazioni coincidono)" $
      isRotatedIn (C 1 :: QT Int) [C 0, C 1, C 2] `shouldBe` True

  describe "zipWithQT / insertPict / insertLogo / commonPoints" $ do
    it "zipWithQT su foglie" $
      zipWithQT (+) (C 1 :: QT Int) (C 2 :: QT Int) `shouldBe` C 3

    it "insertPict su foglie (maschera True/False)" $ do
      insertPict (C 10 :: QT Int) (C 20 :: QT Int) (C True) `shouldBe` C 10
      insertPict (C 10 :: QT Int) (C 20 :: QT Int) (C False) `shouldBe` C 20

    it "commonPoints su lista di foglie uguali -> foglia True" $
      commonPoints ([C 3, C 3, C 3] :: [QT Int]) `shouldBe` C True

    it "commonPoints su lista di foglie diverse -> foglia False" $
      commonPoints ([C 3, C 4] :: [QT Int]) `shouldBe` C False

  describe "insertLogo" $ do
    it "se qp è una foglia (1x1), resta invariata" $
      let ql   = C (9 :: Integer)
          qp   = C (5 :: Integer)
          mask = C True
      in insertLogo ql qp mask `shouldBe` C 5

    it "inserisce nel quadrante upper-right usando la maschera (tutti True -> prende tutto dal logo)" $
      let ql   = Q (C (1::Integer)) (C 2) (C 3) (C 4)
          qp   = Q (C 10) (Q (C 5) (C 6) (C 7) (C 8)) (C 20) (C 30)
          mask = C True
      in insertLogo ql qp mask `shouldBe`
          Q (C 10)
            (Q (C 1) (C 2) (C 3) (C 4))
            (C 20)
            (C 30)

    it "inserisce nel quadrante upper-right usando la maschera (tutti False -> lascia lo sfondo)" $
      let ql   = Q (C (1::Integer)) (C 2) (C 3) (C 4)
          ur0  = Q (C 5) (C 6) (C 7) (C 8)
          qp   = Q (C 10) ur0 (C 20) (C 30)
          mask = C False
      in insertLogo ql qp mask `shouldBe`
          Q (C 10) ur0 (C 20) (C 30)

    it "maschera mista: prende dal logo solo dove True (su QT 2x2 nell'UR)" $
      let ql   = Q (C (1::Integer)) (C 2) (C 3) (C 4)
          ur0  = Q (C 5) (C 6) (C 7) (C 8)
          qp   = Q (C 10) ur0 (C 20) (C 30)
          mask = Q (C True) (C False) (C False) (C True)
      in insertLogo ql qp mask `shouldBe`
          Q (C 10)
            (Q (C 1) (C 6) (C 7) (C 4))
            (C 20)
            (C 30)

  describe "framed / frame" $ do
    it "frame su foglia restituisce Just colore" $
      frame (C 'x') `shouldBe` Just 'x'

    it "framed su foglia con predicato vero" $
      framed (=='x') (C 'x') `shouldBe` True

instance Arbitrary a => Arbitrary (QT a) where
  arbitrary = sized gen
    where
      gen 0 = C <$> arbitrary
      gen n =
        frequency
          [ (1, C <$> arbitrary)
          , (3, do
                let m = n `div` 2
                q1 <- gen m
                q2 <- gen m
                q3 <- gen m
                q4 <- gen m
                pure (Q q1 q2 q3 q4)
            )
          ]

  shrink (C x) =
    C <$> shrink x

  shrink (Q a b c d) =
      [a, b, c, d]  -- prova a collassare in un sottoalbero
    ++ [ Q a' b  c  d | a' <- shrink a ]
    ++ [ Q a  b' c  d | b' <- shrink b ]
    ++ [ Q a  b  c' d | c' <- shrink c ]
    ++ [ Q a  b  c  d' | d' <- shrink d ]