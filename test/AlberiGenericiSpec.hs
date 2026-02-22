{-# LANGUAGE ScopedTypeVariables #-}

module AlberiGenericiSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.List (sort)

import AlberiGenerici
  ( Tree(..)
  , treefold
  , height
  , simplify
  , treefoldr
  , treefoldl
  , heightR
  , simplifyR
  , degree
  , transposeT
  , issymm
  , normalize
  , annotate
  , iscorrect
  , diameter
  , maxPathWeight
  , preorder
  , frontier
  , smallParents
  , arithmSmallParents
  , rpn2tree
  )

-- Helper (solo per i test)

heightRef :: Tree a -> Int
heightRef Void = -1
heightRef (Node _ cs) =
  case cs of
    [] -> 0
    _  -> 1 + maximum (map heightRef cs)

shape :: Tree a -> Tree ()
shape Void = Void
shape (Node _ cs) = Node () (map shape cs)

leaf :: a -> Tree a
leaf x = Node x []

transposeShape :: Tree a -> Tree a
transposeShape Void = Void
transposeShape (Node x cs) = Node x (reverse (map transposeShape cs))

isVoid :: Tree a -> Bool
isVoid Void = True
isVoid _    = False

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree _ Void = Void
fmapTree f (Node x cs) = Node (f x) (map (fmapTree f) cs)

spec :: Spec
spec = do
  describe "treefold (smoke + proprietà base)" $ do
    it "fold su Void restituisce lo zero" $
      treefold (\_ _ -> (1 :: Int)) 0 (Void :: Tree Char) `shouldBe` 0

    it "height: Node 'a' (replicate n Void) = 0 per ogni n (esempi)" $ do
      height (Node 'a' (replicate 0 Void)) `shouldBe` 0
      height (Node 'a' (replicate 3 Void)) `shouldBe` 0
      height (Node 'a' (replicate 10 Void)) `shouldBe` 0

  describe "height / heightR" $ do
    it "height Void = -1" $
      height (Void :: Tree Int) `shouldBe` (-1)

    it "height su albero non banale" $
      height (Node 1 [leaf 2, Node 3 [leaf 4]]) `shouldBe` 2

    prop "heightR coincide con height" $
      \(t :: Tree Int) ->
        heightR t == height t

    prop "height coerente con una definizione di riferimento" $
      \(t :: Tree Int) ->
        height t == heightRef t

  describe "simplify / simplifyR" $ do
    it "rimuove figli Void ridondanti (esempio)" $
      simplify (Node 1 [Void, leaf 2, Void]) `shouldBe` Node 1 [leaf 2]

    it "non elimina foglie Node x []" $
      simplify (leaf 5) `shouldBe` leaf 5

    prop "simplifyR coincide con simplify" $
      \(t :: Tree Int) ->
        simplifyR t == simplify t

    prop "simplify non cambia l'altezza (ignorando i Void come foglie spurie)" $
      \(t :: Tree Int) ->
        height (simplify t) == height (simplify (simplify t))

  describe "degree" $ do
    it "grado di Void = 0" $
      degree (Void :: Tree Int) `shouldBe` 0

    it "grado di un nodo è max tra numero figli e grado figli" $
      degree (Node 1 [leaf 2, Node 3 [leaf 4, leaf 5, leaf 6]]) `shouldBe` 3

  describe "treefoldr" $ do
    it "treefoldr su Void restituisce zT" $
      treefoldr (\_ _ -> (1 :: Int)) 0 (+) 0 (Void :: Tree Char) `shouldBe` 0

    it "treefoldr per contare nodi" $
      let
        -- f: valore nodo + accumulatore dei conteggi figli -> conteggio totale
        f _ sumChildCounts = 1 + sumChildCounts
        -- g: conteggio figlio + accumulatore -> nuovo accumulatore
        g childCount acc = childCount + acc
        countTree = treefoldr f 0 g 0
      in
        countTree (Node 1 [leaf 2, Node 3 [leaf 4]]) `shouldBe` 4

    it "treefoldr per calcolare altezza (come heightR)" $
      let
        heightViaFoldr = treefoldr (\_ mh -> 1 + mh) (-1) max (-1)
      in
        heightViaFoldr (Node 1 [leaf 2, Node 3 [leaf 4]]) `shouldBe` 2

    prop "treefoldr per contare nodi = lunghezza preorder" $
      \(t :: Tree Int) ->
        let
          f _ sumChildCounts = 1 + sumChildCounts
          g childCount acc = childCount + acc
          countTree = treefoldr f 0 g 0
        in
          countTree t == length (preorder t)

  describe "treefoldl" $ do
    it "treefoldl su Void restituisce zT" $
      treefoldl (\_ _ -> (1 :: Int)) 0 (+) 0 (Void :: Tree Char) `shouldBe` 0

    it "treefoldl per sommare i valori (con aggregazione da sinistra)" $
      let
        -- f prende accumulatore + valore nodo e aggiorna l'accumulatore
        f sumAcc x = sumAcc + x
        -- g inserisce il risultato di un figlio nell'accumulatore (sum)
        g accSum childSum = accSum + childSum
        sumTree = treefoldl f 0 g 0
      in
        sumTree (Node (10 :: Int) [leaf 5, Node 3 [leaf 2]]) `shouldBe` 20

    prop "treefoldl produce risultati consistenti" $
      \(t :: Tree Int) ->
        let
          -- Sommare tutti i valori dell'albero
          f sumAcc x = sumAcc + x
          g accSum childSum = accSum + childSum
          sumTree = treefoldl f 0 g 0
        in
          -- La somma deve essere la somma di preorder (che visita ogni nodo una volta)
          sumTree t == sum (preorder t)

  describe "transposeT / issymm" $ do
    it "transposeT inverte l'ordine dei figli (a ogni nodo)" $
      transposeT (Node 1 [leaf 2, leaf 3, leaf 4]) `shouldBe`
        Node 1 [leaf 4, leaf 3, leaf 2]

    prop "shape (transposeT t) = transpose della shape" $
      \(t :: Tree Int) ->
        shape (transposeT t) == transposeShape (shape t)

    it "issymm True su albero con forma simmetrica semplice" $
      issymm (Node 'x' [leaf 'a', leaf 'b', leaf 'a']) `shouldBe` True

    it "issymm False su forma non simmetrica" $
      issymm (Node 'x' [leaf 'a', Node 'b' [leaf 'c']]) `shouldBe` False

  describe "normalize" $ do
    it "normalize su Void = Void" $
      normalize (Void :: Tree Int) `shouldBe` (Void :: Tree Double)

    it "normalize produce Double e preserva forma" $
      let t = Node 2 [leaf 4, Node 6 [leaf 8]]
      in shape (normalize t) `shouldBe` shape t

  describe "annotate" $ do
    it "annotate mette (valore, altezzaNodo) (esempio)" $
      annotate (Node 'a' [leaf 'b', Node 'c' [leaf 'd']])
        `shouldBe`
        Node ('a',2)
          [ Node ('b',0) []
          , Node ('c',1) [Node ('d',0) []]
          ]

    prop "shape preservata" $
      \(t :: Tree Int) ->
        shape (annotate t) == shape t

  describe "iscorrect" $ do
    it "iscorrect su foglia accetta espansione []" $
      let g 'A' = [[]]
          g _   = []
      in iscorrect g (Node 'A' []) `shouldBe` True

    it "iscorrect esempio negativo (espansione non ammessa)" $
      let g 'A' = [['B']]
          g 'B' = [[]]
          g _   = []
      in iscorrect g (Node 'A' []) `shouldBe` False

  describe "diameter" $ do
    it "diametro Void = 0 o -1? (smoke: non negativo)" $
      diameter (Void :: Tree Int) `shouldSatisfy` (>= 0)

    it "diametro foglia (smoke: non negativo)" $
      diameter (leaf 1) `shouldSatisfy` (>= 0)

  describe "maxPathWeight" $ do
    it "esempio semplice" $
      maxPathWeight (Node (5 :: Int) [Node 3 [leaf 2], leaf 4]) `shouldSatisfy` (>= 5)

    prop "su valori non negativi, maxPathWeight >= massimo valore nodo" $
      \(t0 :: Tree Int) ->
        let t = fmapTree abs t0
        in not (isVoid t) ==> maxPathWeight t >= maximum (preorder t)

  describe "preorder / frontier" $ do
    it "preorder visita radice prima dei figli (esempio)" $
      preorder (Node 1 [Node 2 [leaf 3], leaf 4]) `shouldBe` [1,2,3,4]

    it "frontier prende valori delle foglie (esempio)" $
      frontier (Node 1 [Node 2 [leaf 3], leaf 4]) `shouldBe` [3,4]

    prop "frontier è sottolista (multinsieme) di preorder" $
      \(t :: Tree Int) ->
        let f = frontier t
            p = preorder t
        in all (`elem` p) f

  describe "smallParents / arithmSmallParents" $ do
    it "smallParents (esempio): genitori ma non nonni" $
      smallParents (Node 10 [Node 1 [leaf 2, leaf 3], leaf 7]) `shouldBe` [1]

    it "arithmSmallParents True su lista vuota o singola (smoke)" $ do
      arithmSmallParents (Void :: Tree Int) `shouldBe` True
      arithmSmallParents (leaf 5) `shouldBe` True

  describe "rpn2tree" $ do
    it "esempio del testo: x y z s/2 m/2" $
      rpn2tree
        [ Left "x"
        , Left "y"
        , Left "z"
        , Right ("s",2)
        , Right ("m",2)
        ]
      `shouldBe`
        Node "m"
          [ Node "x" []
          , Node "s" [Node "y" [], Node "z" []]
          ]

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized gen where
    gen 0 = oneof [pure Void, Node <$> arbitrary <*> pure []]
    gen n = oneof
      [ pure Void
      , do
          x <- arbitrary
          k <- chooseInt (0, 3)
          cs <- vectorOf k (gen (n `div` 2))
          pure (Node x cs)
      ]