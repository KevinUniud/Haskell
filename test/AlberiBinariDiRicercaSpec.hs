{-# LANGUAGE ScopedTypeVariables #-}

module AlberiBinariDiRicercaSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.List (nub, sort)

import AlberiBinariDiRicerca
  ( BST(..)
  , WBST(..)
  , RBT(..)
  , Color(..)
  , Bal(..)
  , ABST(..)
  , sumTree
  , sumOddTree
  , samesums
  , bstElem
  , insertInTree
  , bst2List
  , bstSort
  , filtertree
  , annotate
  , almostBalanced
  , insertWBST
  , diff2next
  , levelOrder
  , treeheight
  , annotateFold
  , almostBalancedFold
  , maxDiameter
  , isBST
  , isAVL
  , isRBT
  , bst2ListFold
  , filtertreeFold
  , diff2nextFold
  , limitedVisit
  , shiftTreeToZero
  )

-- Helper (solo per i test)

fromListBST :: Ord a => [a] -> BST a
fromListBST = foldl insertInTree Void

inorder :: BST a -> [a]
inorder Void = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

treeMin :: Ord a => BST a -> a
treeMin (Node x Void _) = x
treeMin (Node _ l    _) = treeMin l
treeMin Void = error "treeMin su albero vuoto"

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Void = Node x Void Void
insertBST x (Node v l r)
  | x <= v    = Node v (insertBST x l) r
  | otherwise = Node v l (insertBST x r)

newtype Distinct a = Distinct { getDistinct :: [a] }
  deriving (Show)

instance (Arbitrary a, Eq a) => Arbitrary (Distinct a) where
  arbitrary = Distinct . nub <$> arbitrary

-- Definizione di riferimento (ricorsiva) per l'altezza
heightRef :: BST a -> Int
heightRef Void = -1
heightRef (Node _ l r) = 1 + max (heightRef l) (heightRef r)

-- Altezza "letta" da un BST annotato (assumendo la tua convenzione)
heightAnn :: BST (a, Int) -> Int
heightAnn Void = -1
heightAnn (Node (_,h) _ _) = h

-- Verifica locale dell'invariante di annotazione
validAnnot :: BST (a, Int) -> Bool
validAnnot Void = True
validAnnot (Node (_,h) l r) =
  h == 1 + max (heightAnn l) (heightAnn r)
  && validAnnot l
  && validAnnot r

spec :: Spec
spec = do
  describe "sumTree / sumOddTree" $ do
    it "sumTree esempio" $
      sumTree (fromListBST [4,2,6,1,3,5,7 :: Integer]) `shouldBe` 28

    it "sumOddTree esempio" $
      sumOddTree (fromListBST [4,2,6,1,3,5,7 :: Integer]) `shouldBe` (1+3+5+7)

    prop "sumOddTree <= sumTree per valori non-negativi" $
      \(xs :: [Integer]) ->
        let ys = map abs xs
            t  = fromListBST ys
        in sumOddTree t <= sumTree t

  describe "samesums" $ do
    it "True se tutte le somme sono uguali" $
      samesums [ fromListBST [1,2,3 :: Integer]
              , fromListBST [6 :: Integer]
              ] `shouldBe` True

    it "False se c'è almeno una somma diversa" $
      samesums [ fromListBST [1,2,3 :: Integer]
              , fromListBST [1,2 :: Integer]
              ] `shouldBe` False

  describe "bstElem / insertInTree" $ do
    prop "dopo insert, l'elemento è presente" $
      \(xs :: [Integer]) (x :: Integer) ->
        let t = fromListBST xs
        in bstElem x (insertInTree t x)

    it "insertInTree: inserisce nel sottoalbero destro-sinistro (esempio concreto)" $
      let t = Node 10 (Node 5 Void Void) (Node 15 Void Void)
      in insertInTree t (12 :: Integer) `shouldBe`
          Node 10
            (Node 5 Void Void)
            (Node 15 (Node 12 Void Void) Void)

  describe "bst2List / bstSort" $ do
    prop "bst2List restituisce una lista ordinata (inorder di BST)" $
      \(xs :: [Integer]) ->
        let t  = fromListBST xs
            ys = bst2List t
        in ys == sort ys

    prop "bstSort equivale a sort . nub" $
      \(xs :: [Integer]) ->
      bstSort xs == sort (nub xs)

  describe "filtertree / filtertreeFold" $ do
    prop "filtertree produce solo elementi che soddisfano p" $
      \(xs :: [Integer]) ->
        let t = fromListBST xs
            ys = filtertree odd t
        in all odd ys

    prop "filtertreeFold coincide con filtertree" $
      \(xs :: [Integer]) ->
        let t = fromListBST xs
        in filtertreeFold odd t == filtertree odd t

  describe "annotate / annotateFold / treeheight" $ do

    prop "treeheight coincide con heightRef" $
      \(Distinct (xs :: [Integer])) ->
        let t = fromListBST xs
        in treeheight t == heightRef t

    prop "annotate e annotateFold producono lo stesso albero annotato" $
      \(Distinct (xs :: [Integer])) ->
        let t = fromListBST xs
        in annotate t == annotateFold t

    prop "annotate produce annotazioni coerenti (invariante locale)" $
      \(Distinct (xs :: [Integer])) ->
        validAnnot (annotate (fromListBST xs))

    prop "l'altezza della radice annotata coincide con treeheight (se non vuoto)" $
      \(Distinct (xs :: [Integer])) ->
        let t = fromListBST xs
        in case annotate t of
             Void -> treeheight t == (-1)
             at   -> treeheight t == heightAnn at

  describe "almostBalanced / almostBalancedFold" $ do
    prop "almostBalancedFold coincide con almostBalanced" $
      \(xs :: [Integer]) ->
        let t = fromListBST xs
        in almostBalancedFold t == almostBalanced t

  describe "insertWBST" $ do
    it "aggiorna correttamente le altezze dopo inserimento" $
      let t =
            WNode 10 1
              (WNode 5 0 WVoid WVoid)
              (WNode 15 0 WVoid WVoid)

      in insertWBST t (12 :: Integer) `shouldBe`
          WNode 10 2
            (WNode 5 0 WVoid WVoid)
            (WNode 15 1
              (WNode 12 0 WVoid WVoid)
              WVoid)

  describe "diff2next / diff2nextFold" $ do
    it "esempio del testo" $
      diff2next (Node 4 Void (Node 7 (Node 5 Void Void) Void))
        `shouldBe`
        Node (4, Just 1) Void (Node (7, Nothing) (Node (5, Just 2) Void Void) Void)

    prop "diff2nextFold coincide con diff2next" $
      \(xs :: [Integer]) ->
        let t = fromListBST xs
        in diff2nextFold t == diff2next t

  describe "levelOrder" $ do
    it "levelOrder su albero vuoto" $
      levelOrder (Void :: BST Integer) `shouldBe` []

    it "levelOrder su albero bilanciato" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in levelOrder t `shouldBe` [4,2,6,1,3,5,7]

    it "levelOrder su albero sbilanciato (solo destra)" $
      let t = fromListBST [1,2,3,4 :: Integer]
      in levelOrder t `shouldBe` [1,2,3,4]

  describe "treeheight (con fold)" $ do
    it "altezza albero vuoto = -1" $
      treeheight (Void :: BST Integer) `shouldBe` (-1)

    it "altezza foglia = 0" $
      let t = Node 10 Void Void
      in treeheight t `shouldBe` 0

    it "altezza albero bilanciato" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in treeheight t `shouldBe` 2

    it "altezza albero sbilanciato (catena destra)" $
      let t = fromListBST [1,2,3,4 :: Integer]
      in treeheight t `shouldBe` 3

  describe "annotateFold" $ do
    it "annotateFold su albero vuoto" $
      annotateFold (Void :: BST Integer) `shouldBe` Void

    it "annotateFold su foglia" $
      let t = Node 10 Void Void
      in annotateFold t `shouldBe`
          Node (10, 0) Void Void

    it "annotateFold su albero bilanciato" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in annotateFold t `shouldBe`
          Node (4,2)
            (Node (2,1)
              (Node (1,0) Void Void)
              (Node (3,0) Void Void))
            (Node (6,1)
              (Node (5,0) Void Void)
              (Node (7,0) Void Void))

  describe "almostBalancedFold" $ do
    it "albero vuoto è almost balanced" $
      almostBalancedFold (Void :: BST Integer) `shouldBe` True

    it "albero bilanciato" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in almostBalancedFold t `shouldBe` True

    it "albero sbilanciato (catena destra)" $
      let t = fromListBST [1,2,3,4 :: Integer]
      in almostBalancedFold t `shouldBe` False

    it "albero quasi bilanciato (differenza 1 ammessa)" $
      let t = fromListBST [2,1,3,4 :: Integer]
      in almostBalancedFold t `shouldBe` True

  describe "maxDiameter" $ do
    it "lista vuota -> 0" $
      maxDiameter ([] :: [BST Integer]) `shouldBe` 0

    it "massimo tra più alberi" $
      let t1 = fromListBST [4,2,6,1,3,5,7 :: Integer]  -- bilanciato: diametro 4
          t2 = fromListBST [1,2,3,4 :: Integer]        -- catena: diametro 3
          t3 = Node 10 Void Void                       -- singolo nodo: diametro 0
      in maxDiameter [t2, t3, t1] `shouldBe` 4

    it "un solo albero" $
      let t = fromListBST [1,2,3,4,5 :: Integer]       -- catena: diametro 4
      in maxDiameter [t] `shouldBe` 4

  describe "isBST" $ do
    it "albero vuoto è BST" $
      isBST (Void :: BST Integer) `shouldBe` True

    it "albero costruito con insert è BST" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in isBST t `shouldBe` True

    it "albero non valido (violazione a sinistra)" $
      let t = Node 10
                (Node 20 Void Void)  -- 20 > 10 → viola proprietà
                (Node 15 Void Void)
      in isBST (t :: BST Integer) `shouldBe` False

    it "albero non valido (violazione profonda)" $
      let t = Node 10
                (Node 5
                  (Node 1 Void Void)
                  (Node 12 Void Void))  -- 12 > 10 ma nel sottoalbero sinistro
                (Node 15 Void Void)
      in isBST (t :: BST Integer) `shouldBe` False

  describe "isAVL" $ do
    it "albero vuoto è AVL" $
      isAVL (AVoid :: ABST Integer) `shouldBe` True

    it "AVL corretto (bilanciato con etichette coerenti)" $
      let t =
            ANode BalEqual 2
              (ANode BalEqual 1 AVoid AVoid)
              (ANode BalEqual 3 AVoid AVoid)
      in isAVL t `shouldBe` True

    it "non AVL: etichetta Bal incoerente" $
      let t =
            ANode BalLeft 2  -- dovrebbe essere BalEqual
              (ANode BalEqual 1 AVoid AVoid)
              (ANode BalEqual 3 AVoid AVoid)
      in isAVL t `shouldBe` False

    it "non AVL: sbilanciamento di altezza > 1" $
      let t =
            ANode BalRight 1
              AVoid
              (ANode BalRight 2
                AVoid
                (ANode BalEqual 3 AVoid AVoid))
      in isAVL t `shouldBe` False

  describe "isRBT" $ do
    it "albero vuoto è RBT" $
      isRBT (RVoid :: RBT Integer) `shouldBe` True

    it "RBT valido (semplice)" $
      let t =
            RNode 2 Black
              (RNode 1 Red  RVoid RVoid)
              (RNode 3 Red  RVoid RVoid)
      in isRBT t `shouldBe` True

    it "non RBT: radice non Black" $
      let t =
            RNode 2 Red
              (RNode 1 Black RVoid RVoid)
              (RNode 3 Black RVoid RVoid)
      in isRBT (t :: RBT Integer) `shouldBe` False

    it "non RBT: nodo Red con figlio Red" $
      let t =
            RNode 2 Black
              (RNode 1 Red (RNode 0 Red RVoid RVoid) RVoid)
              (RNode 3 Black RVoid RVoid)
      in isRBT (t :: RBT Integer) `shouldBe` False

    it "non RBT: black-height diverso nei due sottoalberi" $
      let t =
            RNode 2 Black
              (RNode 1 Black RVoid RVoid)                 -- bh = 1
              (RNode 3 Red (RNode 4 Black RVoid RVoid) RVoid) -- bh = 2
      in isRBT (t :: RBT Integer) `shouldBe` False

    it "non RBT: violazione proprietà BST (sinistra <= x, destra > x)" $
      let t =
            RNode 2 Black
              (RNode 5 Red  RVoid RVoid)  -- 5 > 2 a sinistra
              (RNode 3 Red  RVoid RVoid)
      in isRBT (t :: RBT Integer) `shouldBe` False

  describe "bst2ListFold" $ do
    prop "bst2ListFold coincide con bst2List" $
      \(xs :: [Integer]) ->
        let t = fromListBST xs
        in bst2ListFold t == bst2List t

  describe "filtertreeFold" $ do
    it "filtertreeFold su albero vuoto" $
      filtertreeFold even (Void :: BST Integer) `shouldBe` []

    it "filtertreeFold: mantiene l'ordine inorder dei nodi che soddisfano il predicato" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in filtertreeFold odd t `shouldBe` [1,3,5,7]

    it "filtertreeFold: predicato sempre falso -> lista vuota" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in filtertreeFold (const False) t `shouldBe` []

    it "filtertreeFold: predicato sempre vero -> inorder completo" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in filtertreeFold (const True) t `shouldBe` inorder t

  describe "diff2nextFold" $ do
    it "diff2nextFold su albero vuoto" $
      diff2nextFold (Void :: BST Integer) `shouldBe` Void

    it "diff2nextFold su foglia (nessun successivo)" $
      let t = Node 10 Void Void
      in diff2nextFold t `shouldBe`
          Node (10, Nothing) Void Void

    it "diff2nextFold su albero bilanciato: differenze rispetto al successivo inorder" $
      let t = fromListBST [4,2,6,1,3,5,7 :: Integer]
      in diff2nextFold t `shouldBe`
          Node (4, Just (5 - 4))
            (Node (2, Just (3 - 2))
              (Node (1, Just (2 - 1)) Void Void)
              (Node (3, Just (4 - 3)) Void Void))
            (Node (6, Just (7 - 6))
              (Node (5, Just (6 - 5)) Void Void)
              (Node (7, Nothing) Void Void))

    it "diff2nextFold su albero sbilanciato (solo destra)" $
      let t = fromListBST [1,2,3,4 :: Integer]
      in diff2nextFold t `shouldBe`
          Node (1, Just 1) Void
            (Node (2, Just 1) Void
              (Node (3, Just 1) Void
                (Node (4, Nothing) Void Void)))

  describe "limitedVisit" $ do
    prop "tutti gli elementi risultanti stanno nell'intervallo [x,y]" $
      \(xs :: [Integer]) (a :: Integer) (b :: Integer) ->
        let t = fromListBST xs
            x = min a b
            y = max a b
            ys = limitedVisit x y t
        in all (\v -> v >= x && v <= y) ys

    prop "risultato ordinato" $
      \(xs :: [Integer]) (a :: Integer) (b :: Integer) ->
        let t = fromListBST xs
            x = min a b
            y = max a b
            ys = limitedVisit x y t
        in ys == sort ys

  describe "shiftTreeToZero" $ do
    prop "se non vuoto, il minimo diventa 0 e l'ordine si preserva (shift costante)" $
      \(xs0 :: [Integer]) ->
        let xs = filter (/= 0) xs0
        in not (null xs) ==>
          let t  = fromListBST xs
              m  = treeMin t
              t' = shiftTreeToZero t
              ys = bst2List t'
          in not (null ys) && head ys == 0 && ys == map (\v -> v - m) (bst2List t)

  describe "WBST insertWBST" $ do
    it "inserimento base (smoke test)" $
      insertWBST (WNode 5 1 WVoid WVoid) (3 :: Integer) `shouldSatisfy` (/= WVoid)

  describe "isRBT (smoke tests su invarianti base)" $ do
    it "radice deve essere Black (esempio negativo)" $
      isRBT (RNode (5 :: Integer) Red RVoid RVoid) `shouldBe` False

    it "esempio positivo minimale" $
      isRBT (RNode (5 :: Integer) Black RVoid RVoid) `shouldBe` True

  describe "maxDiameter" $ do
    it "diametro su lista di alberi (smoke test)" $
      maxDiameter [fromListBST [1..7 :: Integer], fromListBST [1..3 :: Integer]] `shouldSatisfy` (>= 0)

  describe "levelOrder" $ do
    it "visita a livelli (esempio semplice)" $
      levelOrder (Node (4 :: Integer) (Node 2 Void Void) (Node 6 Void Void)) `shouldBe` [4,2,6]