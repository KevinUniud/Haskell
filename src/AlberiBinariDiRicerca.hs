module AlberiBinariDiRicerca
    ( BST(..)
    , WBST(..)
    , RBT(..)
    , Color(..)
    , Bal(..)
    , ABST(..)
    , sumTree, sumOddTree, samesums, bstElem, insertInTree, bst2List, bstSort
    , filtertree, annotate, almostBalanced, insertWBST, diff2next, levelOrder
    , treeheight, annotateFold, almostBalancedFold, maxDiameter, isAVL, isBST
    , isRBT, bst2ListFold, filtertreeFold, diff2nextFold, limitedVisit, shiftTreeToZero
    ) where

{-
4. ALBERI BINARI DI RICERCA

Si definiscano gli Alberi Binari di Ricerca col seguente tipo di dato astratto (polimorfo)

data ( Ord a , Show a , Read a ) => BST a = Void | Node {
    val :: a ,
    left , right :: BST a
    }
    deriving ( Eq , Ord , Read , Show )

e si usi (per comodità) lo stesso tipo di dato anche per Alberi Binari normali.
-}

-- Definizione di Albero Binario (usato anche come BST).
data BST a = Void 
            | Node {
            val   :: a,
            left  :: BST a,
            right :: BST a
            }
            deriving (Eq, Ord, Read, Show)

{-
Esercizio 1

Scrivere una funzione che calcola la somma dei valori di un albero a valori sommabili.
-}

-- sumTree calcola la somma di tutti i valori contenuti nell’albero.
-- È polimorfa su qualsiasi tipo appartenente alla classe Num.
sumTree :: Num a => BST a -> a

-- Caso base: albero vuoto → somma pari a 0.
sumTree Void = 0

-- Caso ricorsivo:
-- La somma dell’albero è data da:
--   valore del nodo corrente
--   + somma del sottoalbero sinistro
--   + somma del sottoalbero destro
sumTree (Node v l r) =
    v + sumTree l + sumTree r

{-
Esercizio 2

Scrivere una funzione che calcola la somma dei valori dispari di un albero a valori sommabili su cui sia utilizzabile la funzione odd.
-}

-- sumOddTree calcola la somma dei soli valori dispari contenuti nell’albero.
-- Richiede un tipo appartenente alla classe Integral,
-- poiché la funzione odd è definita per tipi integrali.
sumOddTree :: Integral a => BST a -> a

-- Caso base: albero vuoto → somma pari a 0.
sumOddTree Void = 0

-- Caso ricorsivo:
-- Se il valore del nodo è dispari, viene sommato,
-- altrimenti si aggiunge 0.
-- In entrambi i casi si sommano ricorsivamente
-- i risultati dei sottoalberi sinistro e destro.
sumOddTree (Node v l r) =
    (if odd v then v else 0)
    + sumOddTree l
    + sumOddTree r

{-
Esercizio 3

Si scriva un predicato samesums che presa una lista di alberi [t1 , ..., tn ] determina se le somme s1 , ..., sn dei valori degli elementi di ogni ti sono tutte uguali fra loro.
-}

-- samesums verifica se, data una lista di alberi,
-- le somme dei valori di ciascun albero sono tutte uguali.
--
-- Si utilizza la funzione sumTree definita in precedenza.

samesums :: (Num a, Eq a) => [BST a] -> Bool

-- Lista vuota: non ci sono controesempi → True.
samesums [] = True

-- Lista con un solo albero: le somme coincidono trivialmente.
samesums [_] = True

-- Caso generale:
-- Si calcola la somma del primo albero
-- e si verifica che tutte le altre somme coincidano con essa.
samesums (t:ts) =
    let firstSum = sumTree t
    in all (\tree -> sumTree tree == firstSum) ts


{-
Esercizio 4

Scrivere un predicato bstElem (infisso magari) per determinare se un valore è presente in un BST.
-}

-- bstElem verifica se un valore è presente in un
-- Albero Binario di Ricerca (BST).
--
-- Sfrutta la proprietà dei BST:
-- - tutti i valori nel sottoalbero sinistro sono < del nodo
-- - tutti i valori nel sottoalbero destro sono > del nodo

bstElem :: Ord a => a -> BST a -> Bool

-- Caso base: albero vuoto → elemento non presente.
bstElem _ Void = False

bstElem x (Node v l r)
    -- Se il valore coincide con quello del nodo → trovato.
    | x == v = True

    -- Se x è minore, si cerca solo nel sottoalbero sinistro.
    | x < v  = bstElem x l

    -- Se x è maggiore, si cerca solo nel sottoalbero destro.
    | otherwise = bstElem x r

{-
Esercizio 5

Si scriva una funzione per eseguire l’inserimento di un dato x in un albero t.
-}

-- insertInTree inserisce un valore x in un BST,
-- preservando la proprietà di ordinamento.
--
-- Se l’albero è vuoto, si crea un nuovo nodo contenente x.
insertInTree :: Ord a => BST a -> a -> BST a

insertInTree Void x =
    Node x Void Void

insertInTree (Node v l r) x
    -- Se x è minore del valore del nodo,
    -- si inserisce ricorsivamente nel sottoalbero sinistro.
    | x < v =
        Node v (insertInTree l x) r

    -- Se x è maggiore del valore del nodo,
    -- si inserisce nel sottoalbero destro.
    | x > v =
        Node v l (insertInTree r x)

    -- Se x è uguale, si restituisce l’albero invariato
    -- (si evita l’inserimento di duplicati).
    | otherwise = Node v l r

{-
Esercizio 6

Si scriva una funzione bst2List che calcola la lista ordinata degli elementi di un BST.
Ci si assicuri di scrivere una funzione lineare.
-}

-- bst2List restituisce la lista ordinata degli elementi di un BST.
-- Si utilizza una visita in-order:
--   sinistra → nodo → destra

bst2List :: BST a -> [a]
bst2List t = bst2ListAcc t []
    where
        -- bst2ListAcc visita l’albero in-order
        -- accumulando il risultato in modo efficiente.
        bst2ListAcc Void acc = acc
        bst2ListAcc (Node v l r) acc =
            bst2ListAcc l (v : bst2ListAcc r acc)

{-
Esercizio 7

Si scriva una (semplice) funzione di ordinamento di liste come combinazione di funzioni fatte nei precedenti esercizi.
-}

-- bstSort ordina una lista costruendo prima un BST
-- e poi trasformandolo in lista ordinata.
--
-- 1) foldr (flip insertInTree) Void xs
--    inserisce tutti gli elementi della lista nel BST,
--    partendo dall’albero vuoto.
--
-- 2) bst2List esegue una visita in-order,
--    producendo la lista ordinata.

bstSort :: Ord a => [a] -> [a]
bstSort xs =
    bst2List (foldr (flip insertInTree) Void xs)

{-
Esercizio 8

Si scriva una funzione filtertree p t che costruisce una lista (ordinata) di tutti gli elementi dell’albero t che soddisfano il predicato p.
-}

-- filtertree costruisce la lista ordinata degli elementi
-- di un BST che soddisfano il predicato p.
--
-- Si utilizza una visita in-order (sinistra → nodo → destra)
-- per mantenere l’ordinamento.
-- Per evitare l’uso ripetuto di (++), si usa un accumulatore.

filtertree :: (a -> Bool) -> BST a -> [a]
filtertree p t = filterAcc t []
    where
        filterAcc Void acc = acc

        filterAcc (Node v l r) acc =
            filterAcc l
                (if p v
                    then v : filterAcc r acc
                    else filterAcc r acc)

{-
Esercizio 9

Si scriva una funzione annotate che costruisca un nuovo BST che in ogni nodo contenga, al posto del valore originale, una coppia composta dal medesimo valore e dall’altezza del nodo stesso (la lunghezza del massimo cammino, cioè 1 + max(height(sx), height(dx)).

Si scelga di attribuire all’albero vuoto 0 o -1 a seconda delle preferenze.
[Con una opportuna scelta dell’ordine di ricorsione si può fare in tempo lineare]
-}

-- annotateTree restituisce una coppia:
--   1) un nuovo albero con valori annotati (valore originale, altezza del nodo)
--   2) l’altezza del nodo corrente (utile per evitare ricalcoli)
--
-- Scelta: altezza dell’albero vuoto = -1.
-- In questo modo una foglia avrà altezza 0, perché:
--   1 + max(-1, -1) = 0
annotateTree :: BST a -> (BST (a, Int), Int)

-- Caso base: albero vuoto → albero vuoto annotato e altezza -1.
annotateTree Void = (Void, -1)

annotateTree (Node v l r) =
    -- Si annotano ricorsivamente i due sottoalberi ottenendo anche le loro altezze.
    let (lAnn, hL) = annotateTree l
        (rAnn, hR) = annotateTree r

        -- Altezza del nodo corrente:
        -- 1 + max(altezza_sinistra, altezza_destra)
        h = 1 + max hL hR

        -- Si costruisce il nodo annotato con (valore, altezza_del_nodo).
    in (Node (v, h) lAnn rAnn, h)

-- annotate è la funzione richiesta: restituisce solo l’albero annotato,
-- scartando l’altezza complessiva (secondo elemento della coppia).
annotate :: BST a -> BST (a, Int)
annotate t = fst (annotateTree t)


{-
Esercizio 10

Si scriva un predicato (funzione a valori booleani) almostBalanced per determinare se un albero binario ha la seguente proprietà: per ogni nodo le altezze dei figli destro e sinistro differiscono al massimo di 1.
-}

-- almostBalanced verifica se un albero binario soddisfa la proprietà:
-- per ogni nodo, le altezze dei sottoalberi sinistro e destro
-- differiscono al massimo di 1.
--
-- La funzione è lineare (O(n)) perché ogni nodo viene visitato una sola volta.
-- Si calcolano contemporaneamente:
--   1) se il sottoalbero è almost balanced
--   2) la sua altezza
--
-- Convenzione adottata:
--   altezza dell’albero vuoto = -1
--   quindi una foglia ha altezza 0

almostBalanced :: BST a -> Bool
almostBalanced t = fst (check t)
    where
        -- check restituisce una coppia:
        --   (èAlmostBalanced, altezza)
        check :: BST a -> (Bool, Int)

        -- Caso base: albero vuoto.
        -- È bilanciato e ha altezza -1.
        check Void = (True, -1)

        check (Node _ l r) =
            -- Si analizzano ricorsivamente i sottoalberi sinistro e destro.
            let (okL, hL) = check l
                (okR, hR) = check r

                -- Il nodo corrente è bilanciato se
                -- la differenza tra le altezze è al massimo 1.
                okHere = abs (hL - hR) <= 1

                -- Altezza del nodo corrente:
                -- 1 + max(altezza_sinistra, altezza_destra)
                hHere = 1 + max hL hR

            -- L’intero sottoalbero è almost balanced se:
            --   - il sinistro è almost balanced
            --   - il destro è almost balanced
            --   - il nodo corrente è bilanciato
            in (okL && okR && okHere, hHere)

{-
Esercizio 11

Data la seguente definizione del tipo di dato astratto (polimorfo) Weighted Binary Search Tree che consiste in un BST in cui in ogni nodo viene mantenuta l’altezza del nodo stesso.

data WBST a = Void | Node a Int ( WBST a ) ( WBST a )

Si scriva una funzione insert che inserisce un nuovo valore in un WBST.
-}

-- Definizione di Weighted BST:
-- ogni nodo contiene anche l’altezza del nodo stesso.
data WBST a
    = WVoid
    | WNode a Int (WBST a) (WBST a)
    deriving (Eq, Ord, Read, Show)

-- wbstHeight estrae l’altezza salvata nel nodo.
-- Convenzione: altezza dell’albero vuoto = -1 (foglia = 0).
wbstHeight :: WBST a -> Int
wbstHeight WVoid           = -1
wbstHeight (WNode _ h _ _) = h

-- insertWBST inserisce un valore x in un WBST preservando la proprietà BST.
-- Dopo l’inserimento aggiorna l’altezza del nodo corrente usando le altezze
-- (già memorizzate) dei due figli.
insertWBST :: Ord a => WBST a -> a -> WBST a

-- Caso base: inserimento in albero vuoto → si crea una foglia di altezza 0.
insertWBST WVoid x = WNode x 0 WVoid WVoid

insertWBST (WNode v h l r) x
    -- Se x è minore del valore del nodo, si inserisce nel sottoalbero sinistro.
    | x < v =
        let newLeft   = insertWBST l x
            -- La nuova altezza dipende dalle altezze dei due figli:
            -- 1 + max(height(sx), height(dx)).
            newHeight = 1 + max (wbstHeight newLeft) (wbstHeight r)
        in WNode v newHeight newLeft r

    -- Altrimenti (x >= v) si inserisce nel sottoalbero destro.
    -- Nota: così facendo, eventuali duplicati finiscono a destra.
    | otherwise =
        let newRight  = insertWBST r x
            newHeight = 1 + max (wbstHeight l) (wbstHeight newRight)
        in WNode v newHeight l newRight

{-
Esercizio 12

Si scriva una funzione diff2next che, dato un albero binario di ricerca, costruisce un albero binario di ricerca (annotato) di coppie dove il primo elemento di ogni coppia è l’elemento dell’albero originale mentre il secondo elemento è Just(la differenza rispetto al valore successivo), secondo l’ordinamento dei valori contenuti, oppure Nothing per il nodo di valore massimo.
A titolo di esempio,

Node 4 Void (Node 7 (Node 5 Void Void) Void)

restituisce la soluzione

Node (4,Just 1) Void (Node (7,Nothing) (Node (5,Just 2) Void Void) Void).
-}

-- diff2next costruisce un BST con la stessa forma dell’albero originale,
-- ma in ogni nodo mette una coppia:
--   (valoreOriginale, Just(diffRispettoAlSuccessivo)) oppure Nothing se non esiste successivo.
--
-- Per calcolare il “valore successivo” (in-order), si fa una visita in-order 
-- inversa (destra → nodo → sinistra), mantenendo come stato il valore visitato 
-- “subito prima” in questa visita, che corrisponde al successivo (più grande) 
-- nell’ordinamento normale.

diff2next :: (Num a, Ord a) => BST a -> BST (a, Maybe a)
diff2next t = fst (go t Nothing)
    where
        -- go t next:
        --   next è il valore successivo (più grande) già noto per i nodi che stiamo per visitare
        --   (cioè l’ultimo valore visto nella visita destra→nodo→sinistra).
        --
        -- Restituisce:
        --   1) l’albero annotato
        --   2) il nuovo stato "next" aggiornato dopo aver processato tutto il sottoalbero
        go :: (Num a, Ord a) => BST a -> Maybe a -> (BST (a, Maybe a), Maybe a)

        -- Albero vuoto: nessun nodo da annotare, lo stato resta invariato.
        go Void next = (Void, next)

        go (Node v l r) next =
        -- 1) Visita del sottoalbero destro: qui troviamo prima i valori più grandi.
        --    Il valore "next1" risultante è il successivo di v (se esiste).
            let (rAnn, next1) = go r next

                -- 2) Annotazione del nodo corrente:
                --    se next1 = Just s, allora la differenza è s - v
                --    se next1 = Nothing, v è il massimo e la differenza è Nothing
                diff = fmap (\s -> s - v) next1

                -- 3) Dopo aver “visitato” v, per il sottoalbero sinistro
                --    il successivo diventa proprio v.
                (lAnn, next2) = go l (Just v)

            -- Si ricostruisce il nodo con i sottoalberi annotati.
            in (Node (v, diff) lAnn rAnn, next2)

{-
Esercizio 13

Si scriva una funzione che dato un BST ne restituisce la lista degli elementi ottenuti visitando l’albero a livelli.
Si consideri d’ora in poi la seguente generalizzazione a BST della funzione foldr su liste:

fold :: ( Ord a ) = > ( a -> b -> b -> b ) -> b -> BST a -> b
fold _ z Void = z
fold f z ( Node x l r ) = f x ( fold f z l ) ( fold f z r )

Ci si assicuri di scrivere funzioni lineari (non ha senso scrivere soluzioni che usino “forzosamente” una fold).
-}

-- levelOrder restituisce la lista dei valori di un BST
-- visitato per livelli (breadth-first).

levelOrder :: BST a -> [a]
levelOrder t = bfs (enqueue t ([], []))
    where
        -- enqueue inserisce un elemento in coda.
        enqueue :: x -> ([x], [x]) -> ([x], [x])
        enqueue x (front, back) = (front, x : back)

        -- dequeue estrae un elemento dalla coda.
        dequeue :: ([x], [x]) -> Maybe (x, ([x], [x]))
        dequeue (x:xs, back) = Just (x, (xs, back))
        dequeue ([], back) =
            case reverse back of
                []     -> Nothing
                (x:xs) -> Just (x, (xs, []))

        -- bfs esegue la visita per livelli.
        bfs :: ([BST a], [BST a]) -> [a]
        bfs q =
            case dequeue q of
                -- Coda vuota → fine visita.
                Nothing -> []

                -- Nodo vuoto → si ignora.
                Just (Void, q') ->
                    bfs q'

                -- Nodo non vuoto:
                -- si emette il valore e si accodano i figli.
                Just (Node v l r, q') ->
                    v : bfs (enqueue r (enqueue l q'))

{-
Esercizio 14

Si scriva una funzione treeheight per calcolare l’altezza di un albero usando opportunamente fold.
-}

-- fold generalizza l’idea di foldr sulle liste agli alberi binari.
-- Per ogni nodo applica la funzione f al valore del nodo
-- e ai risultati ottenuti ricorsivamente dai sottoalberi.
fold :: Ord a => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void = z
fold f z (Node x l r) =
    f x (fold f z l) (fold f z r)


-- treeheight calcola l’altezza di un albero usando fold.
--
-- Convenzione adottata:
--   altezza dell’albero vuoto = -1
--   quindi una foglia ha altezza 0.
--
-- Per ottenere questa convenzione:
--   - si usa come valore base z = -1
--   - per ogni nodo si calcola 1 + max(altezza_sx, altezza_dx)
treeheight :: Ord a => BST a -> Int
treeheight =
    fold (\_ lh rh -> 1 + max lh rh) (-1)

{-
Esercizio 15

Si riscriva la funzione annotate dell’Esercizio 9 usando opportunamente fold.
-}

-- annotateFold riscrive la funzione annotate usando fold.
--
-- Idea:
-- fold costruisce ricorsivamente un nuovo albero.
-- Per ogni nodo abbiamo già i due sottoalberi annotati (lt e rt),
-- quindi possiamo ricavare direttamente le loro altezze
-- leggendo il secondo elemento della coppia memorizzata nel nodo.

annotateFold :: Ord a => BST a -> BST (a, Int)
annotateFold =
    fold build Void
    where
        -- build riceve:
        --   v  = valore originale del nodo
        --   lt = sottoalbero sinistro già annotato
        --   rt = sottoalbero destro già annotato
        --
        -- Costruisce il nuovo nodo annotato.
        build v lt rt =
            let h = 1 + max (height lt) (height rt)
            in Node (v, h) lt rt

        -- height estrae l’altezza dal nodo annotato.
        -- Convenzione: altezza di Void = -1.
        height Void = -1
        height (Node (_, h) _ _) = h

{-
Esercizio 16

Si riscriva la funzione almostBalanced dell’Esercizio 10 usando opportunamente fold.
-}

-- almostBalancedFold verifica la proprietà “almost balanced”
-- usando la fold sugli alberi.
--
-- Strategia:
-- come nella versione lineare, per ogni sottoalbero calcoliamo insieme:
--   1) se è bilanciato
--   2) la sua altezza
--
-- La fold combina i risultati dei sottoalberi sinistro e destro in un solo passaggio.
-- Convenzione: altezza di Void = -1 (foglia = 0).

almostBalancedFold :: Ord a => BST a -> Bool
almostBalancedFold =
    fst . fold step (True, -1)
    where
        -- step combina:
        --   x  = valore del nodo (qui non serve)
        --   (okL, hL) = risultato del sottoalbero sinistro
        --   (okR, hR) = risultato del sottoalbero destro
        --
        -- Restituisce (okHere, hHere) per il nodo corrente.
        step _ (okL, hL) (okR, hR) =
            let okHere = okL && okR && abs (hL - hR) <= 1
                hHere  = 1 + max hL hR
            in (okHere, hHere)

{-
Esercizio 17

Si scriva una funzione maxDiameter che data una lista l di BST determina il massimo dei dia- metri dei BST di l.
Il diametro di un BST è la lunghezza del massimo cammino fra due nodi, indipendentemente dall’orientamento degli archi.
-}

-- diameter calcola il diametro di un albero:
-- la lunghezza (numero di archi) del massimo cammino fra due nodi qualsiasi.
--
-- Implementazione lineare: in un’unica visita ricorsiva calcola
-- sia il diametro sia l’altezza del sottoalbero.
--
-- Convenzioni:
--   altezza(Void) = -1  (foglia = 0)
--   diametro(Void) = 0  (nessun arco)
diameter :: BST a -> Int
diameter t = fst (go t)
    where
        -- go restituisce (diametro, altezza)
        go :: BST a -> (Int, Int)
        go Void = (0, -1)
        go (Node _ l r) =
            let (dL, hL) = go l
                (dR, hR) = go r

                -- altezza del nodo corrente
                hHere = 1 + max hL hR

                -- diametro che passa per il nodo corrente:
                -- percorso che va dalla foglia più profonda a sinistra
                -- alla foglia più profonda a destra (in numero di archi).
                throughHere = hL + hR + 2

                -- diametro complessivo: massimo tra quello “interno” ai figli
                -- e quello che passa per il nodo corrente.
                dHere = max throughHere (max dL dR)
            in (dHere, hHere)

-- maxDiameter prende una lista di alberi e restituisce
-- il massimo dei loro diametri.
maxDiameter :: [BST a] -> Int
maxDiameter [] = 0
maxDiameter ts = maximum (map diameter ts)

{-
Esercizio 18

Si scriva un predicato isBST, usando opportunamente fold, che dato un albero verifica se i valori in esso contenuti soddisfano la proprietà strutturale dei Binary Search Trees.
-}

-- isBST verifica che l’albero soddisfi la proprietà dei BST:
-- per ogni nodo x:
--   - tutti i valori nel sottoalbero sinistro sono < x
--   - tutti i valori nel sottoalbero destro sono > x
--
-- Si usa fold per calcolare in un’unica visita:
--   1) se il sottoalbero è un BST
--   2) il minimo valore del sottoalbero (se esiste)
--   3) il massimo valore del sottoalbero (se esiste)
--
-- Questo evita di costruire liste e rimane lineare.

isBST :: Ord a => BST a -> Bool
isBST = fst3 . fold step (True, Nothing, Nothing)
    where
        -- Estrae il booleano dal triplo (isBST, min, max)
        fst3 (b, _, _) = b

        -- step combina le informazioni dei due sottoalberi per il nodo x.
        --
        -- (okL, minL, maxL) = info del sottoalbero sinistro
        -- (okR, minR, maxR) = info del sottoalbero destro
        --
        -- Il nodo è valido se:
        --   - sinistro e destro sono BST
        --   - max del sinistro (se esiste) è < x
        --   - min del destro  (se esiste) è > x
        --
        -- Il nuovo minimo è il min tra minL e x (se minL esiste), altrimenti x.
        -- Il nuovo massimo è il max tra maxR e x (se maxR esiste), altrimenti x.
        step x (okL, minL, maxL) (okR, minR, maxR) =
            let leftOk  = okL && maybe True (< x) maxL
                rightOk = okR && maybe True (> x) minR
                okHere  = leftOk && rightOk

                newMin = Just (maybe x (`min` x) minL)
                newMax = Just (maybe x (`max` x) maxR)
            in (okHere, newMin, newMax)

{-
Esercizio 19

Si scriva un predicato isAVL che dato un albero secondo la seguente definizione di tipo

data ( Ord a ) = > ABST a = Void | Node Bal a ( ABST a ) ( ABST a )
    deriving ( Eq , Ord , Read , Show )
data Bal = Left | Bal | Right deriving ( Eq , Ord , Read , Show )

determina se è ben formato, cioè se
- la differenza fra le profondità dei sottoalberi destro e sinistro di un qualunque nodo è al massimo 1;
- le etichette Bal dei nodi sono consistenti con lo (s)bilanciamento.
-}

-- Tipo per il fattore di bilanciamento:
-- BalLeft  : sottoalbero sinistro più alto di 1
-- BalEqual : stessa altezza
-- BalRight : sottoalbero destro più alto di 1
data Bal = BalLeft | BalEqual | BalRight
    deriving (Eq, Ord, Read, Show)

-- Albero binario “annotato” con Bal in ogni nodo.
data ABST a = AVoid | ANode Bal a (ABST a) (ABST a)
    deriving (Eq, Ord, Read, Show)

-- isAVL verifica che l’albero sia ben formato come AVL:
-- 1) per ogni nodo |hL - hR| <= 1
-- 2) l’etichetta Bal è coerente con il confronto tra hL e hR
--
-- Implementazione lineare: in un’unica visita calcola
-- (ok, altezza). Convenzione: altezza(AVoid) = -1 (foglia = 0).
isAVL :: ABST a -> Bool
isAVL t = fst (check t)
    where
        -- check restituisce:
        --   (ilSottoalberoÈAVL, altezzaDelSottoalbero)
        check :: ABST a -> (Bool, Int)
        check AVoid = (True, -1)

        check (ANode bal _ l r) =
            let (okL, hL) = check l
                (okR, hR) = check r

                -- Condizione AVL sulle altezze
                heightOk = abs (hL - hR) <= 1

                -- Etichetta attesa in base al confronto tra altezze
                expectedBal
                    | hL > hR  = BalLeft
                    | hL < hR  = BalRight
                    | otherwise = BalEqual

                -- Etichetta coerente
                labelOk = bal == expectedBal

                -- Altezza del nodo corrente
                hHere = 1 + max hL hR

                okHere = okL && okR && heightOk && labelOk
            in (okHere, hHere)

{-
Esercizio 20

Si scriva un predicato isRBT che dato un albero secondo la seguente definizione di tipo

data ( Ord a ) = > RBT a = Void | Node a Color ( RBT a ) ( RBT a )
    deriving ( Eq , Ord , Read , Show )
data Color = Red | Black deriving ( Eq , Ord , Read , Show )

determina se è ben formato, cioè se
- ogni nodo contiene un valore non minore dei valori del suo sottoalbero sinistro e minore dei valori del sottoalbero destro;
- tutti i cammini dalla radice a una foglia hanno lo stesso numero di nodi Black;
- i nodi Red devono avere genitore Black;
- la radice è Black.
-}

-- Colori dei nodi Red-Black.
data Color = Red | Black
    deriving (Eq, Ord, Read, Show)

-- Red-Black Tree: come BST + colore in ogni nodo.
data RBT a = RVoid | RNode a Color (RBT a) (RBT a)
    deriving (Eq, Ord, Read, Show)

-- isRBT verifica che un RBT sia ben formato:
-- 1) Proprietà BST (qui: sinistra <= x, destra > x)
-- 2) La radice è Black (se non vuota)
-- 3) Nessun nodo Red ha figli Red (equivalente: ogni Red ha genitore Black)
-- 4) Tutti i cammini radice→foglia hanno lo stesso numero di nodi Black (black-height)
--
-- Implementazione lineare:
-- in un’unica visita calcola (ok, min, max, blackHeight, rootColor)
-- usando Maybe per gestire l’albero vuoto.

isRBT :: Ord a => RBT a -> Bool
isRBT t =
    case t of
        RVoid -> True
        RNode _ c _ _ -> c == Black && ok
    where
        ok = case check t of
            (True, _, _, _, _) -> True
            _                  -> False

        -- check restituisce:
        -- (èValido, minimo, massimo, blackHeight, coloreRadiceDelSottoalbero)
        --
        -- minimo/massimo servono a verificare la proprietà BST senza costruire liste.
        check :: Ord a => RBT a -> (Bool, Maybe a, Maybe a, Int, Maybe Color)
        check RVoid = (True, Nothing, Nothing, 0, Nothing)
        -- Nota: per RVoid si usa blackHeight = 0 come numero di nodi Black “sotto”
        -- la foglia vuota (sentinel). È una convenzione comune e funziona bene.

        check (RNode x col l r) =
            let (okL, minL, maxL, bhL, rootColL) = check l
                (okR, minR, maxR, bhR, rootColR) = check r

                -- Proprietà BST richiesta:
                -- tutti i valori a sinistra <= x, tutti quelli a destra > x.
                bstOkLeft  = maybe True (<= x) maxL
                bstOkRight = maybe True (>  x) minR
                bstOk      = bstOkLeft && bstOkRight

                -- Vincolo Red: un nodo Red non può avere figli Red.
                redOk =
                    col == Black
                    || (rootColL /= Just Red && rootColR /= Just Red)

                -- Vincolo black-height: i due sottoalberi devono avere lo stesso bh.
                bhOk = bhL == bhR

                -- Black-height del nodo corrente: se il nodo è Black aggiunge 1.
                bhHere = bhL + if col == Black then 1 else 0

                -- Min e max del sottoalbero corrente (per propagare info BST).
                minHere = case minL of
                            Just m  -> Just m
                            Nothing -> Just x
                maxHere = case maxR of
                            Just m  -> Just m
                            Nothing -> Just x

                okHere = okL && okR && bstOk && redOk && bhOk
            in (okHere, minHere, maxHere, bhHere, Just col)

{-
Esercizio 21

Si riscriva la funzione bst2List dell’Esercizio 6 usando opportunamente fold.
Difficile quanto l’Esercizio 6 delle liste
-}

-- bst2ListFold riscrive bst2List usando fold.
--
-- Per ottenere una versione lineare (come nell’esercizio 6),
-- non si deve usare (++), che renderebbe la funzione quadratica.
--
-- Si usa quindi una “difference list”:
-- invece di costruire direttamente una lista [a],
-- si costruisce una funzione [a] -> [a].
--
-- Alla fine si applica la funzione alla lista vuota.

bst2ListFold :: Ord a => BST a -> [a]
bst2ListFold t = fold step id t []
    where
        -- step combina:
        --   x     = valore del nodo
        --   lFun  = funzione che costruisce la lista del sottoalbero sinistro
        --   rFun  = funzione che costruisce la lista del sottoalbero destro
        --
        -- Costruiamo una nuova funzione che,
        -- data una lista acc, produce:
        --   l ++ (x : r ++ acc)
        --
        -- Senza usare (++), ma solo composizione.
        step x lFun rFun =
            lFun . (x :) . rFun


{-
Esercizio 22

Si riscriva la funzione filtertree dell’Esercizio 8 usando opportunamente fold.
-}

-- filtertreeFold riscrive filtertree usando fold.
--
-- Come per bst2ListFold, per ottenere una soluzione lineare
-- si evita l’uso di (++), costruendo una difference list
-- (cioè una funzione [a] -> [a]).
--
-- Alla fine si applica la funzione risultante alla lista vuota.

filtertreeFold :: Ord a => (a -> Bool) -> BST a -> [a]
filtertreeFold p t = fold step id t []
    where
        -- step combina:
        --   x     = valore del nodo
        --   lFun  = funzione che costruisce la lista filtrata del sottoalbero sinistro
        --   rFun  = funzione che costruisce la lista filtrata del sottoalbero destro
        --
        -- Se p x è vero, x viene inserito tra sinistra e destra,
        -- altrimenti viene scartato.
        step x lFun rFun =
            if p x
                then lFun . (x :) . rFun
                else lFun . rFun


{-
Esercizio 23

Si riscriva la funzione diff2next dell’Esercizio 12 usando opportunamente fold.
-}

-- diff2nextFold riscrive diff2next usando fold.
--
-- Idea (lineare, senza mappe né lookup):
-- si usa fold per calcolare una funzione di stato, cioè una “difference tree”
-- che, dato il valore successivo (Maybe a), restituisce:
--   1) l’albero annotato per quel sottoalbero
--   2) il nuovo valore “successivo” da passare al padre (cioè il massimo del sottoalbero).
--
-- Per ottenere la differenza rispetto al successivo in-order,
-- si visita logicamente in ordine inverso (destra → nodo → sinistra).
-- Con fold (che fornisce i risultati dei due sottoalberi), si ottiene
-- combinando prima rt (destra), poi il nodo, poi lt (sinistra).

diff2nextFold :: (Num a, Ord a) => BST a -> BST (a, Maybe a)
diff2nextFold t = fst (builder Nothing)
    where
        -- builder :: Maybe a -> (BST (a, Maybe a), Maybe a)
        -- dove l’argomento è il successivo (più grande) esterno al sottoalbero.
        builder =
            fold step base t

        -- Caso base (Void): non produce nodi e non modifica lo stato.
        base :: Maybe a -> (BST (a, Maybe a), Maybe a)
        base next = (Void, next)

        -- step costruisce la funzione di stato per un nodo con valore x,
        -- avendo già:
        --   lt = builder per il sottoalbero sinistro
        --   rt = builder per il sottoalbero destro
        step
            :: (Num a, Ord a)
            => a
            -> (Maybe a -> (BST (a, Maybe a), Maybe a))  -- lt
            -> (Maybe a -> (BST (a, Maybe a), Maybe a))  -- rt
            -> (Maybe a -> (BST (a, Maybe a), Maybe a))  -- risultato
        step x lt rt next =
            let
                -- Prima si processa il destro con il successivo esterno.
                (rAnn, next1) = rt next

                -- next1 è il successivo di x (se esiste).
                diff = fmap (\s -> s - x) next1

                -- Poi si processa il sinistro, passando come successivo proprio x.
                (lAnn, next2) = lt (Just x)
            in
                -- Si ricostruisce il nodo annotato e si propaga verso l’alto next2,
                -- che rappresenta il massimo del sottoalbero corrente.
                (Node (x, diff) lAnn rAnn, next2)


{-
Esercizio 24

Si scriva una funzione limitedVisit che dato un BST e due valori x, y costruisce la lista (ordinata) degli elementi dell’albero compresi nell’intervallo di valori da x a y.
Garantire che let d=d in limitedVisit 3 7 (Node 7 (Node 2 (Node d Void Void) Void) (Node d Void Void)) termini non è immediato.
-}

-- limitedVisit restituisce la lista ordinata degli elementi del BST
-- compresi nell’intervallo [x, y].
--
-- Sfrutta la proprietà dei BST per evitare visite inutili:
-- - se v < x, tutto il sottoalbero sinistro contiene valori <= v < x,
--   quindi può essere scartato; si visita solo il destro.
-- - se v > y, tutto il sottoalbero destro contiene valori > v > y,
--   quindi può essere scartato; si visita solo il sinistro.
-- - se x <= v <= y, si visita entrambi i sottoalberi e si include v.
--
-- Per mantenere complessità lineare e buona produttività con la laziness,
-- si usa una difference list ([a] -> [a]) tramite un accumulatore `acc`,
-- evitando l’uso di (++).

limitedVisit :: Ord a => a -> a -> BST a -> [a]
limitedVisit x y t = go t []
    where
        -- go visita l’albero e costruisce il risultato come difference list:
        -- dato un accumulatore acc, restituisce (risultato ++ acc).
        go Void acc = acc

        go (Node v l r) acc
        -- v è troppo piccolo: si scarta tutto il sinistro e si continua a destra.
            | v < x =
                go r acc

            -- v è troppo grande: si scarta tutto il destro e si continua a sinistra.
            | v > y =
                go l acc

            -- v è nell’intervallo: visita in-order limitata.
            -- Si costruisce:
            --   (valori validi nel sinistro)
            --   ++ [v]
            --   ++ (valori validi nel destro)
            -- senza usare (++), ma solo cons e composizione tramite acc.
            | otherwise =
                go l (v : go r acc)


{-
Esercizio 25

Si scriva una funzione shiftToZero che dato un BST t costruisce un nuovo BST isomorfo che contiene gli elementi t diminuiti del valore minimo di t.
La funzione non deve visitare un nodo dell’albero t più di una volta (si sfrutti laziness e scoping mutuamente ricorsivo).
Difficile quanto l’Esercizio 7 delle liste
-}

-- findMinBST trova il valore minimo in un BST.
-- In un BST il minimo si trova seguendo sempre il figlio sinistro.
findMinBST :: Ord a => BST a -> a
findMinBST Void               = error "Empty tree"
findMinBST (Node x Void _)    = x
findMinBST (Node _ leftSub _) = findMinBST leftSub


-- mapBST applica una funzione a tutti i valori contenuti nell’albero,
-- mantenendo invariata la struttura (albero isomorfo).
mapBST :: (a -> b) -> BST a -> BST b
mapBST _ Void = Void
mapBST f (Node x l r) =
    Node (f x) (mapBST f l) (mapBST f r)


-- shiftTreeToZero costruisce un nuovo BST isomorfo a t
-- in cui ogni valore è diminuito del valore minimo di t.
--
-- Requisito “non visitare un nodo più di una volta”:
-- - il minimo minVal viene calcolato con findMinBST, visitando solo il cammino sinistro
--   fino al minimo (non tutto l’albero).
-- - poi mapBST visita ogni nodo dell’albero una sola volta per applicare lo shift.
--
-- Nota: grazie allo scoping del let, minVal è condiviso (calcolato una sola volta)
-- e riutilizzato per tutti i nodi nella funzione di mapping.
shiftTreeToZero :: (Num a, Ord a) => BST a -> BST a
shiftTreeToZero t =
    let minVal = findMinBST t
    in mapBST (\x -> x - minVal) t