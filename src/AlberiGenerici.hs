module AlberiGenerici
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
    ) where

{- 
5. ALBERI GENERICI

Si definiscano Alberi “generici” col seguente tipo di dato astratto (polimorfo)

data ( Eq a , Show a ) = > Tree a = Void | Node a [ Tree a ]
    deriving ( Eq , Show )

Con questo tipo di dato ci sono vari possibili modi per rappresentare una foglia: 
(Node x []), (Node x [Void]), (Node x [Void, Void]), ..., (Node x [Void, ..., Void]), ... .
Rinunciando all’albero vuoto si avrebbe una formulazione unica come

data ( Eq a , Show a ) = > NonEmptyTree a = Node a [ NonEmptyTree a ]
    deriving ( Eq , Show )

ma nel seguito abbiamo bisogno dell’albero vuoto e andremo a convivere con la rappresentazione non univoca.
-}

-- Definizione di albero generico:
-- - Void rappresenta l’albero vuoto
-- - Node contiene un valore e una lista di sottoalberi (figli)
data Tree a = Void | Node a [Tree a]
    deriving (Eq, Show)

{-
Esercizio 1

Si scriva una generalizzazione della funzione foldr delle liste per Alberi Generici che abbia il seguente tipo:

treefold :: ( Eq a , Show a ) = > (a - >[ b ] - > b ) -> b -> Tree a -> b
-}

-- treefold è una generalizzazione di foldr alle strutture ad albero “generiche”.
--
-- Parametri:
--   f : funzione che, dato il valore del nodo e i risultati già calcolati
--       sui figli (lista di b), produce il risultato per quel nodo (b).
--   z : valore base da usare quando l’albero è Void.
--   t : albero su cui fare il fold.
--
-- Comportamento:
-- - se t è Void, restituisce z
-- - se t è Node x cs, calcola ricorsivamente i risultati sui figli cs
--   e poi combina tutto con f.
treefold :: (a -> [b] -> b) -> b -> Tree a -> b
treefold f z t =
    case t of
        -- Caso base: albero vuoto
        Void ->
            z

        -- Caso ricorsivo:
        -- map (treefold f z) cs applica treefold a ogni figlio,
        -- ottenendo una lista di risultati [b] (uno per figlio).
        -- Poi f combina il valore del nodo x e i risultati dei figli.
        Node x cs ->
            f x (map (treefold f z) cs)

-- Variante che produce terne come risultato.
-- Non è una "fold diversa": è la stessa idea, specializzata al caso b = (b,c,d).
-- Serve quando si vuole calcolare più informazioni in un solo passaggio.
treefoldTuple :: (a -> [(b,c,d)] -> (b,c,d)) -> (b,c,d) -> Tree a -> (b,c,d)
treefoldTuple f z t =
    case t of
        -- Albero vuoto: restituisce la terna base z
        Void ->
            z

        -- Nodo: calcola prima le terne dei figli, poi le combina con f
        Node x cs ->
            f x (map (treefoldTuple f z) cs)

{-
Esercizio 2

Si scriva una funzione height per calcolare l’altezza di un albero usando opportunamente la treefold dell’Esercizio 1.
Si attribuisca altezza -1 all’albero vuoto.
Si colga l’occasione per verificare che treefold sia stata definita correttamente e quindi

height ( Node ’a ’ $ replicate n Void )

restituisca sempre 0 al variare di n.
-}

-- height calcola l’altezza di un albero generico usando treefold.
--
-- Convenzione:
--   altezza Void = -1
--   quindi un nodo i cui figli sono tutti Void ha altezza 0.
--
-- treefold f z:
--   - z è il risultato per Void
--   - f combina il valore del nodo e la lista dei risultati dei figli

height :: Tree a -> Int
height =
    treefold step (-1)
    where
        -- step riceve:
        --   _  = valore del nodo (non serve per l’altezza)
        --   hs = lista delle altezze dei figli
        --
        -- L’altezza del nodo è:
        --   1 + massimo tra le altezze dei figli.
        --
        -- L’espressione (-1 : hs) garantisce che:
        -- - se hs è vuota (Node x []),
        --   maximum (-1 : []) = -1,
        --   quindi altezza = 1 + (-1) = 0.
        --
        -- Questo assicura che anche
        --   Node 'a' (replicate n Void)
        -- abbia sempre altezza 0 per ogni n.
        step _ hs = 1 + maximum (-1 : hs)

{-
Esercizio 3

Si scriva una funzione simplify per eliminare i figli Void ridondanti usando opportunamente la treefold dell’Esercizio 1.
-}

-- simplify elimina i figli Void ridondanti.
--
-- Idea:
-- si usa treefold per ricostruire l’albero dal basso verso l’alto.
-- Per ogni nodo:
--   - si ricevono già i figli semplificati
--   - si rimuovono quelli uguali a Void
--
-- L’albero Void rimane Void.

simplify :: Eq a => Tree a -> Tree a
simplify =
    treefold step Void
    where
        -- step riceve:
        --   x  = valore del nodo
        --   ts = lista dei figli già semplificati
        --
        -- Si eliminano tutti i figli Void.
        step x ts =
            Node x [ t | t <- ts, t /= Void ]

{-
Esercizio 4

Si scrivano le generalizzazioni delle funzioni foldr e foldl delle liste per Alberi Generici aventi i seguenti tipi (abbiamo bisogno di due “zeri” corrispondenti all’albero vuoto e alla lista di alberi vuota):

treefoldr :: ( Eq a , Show a ) = > (a - >b - > c ) - >c - >(c - >b - > b ) - >b - > Tree a - > c
treefoldl :: ( Eq a , Show a ) = > (b - >a - > c ) - >c - >(c - >b - > b ) - >b - > Tree a - > c

Con queste fold non c’e bisogno di costruire la lista intermedia a cui applicare la funzione di “aggregazione” ma si esegue il lavoro man mano.
-}

-- treefoldr e treefoldl sono due generalizzazioni di foldr e foldl
-- per alberi generici.
--
-- L’idea è separare:
-- 1) il calcolo “sull’albero” (funzione f)
-- 2) l’aggregazione dei risultati dei figli (funzione g)
-- e prevedere due elementi neutri:
-- - zT per l’albero vuoto (Void)
-- - zL per la lista di figli vuota ([])

treefoldr
    :: (a -> b -> c)   -- f: valore nodo + aggregato figli -> risultato sul nodo
    -> c               -- zT: valore per Void
    -> (c -> b -> b)   -- g: inserisce il risultato di un figlio (c) nell’accumulatore (b), da destra
    -> b               -- zL: valore per lista figli vuota
    -> Tree a
    -> c
treefoldr f zT g zL t =
    case t of
        -- Caso base: albero vuoto.
        Void ->
            zT

        -- Caso nodo:
        -- 1) si aggregano i risultati dei figli (foldati ricorsivamente a c) in un valore b
        --    usando foldr e la funzione g
        -- 2) si combina il valore del nodo con l’aggregato dei figli usando f
        Node x cs ->
            let
                childrenAgg =
                    foldr
                        (g . treefoldr f zT g zL)
                        zL
                        cs
            in
                f x childrenAgg


treefoldl
    :: (b -> a -> c)   -- f: aggregato figli + valore nodo -> risultato sul nodo
    -> c               -- zT: valore per Void
    -> (b -> c -> b)   -- g: inserisce il risultato di un figlio (c) nell’accumulatore (b), da sinistra
    -> b               -- zL: valore per lista figli vuota
    -> Tree a
    -> c
treefoldl f zT g zL t =
    case t of
        -- Caso base: albero vuoto.
        Void ->
            zT

        -- Caso nodo:
        -- 1) si aggregano i risultati dei figli in un valore b usando foldl e g
        -- 2) si combina tale aggregato con il valore del nodo usando f
        Node x cs ->
            let
                childrenAgg =
                    foldl
                        (\acc child -> g acc (treefoldl f zT g zL child))
                        zL
                        cs
            in
                f childrenAgg x
{-
Esercizio 5

Si riscriva la funzione height per calcolare l’altezza di un albero usando opportunamente la treefoldr dell’Esercizio 4.
-}

-- heightR calcola l’altezza di un albero generico usando treefoldr.
--
-- Convenzione:
--   altezza Void = -1
--   quindi una foglia (Node x [] oppure Node x [Void,Void,...]) ha altezza 0.
--
-- Si usa treefoldr con:
-- - f: dato il valore del nodo (ignorato) e l’aggregato dei figli (mh),
--      restituisce 1 + mh.
-- - zT: -1 per Void.
-- - g: max per aggregare le altezze dei figli tenendo il massimo.
-- - zL: -1 come neutro per la lista dei figli vuota, così:
--      * se non ci sono figli (o sono tutti Void), mh resta -1
--      * quindi l’altezza del nodo diventa 0.

heightR :: Tree a -> Int
heightR =
    treefoldr
        (\_ mh -> 1 + mh)  -- f: altezza nodo = 1 + massimo altezza figli
        (-1)               -- zT: altezza di Void
        max                -- g: aggrega le altezze dei figli prendendo il massimo
        (-1)               -- zL: massimo su lista figli vuota

{-
Esercizio 6

Si riscriva la funzione simplify per eliminare i figli Void ridondanti usando opportunamente la treefoldr dell’Esercizio 4.
-}

-- simplifyR elimina i figli Void ridondanti usando treefoldr.
--
-- Strategia:
-- - f = Node: ricostruisce il nodo a partire dal valore e
--   dalla lista dei figli già filtrati.
-- - zT = Void: l’albero vuoto resta Void.
-- - g: durante l’aggregazione dei figli, scarta quelli uguali a Void.
-- - zL = []: lista vuota come neutro per la lista dei figli.

simplifyR :: Eq a => Tree a -> Tree a
simplifyR =
    treefoldr
        -- f: ricostruisce il nodo con i figli già semplificati
        Node

        -- zT: valore per l’albero vuoto
        Void

        -- g: inserisce un figlio solo se non è Void
        (\t acc -> if t == Void then acc else t : acc)

        -- zL: lista figli vuota
        []

{-
Esercizio 7

Si scriva una funzione degree che restituisce il grado di un albero (il massimo del numero di figli per ogni nodo).
-}

-- degree restituisce il grado di un albero generico,
-- cioè il massimo numero di figli tra tutti i nodi dell’albero.
--
-- Si usa treefold:
-- - per Void si restituisce 0
-- - per ogni nodo si ricevono in ds i gradi già calcolati
--   per ciascun figlio
-- - il grado del nodo corrente è:
--     max( numero di figli del nodo,
--          massimo grado tra i sottoalberi )

degree :: Tree a -> Int
degree =
    treefold step 0
    where
        step _ ds =
            -- length ds = numero di figli del nodo corrente
            -- maximum (0 : ds) = massimo grado nei sottoalberi
            max (length ds) (maximum (0 : ds))

{-
Esercizio 8

Si scriva una funzione transpose che restituisce il trasposto di un albero (per ogni nodo i trasposti dei figli in ordine inverso).
-}

-- transposeT restituisce il “trasposto” di un albero generico.
--
-- Per ogni nodo:
-- - si calcolano prima ricorsivamente i trasposti dei figli (grazie a treefold)
-- - poi si inverte l’ordine dei figli
--
-- L’albero Void resta Void.

transposeT :: Tree a -> Tree a
transposeT =
    treefold
        -- f: ricostruisce il nodo con i figli già trasposti,
        --     ma in ordine inverso.
        (\x ts -> Node x (reverse ts))

        -- z: valore per l’albero vuoto
        Void

{-
Esercizio 9

Si scriva un predicato issymm che stabilisce se un albero ha una forma simmetrica (cioè è uguale, non considerando il contenuto, al suo trasposto).
-}

-- shape elimina le etichette dell’albero,
-- mantenendo solo la struttura (forma).
--
-- Ogni valore viene sostituito con ().
shape :: Tree a -> Tree ()
shape =
    treefold
        (\_ ts -> Node () ts)
        Void


-- issymm verifica se un albero è simmetrico rispetto al trasposto,
-- considerando solo la forma e non i valori contenuti nei nodi.
--
-- Strategia:
-- 1) si confronta la forma dell’albero
-- 2) con la forma del suo trasposto
--
-- In questo modo eventuali differenze nei valori non influenzano il risultato.
issymm :: Tree a -> Bool
issymm t =
    shape t == shape (transposeT t)

{-
Esercizio 10

Si scriva una funzione normalize che dato un albero con valori nella classe Integral costruisca un nuovo albero che in ogni nodo contenga, al posto del valore originale, tale valore moltiplicato per l’inverso dell’altezza.
(Si presti attenzione nell’espressione della moltiplicazione in modo da avere tipi compatibili).
-}

-- normalize costruisce un nuovo albero in cui ogni valore x (integrale)
-- viene sostituito con x * (1 / heightNodo).
--
-- Per evitare divisioni tra interi (che troncherebbero), il risultato è Double:
-- - x viene convertito con fromIntegral
-- - l’altezza h è Int e viene convertita con fromIntegral
-- - l’inverso è quindi un Double
--
-- Convenzione sulle altezze:
--   altezza(Void) = -1
--   quindi una foglia ha altezza 0.
--
-- Nota: per una foglia h = 0 e quindi 1 / h non è definito.
-- Per evitare divisione per zero, si usa (h + 1) al denominatore:
-- - per una foglia: h = 0  => denominatore = 1  => valore invariato
-- - per un nodo di altezza 1: denominatore = 2, ecc.
-- Se invece si vuole esattamente 1/h, bisogna decidere come trattare le foglie.

normalize :: Integral a => Tree a -> Tree Double
normalize =
    snd . treefold step (-1, Void)
    where
        -- step riceve:
        --   x  = valore del nodo
        --   ch = lista dei risultati sui figli, ciascuno (altezzaFiglio, alberoNormalizzatoFiglio)
        --
        -- Restituisce (altezzaNodo, alberoNormalizzatoNodo).
        step x ch =
            let
                -- Altezza del nodo corrente: 1 + massimo delle altezze dei figli
                h = 1 + maximum (-1 : map fst ch)

                -- Normalizzazione: x * (1 / (h+1)) in Double
                invH = 1.0 / fromIntegral (h + 1)
                x'   = fromIntegral x * invH

                -- Ricostruzione del nodo con i figli già normalizzati
                children' = map snd ch
            in
                (h, Node x' children')

{-
Esercizio 11

Si scriva una funzione annotate che costruisca un nuovo albero che in ogni nodo contenga, al posto del valore originale, una coppia composta dal medesimo valore e dall’altezza del nodo stesso.
-}

-- annotate costruisce un nuovo albero in cui ogni nodo contiene:
--   (valoreOriginale, altezzaDelNodo)
--
-- Si usa treefold per calcolare in un solo passaggio:
-- - l’altezza del sottoalbero
-- - l’albero annotato corrispondente
--
-- Convenzione:
--   altezza(Void) = -1
--   quindi una foglia ha altezza 0.

annotate :: Tree a -> Tree (a, Int)
annotate =
    snd . treefold step (-1, Void)
    where
        -- step riceve:
        --   x  = valore del nodo corrente
        --   ch = lista dei risultati sui figli, ciascuno (altezzaFiglio, figlioAnnotato)
        --
        -- Calcola:
        --   h = 1 + massimo delle altezze dei figli (oppure 0 se non ci sono figli utili)
        -- e ricostruisce il nodo con valore (x, h) e figli già annotati.
        step x ch =
            let
                hs = map fst ch
                h  = 1 + maximum (-1 : hs)          -- gestisce anche lista vuota
                childrenAnn = map snd ch
            in
                (h, Node (x, h) childrenAnn)

{-
Esercizio 12

Si scriva un predicato iscorrect che determina se un albero è un albero di parsing secondo le regole di una grammatica codificata mediante una funzione che, dato un simbolo, restituisce la lista delle possibili espansioni (stringhe di simboli) secondo le produzioni.
-}

-- iscorrect verifica se un albero è un albero di parsing rispetto a una grammatica.
--
-- La grammatica è data come funzione:
--   grammar :: s -> [[s]]
-- che, dato un simbolo (non-terminale/terminale) x, restituisce tutte le possibili
-- espansioni di x, ciascuna come lista di simboli (la "stringa" del lato destro).
--
-- Un nodo Node x cs è corretto se:
-- 1) scartati i figli Void ridondanti, tutti i figli rimasti sono nodi (Node ...)
-- 2) la sequenza delle etichette dei figli (in ordine) è una delle espansioni di x
-- 3) ricorsivamente, ciascun figlio è a sua volta corretto

iscorrect :: Eq s => (s -> [[s]]) -> Tree s -> Bool
iscorrect _ Void = True
iscorrect grammar (Node x cs) =
    let
        -- Elimina eventuali Void ridondanti tra i figli
        cs' = [t | t <- cs, t /= Void]

        -- Verifica che tutti i figli non-void siano effettivamente Node
        allChildrenAreNodes =
            all (\t -> case t of Node _ _ -> True; _ -> False) cs'

        -- Estrae la sequenza di simboli dei figli (etichette dei Node)
        childSyms =
            [ y | Node y _ <- cs' ]

        -- Possibili espansioni del simbolo x secondo la grammatica
        expansions = grammar x
    in
        allChildrenAreNodes
        && childSyms `elem` expansions
        && all (iscorrect grammar) cs'

{-
Esercizio 13

Si scriva una funzione diameter che determina il diametro di un albero.
Il diametro di un albero è la lunghezza del massimo cammino fra due nodi, indipendentemente dall’orientamento degli archi.
-}

-- diameter calcola il diametro di un albero generico:
-- la lunghezza (numero di archi) del massimo cammino tra due nodi qualsiasi.
--
-- Implementazione lineare con treefold:
-- per ogni nodo calcoliamo contemporaneamente:
--   (diametroDelSottoalbero, altezzaDelSottoalbero)
--
-- Convenzioni:
--   altezza(Void) = -1  (foglia = 0)
--   diametro(Void) = 0
--
-- Per un nodo, il diametro è il massimo tra:
-- 1) il massimo diametro tra i figli
-- 2) un cammino che passa per il nodo, che usa le due altezze maggiori dei figli:
--      h1 + h2 + 2
--    (se c’è un solo figlio: h1 + 1; se nessun figlio: 0)
--
-- Per restare lineari, non si ordina la lista delle altezze:
-- si estraggono le due maggiori con una scansione.

diameter :: Tree a -> Int
diameter = fst . treefold step (0, -1)
    where
        step _ ch =
            let
                -- diametri e altezze già calcolati sui figli
                diams = map fst ch
                hs    = map snd ch

                -- miglior diametro “interno” a uno dei sottoalberi figli
                bestD = maximum (0 : diams)

                -- due massime altezze tra i figli (se esistono), in modo lineare
                (h1, h2) = top2 hs

                -- cammino che passa per il nodo corrente
                through
                    | h1 == -1  = 0          -- nessun figlio non-void
                    | h2 == -1  = h1 + 1     -- un solo figlio
                    | otherwise = h1 + h2 + 2

                -- altezza del nodo corrente
                hHere = 1 + maximum (-1 : hs)
            in
                (max bestD through, hHere)

        -- top2 restituisce le due massime altezze in una lista.
        -- Se la lista è vuota -> (-1, -1)
        -- Se ha un elemento h -> (h, -1)
        top2 :: [Int] -> (Int, Int)
        top2 = foldr upd (-1, -1)
            where
                upd h (m1, m2)
                    | h >= m1   = (h, m1)
                    | h >  m2   = (m1, h)
                    | otherwise = (m1, m2)

{-
Esercizio 14

Si scriva una funzione maxPathWeight che, dato un albero di valori numerici positivi, determina il massimo peso di tutti i cammini, indipendentemente dall’orientamento degli archi.
Il peso di un cammino, è la somma dei valori dei nodi del cammino.
-}

-- maxPathWeight calcola il massimo peso (somma dei valori dei nodi)
-- tra tutti i cammini dell’albero, indipendentemente dall’orientamento degli archi.
--
-- Assunzione dell’esercizio: valori numerici positivi.
--
-- Implementazione lineare con treefold:
-- per ogni nodo si calcolano due quantità:
--   best : massimo peso di un cammino completamente contenuto nel sottoalbero
--   down : massimo peso di un cammino che parte dal nodo corrente e scende in un solo figlio
--
-- Per un nodo con valore x:
-- - bestSub = massimo dei best dei figli (0 se non ci sono figli)
-- - downHere = x + massimo dei down dei figli (0 se non ci sono figli)
-- - throughHere = massimo cammino che passa per il nodo:
--     x + (miglior down del figlio 1) + (miglior down del figlio 2)
--   usando i due down maggiori tra i figli (se esistono)
--
-- Il risultato per il nodo è:
--   bestHere = max(bestSub, throughHere)


maxPathWeight :: (Num a, Ord a) => Tree a -> a
maxPathWeight = fst . treefold step (0, 0)
    where
        step x ch =
            let
                bests = map fst ch
                downs = map snd ch

                bestSub = maximum (0 : bests)

                -- due massimi down tra i figli (senza ordinare)
                (d1, d2) = top2 downs

                -- cammino che passa per il nodo corrente
                throughHere =
                    case (d1, d2) of
                        (0, 0) -> x
                        (_, 0) -> x + d1
                        _      -> x + d1 + d2

                -- miglior cammino in discesa dal nodo
                downHere = x + maximum (0 : downs)

                bestHere = max bestSub throughHere
            in
                (bestHere, downHere)

        -- top2 restituisce i due massimi valori di una lista.
        -- Usa 0 come neutro perché i valori (e quindi i down) sono non negativi.
        -- Se la lista è vuota -> (0, 0)
        -- Se ha un elemento d -> (d, 0)
        top2 :: (Num a, Ord a) => [a] -> (a, a)
        top2 = foldr upd (0, 0)
            where
                upd d (m1, m2)
                    | d >= m1   = (d, m1)
                    | d >  m2   = (m1, d)
                    | otherwise = (m1, m2)

{-
Esercizio 15

Si scriva una funzione preorder che restituisce la lista degli elementi di una visita in preordine.
-}

-- preorder restituisce la lista dei valori dell’albero
-- secondo una visita in preordine:
--   1) nodo corrente
--   2) poi ricorsivamente i figli da sinistra a destra
--
-- Si usa treefold:
-- - per Void si restituisce []
-- - per Node x cs, si riceve in xs la lista dei risultati
--   delle visite in preordine dei figli,
--   quindi si costruisce: x : (concatenazione dei risultati dei figli)

preorder :: Tree a -> [a]
preorder =
    treefold
        (\x xs -> x : concat xs)
        []

{-
Esercizio 16

Si scriva una funzione frontier che restituisce la frontiera di un albero (la lista degli elementi delle foglie).
-}

-- frontier restituisce la lista delle etichette delle foglie dell’albero.
--
-- Una foglia è un nodo che, dopo aver eliminato eventuali Void ridondanti,
-- non ha figli “reali”.
--
-- Usando treefold:
-- - per Void si restituisce []
-- - per Node x cs, si ricevono in childs le frontiere dei figli
--   (una lista di liste)
--   * se almeno una è non vuota, il nodo non è foglia -> si concatena
--   * se sono tutte vuote, il nodo è foglia -> si restituisce [x]

frontier :: Tree a -> [a]
frontier =
    treefold step []
    where
        step x childs
            | (not . all null) childs = concat childs
            | otherwise               = [x]

{-
Esercizio 17

Si scriva una funzione smallParents che restituisce la lista dei (valori dei) nodi che son genitori ma non nonni di qualche altro nodo.
La lista deve essere prodotta rispettando l’ordine di comparizione nell’albero.
-}

-- smallParents restituisce la lista dei valori dei nodi che:
-- - hanno almeno un figlio “reale” (cioè diverso da Void)
-- - e tutti i loro figli “reali” sono foglie
--
-- In altre parole: sono genitori ma non nonni.
-- L’ordine richiesto è quello di comparizione nell’albero (preordine),
-- quindi il valore del nodo (se valido) deve comparire prima dei risultati dei figli.
--
-- Si usa treefoldTuple per calcolare in un solo passaggio, per ogni sottoalbero:
--   (listaSmallParentsInPreordine, isLeaf, hasNode)
-- dove:
-- - isLeaf: True se il nodo non ha figli non-void (ignorando i Void ridondanti)
-- - hasNode: False solo per Void (serve per filtrare i “figli” vuoti)

smallParents :: Tree a -> [a]
smallParents =
    (\(lst, _, _) -> lst) . treefoldTuple step ([], True, False)
    where
        step x ch =
            let
                -- Liste già prodotte dai figli (in preordine rispetto ai loro sottoalberi)
                childLists = [ls | (ls, _, _) <- ch]

                -- Consideriamo solo i figli non-void (hasNode = True)
                childIsLeaf = [il | (_, il, hn) <- ch, hn]

                -- Il nodo ha figli “reali” se esiste almeno un figlio non-void
                hasKids = not (null childIsLeaf)

                -- Tutti i figli “reali” sono foglie?
                allKidsAreLeaves = and childIsLeaf

                -- Il nodo corrente è foglia se non ha figli reali
                hereIsLeaf = not hasKids

                -- È “small parent” se ha figli reali e sono tutti foglie
                hereSmallParent = hasKids && allKidsAreLeaves

                -- Per rispettare l’ordine di comparizione (preordine):
                -- prima eventualmente x, poi i risultati dei figli nell’ordine originale.
                hereList = ([x | hereSmallParent]) ++ concat childLists
            in
                (hereList, hereIsLeaf, True)

{-
Esercizio 18

Si scriva un predicato arithmSmallParents che determina se i (valori dei) nodi che son genitori ma non nonni di qualche altro nodo sono una progressione aritmetica (∃y, z : ∀i.
xi = y + i ∗ z).
-}

-- arithmSmallParents verifica se i valori restituiti da smallParents
-- formano una progressione aritmetica.
--
-- Una lista [x0, x1, ..., xn] è una progressione aritmetica se:
--   esiste d tale che per ogni i vale
--     xi = x0 + i * d
--
-- Casi base:
-- - lista vuota  -> True
-- - un solo elemento -> True
--
-- Per almeno due elementi:
-- - si calcola la differenza d = x1 - x0
-- - si verifica che ogni elemento soddisfi la relazione

arithmSmallParents :: (Eq a, Num a) => Tree a -> Bool
arithmSmallParents t =
    case smallParents t of
        []        -> True
        [_]       -> True
        xs@(x0:x1:_) ->
            let
                d = x1 - x0
                -- costruisce la progressione attesa
                expected =
                    zipWith (\i _ -> x0 + fromIntegral i * d) [0..] xs
            in
                expected == xs

{-
Esercizio 19

Codificando un’espressione e in notazione polacca inversa mediante una lista di tipo Num a =>[Either a (op,Int)], si scriva una funzione rpn2tree che data e costruisca un opportuno albero di sintassi astratta.

La componente intera della coppia che identifica un operazione ne stabilisce l’arità.

Ad esempio per  x y z s/2 m/2 otteniamo Node m [Node s [Node z [], Node y []], Node x []].

Se servisse si assuma che le e in input siano ben formate (corrispondano ad una vera espressione).
-}

-- rpn2tree costruisce un albero di sintassi astratta a partire da una
-- espressione in notazione polacca inversa (RPN).
--
-- Rappresentazione dei token:
-- - Left v            : operando (qui rappresentato come String)
-- - Right (op, arity) : operatore op con arità arity
--
-- Strategia (classica con stack):
-- - si scorre la lista dei token da sinistra a destra mantenendo uno stack di alberi
-- - quando si legge un operando, si push-a la foglia Node v []
-- - quando si legge un operatore (op,k):
--     * si prelevano dallo stack i k alberi che rappresentano gli argomenti
--     * si costruisce Node op [arg1, ..., argk]
--     * si push-a il nuovo albero sullo stack
--
-- Attenzione all’ordine degli argomenti:
-- lo stack contiene in testa l’elemento più recente.
-- In RPN, l’argomento “più a destra” viene prodotto per ultimo e quindi è in cima.
-- Per ottenere l’ordine naturale [argSinistro, ..., argDestro],
-- dopo splitAt k si fa reverse args.

rpn2tree :: [Either String (String, Int)] -> Tree String
rpn2tree toks =
    case foldl step [] toks of
        [t] -> t
        _   -> error "RPN malformata"
    where
        -- step aggiorna lo stack in base al token corrente.
        step :: [Tree String] -> Either String (String, Int) -> [Tree String]
        step st (Left v) =
            -- Operando: si inserisce una foglia in cima allo stack.
            Node v [] : st

        step st (Right (op, k)) =
        -- Operatore: si estraggono k argomenti dallo stack.
            let (args, rest) = splitAt k st
            in
                if length args < k
                then error "RPN malformata: stack insufficiente"
                else
                    -- reverse per ripristinare l’ordine corretto degli argomenti.
                    Node op (reverse args) : rest