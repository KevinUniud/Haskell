module QuadTrees
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
    ) where

{-
6. QUAD TREES

Molte tecniche sviluppate per la compressione di immagini si basano su una codifica ad albero chiamata “Quad Tree”.
Si codificano in questo modo immagini quadrate il cui lato sia una potenza di 2.
Se l’immagine è omogenea (stesso colore) si codifica, indipendentemente dalle sue dimensioni, con una foglia contenente il colore.
Se l’immagine è eterogenea allora si utilizza un nodo i cui figli contengono le codifiche dei quadranti superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro, rispettivamente.
Si definiscano i QuadTrees col seguente tipo di dato astratto (polimorfo)

data ( Eq a , Show a ) = > QT a = C a | Q ( QT a ) ( QT a ) ( QT a ) ( QT a )
    deriving ( Eq , Show )

Con questa struttura si possono costruire termini che non corrispondono propriamente ad un QuadTree.
Ad esempio

let u = C 1 in Q u u u u

non è la codifica di un’immagine, visto che dovrebbe essere semplicemente C 1.
Chiamerò “termini di tipo QT” questi casi patologici, mentre QuadTrees quelli che corrispondono correttamente alla codifica di un’immagine.
Possiamo subito notare dall’esempio di prima che partendo da 4 QuadTrees non si garantisce di costruire con il costruttore Q un QuadTree, ma solo un termine di tipo QT.
-}

-- Tipo di dato per rappresentare un QuadTree:
-- - C a    : foglia (immagine omogenea) che contiene un singolo colore/valore `a`
-- - Q ...  : nodo interno con 4 figli, uno per ciascun quadrante:
--           (superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro)
data QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
    deriving (Eq, Show)

{-
Esercizio 1

Si scriva una funzione buildNSimplify che dati 4 QuadTree costruisca un QuadTree la cui im- magine codificata sia quella ottenuta dalle 4 immagini corrispondenti ai 4 QuadTree messe nei quadranti superiore-sinistro, superiore-destro, inferiore-sinistro, inferiore-destro, rispettivamente.
(Attenzione che tutti sono e devono essere QuadTrees, non solo termini di tipo QT)
-}

-- buildNSimplify costruisce il QuadTree che rappresenta l’immagine ottenuta
-- mettendo i 4 QuadTree nei rispettivi quadranti.
--
-- Inoltre “semplifica” il risultato: se i 4 quadranti risultano tutti foglie
-- con lo stesso valore, allora l’intera immagine è omogenea e può essere
-- rappresentata con una singola foglia (C x) invece di un nodo Q.
--
-- Vincolo Eq a: serve per confrontare i valori (colori) nelle foglie.
buildNSimplify :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify a b c d =
    case (a,b,c,d) of
        -- Caso di semplificazione:
        -- se tutti e 4 i quadranti sono foglie (C ...) e contengono lo stesso valore,
        -- allora il QuadTree composto si riduce a una sola foglia.
        (C x, C y, C z, C w) | x==y && y==z && z==w -> C x

        -- Altrimenti non si può comprimere ulteriormente:
        -- si costruisce il nodo Q con i 4 sottoalberi così come sono.
        _ -> Q a b c d

{-
Esercizio 2

Si scriva una funzione simplify che dato un termine di tipo QT genera il QuadTree corrispondente.
-}

-- simplify prende un termine di tipo QT (che potrebbe non essere
-- un vero QuadTree, cioè potrebbe contenere nodi Q comprimibili)
-- e restituisce il QuadTree corretto, applicando la semplificazione
-- in modo ricorsivo su tutta la struttura.

simplify :: Eq a => QT a -> QT a

-- Caso base:
-- Se il termine è una foglia (C x), è già un QuadTree valido,
-- quindi viene restituito invariato.
simplify (C x) = C x

-- Caso ricorsivo:
-- Se il termine è un nodo Q con 4 sottoalberi, si procede così:
-- 1) Si semplificano ricorsivamente i 4 sottoalberi.
-- 2) Si usa buildNSimplify per costruire il nodo risultante.
--    Questa funzione si occupa anche di comprimere il nodo
--    nel caso in cui i 4 figli diventino foglie con lo stesso valore.
simplify (Q a b c d) =
    buildNSimplify (simplify a)
                    (simplify b)
                    (simplify c)
                    (simplify d)

{-
Esercizio 3

Si scriva una funzione map che data una funzione f e un QuadTree q determina il QuadTree che codifica l’immagine risultante dall’applicazione di f a tutti i pixel dell’immagine codificata da q.
-}

-- qtmap applica una funzione f a tutti i “pixel” dell’immagine codificata
-- dal QuadTree.
--
-- Poiché un QuadTree compatta regioni omogenee in foglie (C x), applicare f
-- a tutti i pixel equivale ad applicare f:
-- - al valore contenuto in ogni foglia
-- - e ricorsivamente a tutte le foglie dei sottoalberi
--
-- Il vincolo Eq b serve perché, dopo la trasformazione dei valori in b,
-- potrebbe diventare possibile comprimere un nodo: ad esempio, quattro quadranti
-- con valori diversi in a potrebbero diventare uguali in b dopo f.
-- Per questo si usa buildNSimplify anche nel caso Q.
qtmap :: Eq b => (a -> b) -> QT a -> QT b

-- Caso foglia:
-- La regione è omogenea con valore x; dopo la trasformazione diventa omogenea
-- con valore f x.
qtmap f (C x) = C (f x)

-- Caso nodo:
-- Si trasformano ricorsivamente i 4 quadranti.
-- Poi buildNSimplify ricostruisce il nodo e, se i 4 risultati sono foglie
-- uguali, comprime tutto in una singola foglia.
qtmap f (Q a b c d) =
    buildNSimplify (qtmap f a)
                    (qtmap f b)
                    (qtmap f c)
                    (qtmap f d)

{-
Esercizio 4

Si scriva una funzione howManyPixels che dato un QuadTree determina il numero (minimo) di pixel di quell’immagine.
Ad esempio

let z = C 0; u = C 1; q = Q z u u u in howManyPixels ( Q q ( C 0) ( C 2) q )

restituisce 16.
-}

-- qtHeight calcola l’altezza del QuadTree.
--
-- Convenzione adottata:
-- - Una foglia (C _) ha altezza 0.
-- - Un nodo Q ha altezza pari a
--       1 + massimo tra le altezze dei suoi 4 figli.
--
-- L’altezza rappresenta il numero massimo di suddivisioni
-- lungo un cammino dalla radice fino a una foglia.
-- In altre parole, misura la profondità massima dell’albero.
qtHeight :: QT a -> Int
qtHeight (C _) = 0
qtHeight (Q a b c d) =
    1 + maximum [qtHeight a, qtHeight b, qtHeight c, qtHeight d]


-- howManyPixels restituisce il numero minimo di pixel
-- dell’immagine rappresentata dal QuadTree.
--
-- Idea:
-- Se l’altezza dell’albero è h, significa che il quadrato
-- iniziale è stato suddiviso h volte.
--
-- Ogni suddivisione divide un quadrato in 4 sottoquadrati,
-- quindi il numero totale di pixel è:
--
--      4^h
--
-- Questo è il numero minimo coerente con la struttura:
-- l’immagine è un quadrato di lato 2^h,
-- quindi il numero totale di pixel è (2^h)^2 = 4^h.
howManyPixels :: QT a -> Int
howManyPixels q =
        let h = qtHeight q
        in pow 4 h
    where
        -- pow calcola la potenza x^n in modo ricorsivo.
        pow :: Int -> Int -> Int
        pow _ 0 = 1
        pow x n = x * pow x (n-1)

{-
Esercizio 5

Si scriva una funzione limitAll che dato un colore c e una lista di QuadTrees costruisca la lista dei QuadTrees che codificano le immagini i cui pixels sono limitati al colore c (pixel originale se il colore è < c, c altrimenti).
-}

-- limitAll prende:
-- - un colore c
-- - una lista di QuadTree
-- e restituisce la lista dei QuadTree in cui ogni pixel è "limitato" superiormente a c.
--
-- Regola:
-- - se il pixel originale x < c, rimane x
-- - altrimenti diventa c
--
-- Questo comportamento è ottenuto con (`min` c), cioè:
--     min x c
-- che restituisce x se x < c, altrimenti c.
--
-- Si usa qtmap per applicare la trasformazione a tutti i pixel
-- di ciascun QuadTree, mantenendo la proprietà di semplificazione.
--
-- Ord a serve per confrontare i valori con c.
-- Eq a serve perché qtmap può dover comprimere nodi con buildNSimplify.
limitAll :: (Ord a, Eq a) => a -> [QT a] -> [QT a]
limitAll c = map (qtmap (`min` c))

{-
Esercizio 6

Si scriva una funzione occurrencies che dato un QuadTree ed un colore determina il numero (minimo) di pixel di quel colore.
Ad esempio

let z = C 0; u = C 1; q = Q z u u u in occurrencies ( Q q ( C 0) ( C 2) q ) 0

restituisce 6 (visto che il QuadTree codifica almeno 16 pixel).
-}

-- occurrencies calcola il numero minimo di pixel di colore `col`
-- nell’immagine rappresentata dal QuadTree `q`.
--
-- L’idea è che una foglia C x rappresenta una regione omogenea.
-- Tuttavia, se l’albero non è perfettamente bilanciato, foglie a profondità
-- diverse rappresentano blocchi di dimensione diversa.
-- Per questo motivo bisogna “espandere” i conteggi dei sottoalberi
-- meno profondi quando li si combina con altri più profondi.

occurrencies :: Eq a => QT a -> a -> Int
occurrencies q col = snd (go q col)
    where
        -- go restituisce una coppia:
        -- (h, o)
        --   h = altezza del sottoalbero
        --   o = numero di occorrenze del colore a quel livello
        --
        -- Il parametro col' è il colore da contare, passato esplicitamente
        -- per mantenere lo stesso tipo senza estensioni del linguaggio.
        go :: Eq a => QT a -> a -> (Int, Int)

        -- Caso foglia:
        -- altezza = 0
        -- se il valore coincide con col', conta 1 blocco, altrimenti 0
        go (C x) col' =
            (0, if x == col' then 1 else 0)

        -- Caso nodo:
        -- si calcola ricorsivamente (altezza, occorrenze) per ogni quadrante
        go (Q a b c d) col' =
            let
                (ha, oa) = go a col'
                (hb, ob) = go b col'
                (hc, oc) = go c col'
                (hd, od) = go d col'

                -- Altezza del nodo corrente:
                -- 1 + massimo delle altezze dei figli
                h = 1 + maximum [ha, hb, hc, hd]

                -- I figli devono essere portati alla stessa “risoluzione”.
                -- Se un figlio ha altezza minore di (h - 1),
                -- significa che rappresenta un blocco più grande,
                -- quindi il suo conteggio va moltiplicato per 4
                -- per ogni livello mancante.
                oa' = oa * pow4 (h - 1 - ha)
                ob' = ob * pow4 (h - 1 - hb)
                oc' = oc * pow4 (h - 1 - hc)
                od' = od * pow4 (h - 1 - hd)
            in
                -- Somma totale delle occorrenze, già scalate
                (h, oa' + ob' + oc' + od')

        -- pow4 n calcola 4^n
        -- serve per espandere i conteggi quando si uniformano le altezze
        pow4 :: Int -> Int
        pow4 0 = 1
        pow4 n = 4 * pow4 (n - 1)

{-
Esercizio 7

Si scriva una funzione Haskell difference che dato un colore c ed un QuadTree q determina la differenza fra il numero di pixel dell’immagine codificata da q che hanno un colore maggiore di c e quelli minori di c.

Ad esempio

let d = C 2; u = C 1; q = Q d u u u
in difference 1 ( Q q ( C 0) ( C 3) q )

restituisce -4 (visto che il QuadTree codifica almeno 16 pixel).
-}

-- difference calcola:
--   (# pixel con colore > c0) - (# pixel con colore < c0)
-- nell’immagine rappresentata dal QuadTree `q`.
--
-- Come in occurrencies, se l’albero non è bilanciato bisogna scalare i conteggi
-- dei sottoalberi meno profondi: ogni livello mancante moltiplica per 4.
--
-- Nota sul confronto:
-- - i pixel con valore > c0 contribuiscono +1
-- - i pixel con valore < c0 contribuiscono -1
-- - i pixel con valore == c0 contribuiscono 0 (non influiscono sulla differenza)

difference :: Ord a => a -> QT a -> Int
difference c0 q = snd (go q c0)
    where
        -- pow4 n = 4^n, usata per espandere i contributi quando un sottoalbero
        -- è più basso rispetto al livello di riferimento.
        pow4 :: Int -> Int
        pow4 0 = 1
        pow4 n = 4 * pow4 (n - 1)

        -- go ritorna (h, d)
        --   h = altezza del sottoalbero
        --   d = differenza a quel livello (in “blocchi” del livello del sottoalbero)
        --
        -- Passo `c` esplicitamente per evitare che GHC generalizzi `go`
        -- su un tipo diverso (problema visto nell’errore).
        go :: Ord a => QT a -> a -> (Int, Int)

        -- Caso foglia: un blocco omogeneo
        go (C x) c =
            (0, if x > c then 1 else if x < c then -1 else 0)

        -- Caso nodo: combina i 4 quadranti portandoli alla stessa risoluzione
        go (Q a b c d) col =
            let
                (ha, da) = go a col
                (hb, db) = go b col
                (hc, dc) = go c col
                (hd, dd) = go d col

                -- Altezza del nodo corrente
                h = 1 + maximum [ha, hb, hc, hd]

                -- Scala i contributi dei figli al livello (h - 1)
                da' = da * pow4 (h - 1 - ha)
                db' = db * pow4 (h - 1 - hb)
                dc' = dc * pow4 (h - 1 - hc)
                dd' = dd * pow4 (h - 1 - hd)
            in
                (h, da' + db' + dc' + dd')

{-
Esercizio 8

Si scriva una funzione Haskell overColor che dato un colore c ed un QuadTree q determina il numero (minimo) di pixel dell’immagine codificata da q che hanno un colore maggiore di c.

Ad esempio

let d = C 2; u = C 1; q = Q d u u u
in overColor 1 ( Q q ( C 0) ( C 3) q )

restituisce 6 (visto che il QuadTree codifica almeno 16 pixel).
-}

-- overColor calcola il numero minimo di pixel con colore > c0
-- nell’immagine rappresentata dal QuadTree `q`.
--
-- Come per occurrencies/difference, se l’albero non è bilanciato,
-- foglie a altezze diverse rappresentano blocchi di dimensione diversa.
-- Per sommare correttamente i contributi si scala ogni conteggio
-- alla stessa risoluzione: ogni livello mancante moltiplica per 4.

overColor :: Ord a => a -> QT a -> Int
overColor c0 q = snd (go q c0)
    where
        -- go ritorna (h, o)
        --   h = altezza del sottoalbero
        --   o = numero di occorrenze di valori > col, misurate al livello del sottoalbero
        --
        -- Passo col esplicitamente per evitare la generalizzazione di `go`
        -- su un tipo diverso da quello di `c0`.
        go :: Ord a => QT a -> a -> (Int, Int)

        -- Caso foglia:
        -- altezza = 0
        -- se il valore è > col allora conta 1 blocco, altrimenti 0
        go (C x) col =
            (0, if x > col then 1 else 0)

        -- Caso nodo:
        -- calcolo ricorsivo su ciascun quadrante, poi scala e somma
        go (Q a b c d) col =
            let
                (ha, oa) = go a col
                (hb, ob) = go b col
                (hc, oc) = go c col
                (hd, od) = go d col

                -- Altezza del nodo corrente
                h = 1 + maximum [ha, hb, hc, hd]

                -- Porta ogni conteggio al livello (h - 1) moltiplicando per 4
                -- per ogni livello mancante rispetto al figlio più profondo.
                oa' = oa * pow4 (h - 1 - ha)
                ob' = ob * pow4 (h - 1 - hb)
                oc' = oc * pow4 (h - 1 - hc)
                od' = od * pow4 (h - 1 - hd)
            in
                (h, oa' + ob' + oc' + od')

        -- pow4 n = 4^n, usata per scalare i conteggi
        pow4 :: Int -> Int
        pow4 0 = 1
        pow4 n = 4 * pow4 (n - 1)

{-
Esercizio 9

Si scriva una generalizzazione della funzione foldr delle liste per i termini di tipo QT che abbia il seguente tipo:

fold :: ( Eq a , Show a ) = > (b - >b - >b - >b - > b ) -> (a - > b ) -> QT a -> b
-}

-- fold è l’equivalente di foldr per i termini di tipo QT.
-- Serve a “consumare” un QT producendo un valore di tipo b, specificando:
--
-- - cosa fare su una foglia (C x) tramite `cf :: a -> b`
-- - come combinare i risultati dei quattro quadranti di un nodo (Q ...)
--   tramite `qf :: b -> b -> b -> b -> b`
--
-- In pratica:
-- - se il termine è una foglia, si applica cf al valore contenuto
-- - se il termine è un nodo, si fa fold ricorsivamente sui 4 figli e
--   si combinano i 4 risultati con qf (nell’ordine: sup-sx, sup-dx, inf-sx, inf-dx)

fold :: (b -> b -> b -> b -> b) -> (a -> b) -> QT a -> b
fold qf cf t =
    case t of
        -- Caso foglia: restituisce il risultato deciso da cf
        C x ->
            cf x

        -- Caso nodo: calcola ricorsivamente i 4 risultati e li combina con qf
        Q a b c d ->
            qf (fold qf cf a)
                (fold qf cf b)
                (fold qf cf c)
                (fold qf cf d)

{-
Esercizio 10

Si scriva una funzione height che dato un QuadTree ne determina l’altezza usando opportunamente fold.
-}

-- height calcola l’altezza di un QuadTree usando fold.
--
-- Idea:
-- - una foglia (C _) ha altezza 0
-- - un nodo (Q ...) ha altezza 1 + massimo delle altezze dei 4 figli
--
-- Usiamo fold specificando:
-- - cf = const 0
--     per una foglia, ignoriamo il valore contenuto e restituiamo 0
-- - qf = \a b c d -> 1 + maximum [a,b,c,d]
--     combina le altezze dei 4 quadranti aggiungendo 1 per il nodo corrente

height :: QT a -> Int
height =
    fold
        (\a b c d -> 1 + maximum [a, b, c, d])
        (const 0)

{-
Esercizio 11

Si scriva una funzione length che dato un QuadTree ne determina il numero di nodi usando opportunamente fold.
-}

-- qtLength calcola il numero totale di nodi del QuadTree
-- (sia foglie C che nodi interni Q) usando fold.
--
-- Idea:
-- - ogni foglia conta come 1 nodo
-- - ogni nodo Q conta come 1 (sé stesso) più il numero di nodi
--   contenuti nei suoi 4 sottoalberi
--
-- Specifica di fold:
-- - cf = const 1
--     quando incontriamo una foglia, restituiamo 1
-- - qf = \a b c d -> 1 + a + b + c + d
--     somma i nodi dei 4 figli e aggiunge 1 per il nodo corrente

qtLength :: QT a -> Int
qtLength =
    fold
        (\a b c d -> 1 + a + b + c + d)
        (const 1)

{-
Esercizio 12

Si riscriva la funzione simplify dell’Esercizio 2 usando opportunamente fold.
-}

-- simplifyF riscrive simplify usando fold.
--
-- Idea:
-- - nel caso foglia, il risultato è semplicemente C x
-- - nel caso nodo, abbiamo già i 4 sottoalberi semplificati
--   (perché fold lavora bottom-up), quindi possiamo
--   combinarli con buildNSimplify, che:
--     * costruisce un Q se necessario
--     * comprime in una foglia se i 4 figli sono foglie uguali
--
-- buildNSimplify ha tipo:
--     QT a -> QT a -> QT a -> QT a -> QT a
-- quindi coincide esattamente con la funzione di combinazione richiesta da fold.
--
-- C ha tipo:
--     a -> QT a
-- quindi coincide con la funzione da applicare nel caso foglia.

simplifyF :: Eq a => QT a -> QT a
simplifyF =
    fold
        buildNSimplify  -- combinazione dei 4 quadranti
        C               -- costruzione della foglia

{-
Esercizio 13

Si riscriva la funzione map dell’Esercizio 3 usando opportunamente fold.
-}

-- qtmapF riscrive qtmap usando fold.
--
-- Obiettivo:
-- applicare una funzione f a tutti i valori contenuti nelle foglie
-- del QuadTree, ricostruendo la struttura e comprimendo dove possibile.
--
-- Scelte per fold:
--
-- - Caso foglia:
--     (C . f)
--   significa: dato x, prima applica f ottenendo (f x),
--   poi costruisce la foglia C (f x).
--
-- - Caso nodo:
--     buildNSimplify
--   riceve i 4 sottoalberi già trasformati (perché fold è bottom-up)
--   e:
--     * costruisce un nodo Q
--     * oppure lo comprime in una foglia se i 4 figli sono uguali
--
-- Eq b è necessario perché buildNSimplify confronta i valori
-- per decidere se comprimere.

qtmapF :: Eq b => (a -> b) -> QT a -> QT b
qtmapF f =
    fold
        buildNSimplify   -- combinazione dei 4 quadranti trasformati
        (C . f)          -- trasformazione della foglia

{-
Esercizio 14

Si scrivano due funzioni flipHorizontal/flipVertical che costruiscono il QuadTree dell’imma- gine simmetrica rispetto all’asse orizzontale/verticale.
-}

-- flipHorizontal costruisce il QuadTree dell’immagine
-- simmetrica rispetto all’asse orizzontale.
--
-- Simmetria orizzontale:
-- - il quadrante superiore-sinistro (ul) diventa inferiore-sinistro
-- - il quadrante superiore-destro  (ur) diventa inferiore-destro
-- - il quadrante inferiore-sinistro (ll) diventa superiore-sinistro
-- - il quadrante inferiore-destro  (lr) diventa superiore-destro
--
-- Poiché fold lavora bottom-up, i 4 argomenti della funzione
-- di combinazione sono già i sottoalberi trasformati.
-- buildNSimplify ricostruisce il nodo e comprime se possibile.

flipHorizontal :: Eq a => QT a -> QT a
flipHorizontal =
    fold
        (\ul ur ll lr -> buildNSimplify ll lr ul ur)
        C


-- flipVertical costruisce il QuadTree dell’immagine
-- simmetrica rispetto all’asse verticale.
--
-- Simmetria verticale:
-- - ul scambia con ur
-- - ll scambia con lr
--
-- Anche qui, fold fornisce i sottoalberi già trasformati,
-- e buildNSimplify ricostruisce/comprime il nodo risultante.

flipVertical :: Eq a => QT a -> QT a
flipVertical =
    fold
        (\ul ur ll lr -> buildNSimplify ur ul lr ll)
        C

{-
Esercizio 15

Si scrivano tre funzioni rotate90Right, rotate90Left e rotate180 che costruiscono il QuadTree dell’immagine ruotata di −π/2, +π/2 e π.
-}

-- Convenzione dei quadranti:
-- Q ul ur ll lr
--   ul = upper-left     (superiore-sinistro)
--   ur = upper-right    (superiore-destro)
--   ll = lower-left     (inferiore-sinistro)
--   lr = lower-right    (inferiore-destro)
--
-- In tutte le funzioni usiamo fold:
-- - C nel caso foglia (una rotazione di una foglia è la foglia stessa)
-- - una funzione che riordina opportunamente i 4 quadranti
--   e poi ricostruisce il nodo con buildNSimplify


-- Rotazione di -π/2 (90° verso destra, senso orario).
--
-- Mappatura dei quadranti:
--   ul -> ur
--   ur -> lr
--   lr -> ll
--   ll -> ul
--
-- Nuova disposizione:
--   ul' = ll
--   ur' = ul
--   ll' = lr
--   lr' = ur

rotate90Right :: Eq a => QT a -> QT a
rotate90Right =
    fold
        (\ul ur ll lr -> buildNSimplify ll ul lr ur)
        C


-- Rotazione di +π/2 (90° verso sinistra, senso antiorario).
--
-- Mappatura dei quadranti:
--   ul -> ll
--   ll -> lr
--   lr -> ur
--   ur -> ul
--
-- Nuova disposizione:
--   ul' = ur
--   ur' = lr
--   ll' = ul
--   lr' = ll

rotate90Left :: Eq a => QT a -> QT a
rotate90Left =
    fold
        (\ul ur ll lr -> buildNSimplify ur lr ul ll)
        C


-- Rotazione di π (180°).
--
-- Mappatura dei quadranti:
--   ul <-> lr
--   ur <-> ll
--
-- Nuova disposizione:
--   ul' = lr
--   ur' = ll
--   ll' = ur
--   lr' = ul

rotate180 :: Eq a => QT a -> QT a
rotate180 =
    fold
        (\ul ur ll lr -> buildNSimplify lr ll ur ul)
        C

{-
Esercizio 16

Si scrivano tre predicati isHorizontalSymmetric, isVerticalSymmetric e isCenterSymmetric che determinano se un QuadTree codifica un’immagine simmetrica rispetto all’asse orizzontale, all’asse verticale o al centro.
-}

-- I tre predicati verificano la simmetria confrontando l’immagine originale
-- con la sua trasformazione (flip/rotazione).
--
-- Per evitare falsi negativi dovuti a rappresentazioni diverse ma equivalenti
-- (ad esempio un nodo Q comprimibile contro una foglia C), si normalizza
-- prima con simplify:
-- - simplify q produce un QuadTree “canonico” (senza nodi Q comprimibili)
-- - quindi l’uguaglianza (==) confronta correttamente le immagini codificate

isHorizontalSymmetric :: Eq a => QT a -> Bool
isHorizontalSymmetric q =
    simplify q == simplify (flipHorizontal q)
    -- True se l’immagine coincide con la sua simmetrica rispetto all’asse orizzontale

isVerticalSymmetric :: Eq a => QT a -> Bool
isVerticalSymmetric q =
    simplify q == simplify (flipVertical q)
    -- True se l’immagine coincide con la sua simmetrica rispetto all’asse verticale

isCenterSymmetric :: Eq a => QT a -> Bool
isCenterSymmetric q =
    simplify q == simplify (rotate180 q)
    -- True se l’immagine coincide con la rotazione di 180° (simmetria centrale)


{-
Esercizio 17

Si scriva un predicato elemOrMele che dati un QuadTree t e una lista di QuadTrees ts determina se t, o il QuadTree che codifica l’immagine di t ribaltata rispetto all’asse orizzontale, sono elementi della lista ts.
-}

-- elemOrMele verifica se:
--   - il QuadTree t
--   - oppure la sua versione ribaltata orizzontalmente
-- appartengono alla lista ts.
--
-- Per confrontare correttamente le immagini (ed evitare differenze
-- solo strutturali, come nodi Q comprimibili), si normalizzano
-- tutti i QuadTree con simplify prima del confronto.

elemOrMele :: Eq a => QT a -> [QT a] -> Bool
elemOrMele t ts =
    let
        -- versione normalizzata di t
        t'  = simplify t

        -- versione normalizzata di t ribaltata orizzontalmente
        th  = simplify (flipHorizontal t)

        -- lista normalizzata
        ts' = map simplify ts
    in
        -- True se t' è nella lista
        -- oppure se la sua versione ribaltata è nella lista
        t' `elem` ts' || th `elem` ts'

{-
Esercizio 18

Si scriva un predicato isRotatedIn che dati un QuadTree t e una lista di QuadTrees ts determina se uno dei QuadTrees che codificano l’immagine di t ruotata di 0, 90, 180 o 270 gradi è un elemento della lista ts.
-}

-- isRotatedIn verifica se una delle rotazioni di t
-- (0°, 90°, 180°, 270°) appartiene alla lista ts.
--
-- Come negli esercizi precedenti, si applica simplify
-- sia a t (e alle sue rotazioni) sia agli elementi della lista,
-- per confrontare QuadTree in forma canonica.

isRotatedIn :: Eq a => QT a -> [QT a] -> Bool
isRotatedIn t ts =
    let
        -- rotazione 0° (identità)
        t0   = simplify t

        -- rotazione 90° in senso orario
        t90  = simplify (rotate90Right t)

        -- rotazione 180°
        t180 = simplify (rotate180 t)

        -- rotazione 270° (90° in senso antiorario)
        t270 = simplify (rotate90Left t)

        -- normalizzazione della lista
        ts'  = map simplify ts
    in
        -- True se almeno una delle rotazioni è presente nella lista
        any (`elem` ts') [t0, t90, t180, t270]

{-
Esercizio 19

Si riscriva la funzione howManyPixels dell’Esercizio 4 usando opportunamente fold.
-}


-- howManyPixelsF calcola il numero minimo di pixel
-- usando fold sul QuadTree.
--
-- Strategia:
-- invece di sommare i pixel dei sottoalberi (che sarebbe
-- scorretto se le profondità sono diverse),
-- si calcola prima l’altezza tramite fold,
-- e poi si applica la formula 4^h.
--
-- - Caso base (C _): altezza = 0.
-- - Caso ricorsivo (Q a b c d):
--       1 + massimo tra le altezze dei 4 figli.
--
-- Infine si restituisce 4 elevato all’altezza ottenuta.
howManyPixelsF :: QT a -> Int
howManyPixelsF t =
        let h = qtHeightF t
        in pow 4 h
    where
        -- qtHeightF calcola l’altezza usando fold.
        qtHeightF :: QT a -> Int
        qtHeightF =
            fold
                (\h1 h2 h3 h4 -> 1 + maximum [h1,h2,h3,h4])
                (const 0)

        -- pow calcola la potenza x^n.
        pow :: Int -> Int -> Int
        pow _ 0 = 1
        pow x n = x * pow x (n-1)
{-
Esercizio 20

Si riscriva la funzione occurrencies dell’Esercizio 6 usando opportunamente fold.
-}

-- occurrenciesF calcola il numero minimo di pixel di colore `col`
-- nell’immagine rappresentata dal QuadTree `q`, usando fold.
--
-- Strategia:
-- fold non restituisce direttamente solo il numero di occorrenze,
-- ma una coppia:
--
--     (h, o)
--
-- dove:
--   h = altezza del sottoalbero
--   o = numero di occorrenze di `col` misurate alla risoluzione
--       del sottoalbero stesso.
--
-- Quando si combinano i 4 quadranti, bisogna portarli alla stessa
-- risoluzione (quella del figlio più profondo). Se un figlio è meno
-- profondo, il suo contributo va moltiplicato per 4^(livelli mancanti),
-- perché rappresenta un blocco più grande.

occurrenciesF :: Eq a => QT a -> a -> Int
occurrenciesF q col =
    -- fold produce (altezzaTotale, occorrenzeTotali);
    -- snd estrae solo il numero di occorrenze
    snd (fold qf (`cf` col) q)
    where
        -- cf gestisce una foglia.
        --
        -- Una foglia ha altezza 0.
        -- Se il valore della foglia coincide con col', conta 1 blocco,
        -- altrimenti 0.
        cf :: Eq a => a -> a -> (Int, Int)
        cf x col' =
            (0, if x == col' then 1 else 0)

        -- qf combina i risultati dei 4 quadranti.
        --
        -- Ogni argomento è una coppia (altezza, occorrenze).
        -- Si calcola prima l’altezza del nodo corrente:
        --     h = 1 + massimo delle altezze dei figli.
        --
        -- Poi si espandono i conteggi dei figli meno profondi
        -- per portarli alla risoluzione comune (h - 1).
        qf :: (Int, Int)
            -> (Int, Int)
            -> (Int, Int)
            -> (Int, Int)
            -> (Int, Int)
        qf a b c d =
            let
                (ha, oa) = a
                (hb, ob) = b
                (hc, oc) = c
                (hd, od) = d

                -- Altezza del nodo corrente
                h = 1 + maximum [ha, hb, hc, hd]

                -- Espansione dei conteggi alla risoluzione comune
                oa' = oa * pow4 (h - 1 - ha)
                ob' = ob * pow4 (h - 1 - hb)
                oc' = oc * pow4 (h - 1 - hc)
                od' = od * pow4 (h - 1 - hd)
            in
                -- Restituisce altezza e somma delle occorrenze scalate
                (h, oa' + ob' + oc' + od')

        -- pow4 n = 4^n.
        -- Serve per espandere il contributo dei sottoalberi meno profondi.
        pow4 :: Int -> Int
        pow4 0 = 1
        pow4 n = 4 * pow4 (n - 1)

{-
Esercizio 21

Si scriva una funzione zipWith per QuadTrees che, analogamente alla zipWith per le liste, data un’operazione binaria ⊕ e due QuadTrees q1 e q2 costruisce il QuadTree che codifica l’immagine risultante dall’applicazione di ⊕ a tutti i pixel della stessa posizione nelle immagini codificate da q1 e q2 .
-}

-- zipWithQT è l’analogo di zipWith per i QuadTree.
-- Dati:
--   op  : operazione binaria da applicare ai pixel
--   q1  : prima immagine (QuadTree di valori a)
--   q2  : seconda immagine (QuadTree di valori b)
-- costruisce il QuadTree dell’immagine risultante applicando op
-- ai pixel corrispondenti (stessa posizione).
--
-- Problema: q1 e q2 possono avere strutture (profondità) diverse.
-- Se in una regione uno è una foglia (C x) e l’altro è un nodo (Q ...),
-- bisogna “espandere” la foglia replicandola sui 4 quadranti del nodo,
-- così da poter continuare l’applicazione pixel-per-pixel.
--
-- Alla fine si applica simplifyF per comprimere eventuali nodi Q che,
-- dopo l’applicazione di op, risultano omogenei.
-- Per questo serve Eq c: simplifyF/buildNSimplify confrontano i risultati.

zipWithQT :: Eq c => (a -> b -> c) -> QT a -> QT b -> QT c
zipWithQT op q1 q2 =
    -- go costruisce un termine QT combinando q1 e q2.
    -- simplifyF normalizza il risultato (compressione dove possibile).
    simplifyF (go op q1 q2)
    where
        -- go riceve op esplicitamente per evitare problemi di polimorfismo
        -- con signature locali: in questo modo i tipi (a, b, c) restano
        -- gli stessi della definizione esterna di zipWithQT.
        go :: (a -> b -> c) -> QT a -> QT b -> QT c

        -- Caso 1: entrambi foglie.
        -- Entrambe le regioni sono omogenee: basta applicare op ai valori.
        go op' (C x) (C y) =
            C (op' x y)

        -- Caso 2: entrambi nodi.
        -- Si applica ricorsivamente op ai quadranti corrispondenti
        -- (ul con ul, ur con ur, ll con ll, lr con lr).
        go op' (Q a b c d) (Q e f g h) =
            Q (go op' a e)
                (go op' b f)
                (go op' c g)
                (go op' d h)

        -- Caso 3: sinistra foglia, destra nodo.
        -- La foglia (C x) vale su tutta la regione, ma l’altro albero è già diviso
        -- in quadranti: per combinare pixel-per-pixel, si replica (C x)
        -- su ciascun quadrante del nodo destro e si procede ricorsivamente.
        go op' (C x) q@(Q {}) =
            let
                (ul, ur, ll, lr) = explode q
            in
                Q (go op' (C x) ul)
                    (go op' (C x) ur)
                    (go op' (C x) ll)
                    (go op' (C x) lr)

        -- Caso 4: sinistra nodo, destra foglia.
        -- Simmetrico del caso precedente: si replica (C y) sui quadranti
        -- del nodo sinistro e si procede ricorsivamente.
        go op' q@(Q {}) (C y) =
            let
                (ul, ur, ll, lr) = explode q
            in
                Q (go op' ul (C y))
                    (go op' ur (C y))
                    (go op' ll (C y))
                    (go op' lr (C y))

        -- explode estrae i 4 quadranti da un Q.
        -- È definita anche sul caso C per completezza: una foglia viene
        -- replicata in 4 foglie uguali (utile come “espansione” concettuale).
        explode :: QT t -> (QT t, QT t, QT t, QT t)
        explode (Q ul ur ll lr) = (ul, ur, ll, lr)
        explode (C x)           = (C x, C x, C x, C x)

{-
Esercizio 22

Si scriva una funzione Haskell insertPict che dati i QuadTrees di due immagini qt , qf ed un Quad- Tree “maschera” a valori booleani, costruisce il QuadTree dell’immagine risultante mantenendo i pixel di qt in corrispondenza del valore True (della maschera) oppure di qf in corrispondenza del valore False.
-}

-- insertPict combina due immagini (qt e qf) usando una maschera booleana m.
--
-- Per ogni pixel:
-- - se la maschera è True, si prende il pixel corrispondente di qt
-- - se la maschera è False, si prende il pixel corrispondente di qf
--
-- Implementazione:
-- 1) Si costruisce un QuadTree di coppie (tPix, fPix) combinando qt e qf
--    pixel-per-pixel con zipWithQT (,).
-- 2) Si combina la maschera m con questo QuadTree di coppie usando zipWithQT:
--    la funzione prende:
--      sel          : Bool dalla maschera
--      (tPix, fPix) : coppia dei due pixel nelle due immagini
--    e seleziona il valore finale con un if.
--
-- Eq a serve perché zipWithQT usa simplifyF/buildNSimplify per comprimere
-- il risultato quando possibile.

insertPict :: Eq a => QT a -> QT a -> QT Bool -> QT a
insertPict qt qf m =
    zipWithQT
        (\sel (tPix, fPix) -> if sel then tPix else fPix)
        m
        (zipWithQT (,) qt qf)

{-
Esercizio 23

Si scriva una funzione Haskell insertLogo che dati i QuadTrees di due immagini ql , qp ed un QuadTree “maschera” a valori booleani, costruisce il QuadTree dell’immagine risultante inserendo la figura ql all’interno del quadrante marcato * di qp scegliendo i pixel di ql o qp in corrispondenza del valore True o False della maschera.
-}

-- insertLogo inserisce l’immagine ql (logo) nel quadrante marcato (*) di qp,
-- usando una maschera booleana `mask` per decidere, pixel per pixel,
-- se prendere il valore dal logo (True) oppure dallo sfondo (False).
--
-- Assunzione implicita dell’esercizio:
-- il quadrante marcato (*) è l’upper-right (superiore-destro) di qp.
-- Per questo motivo il logo viene inserito in `ur`.
--
-- Passi:
-- 1) Si normalizza qp con simplify per evitare casi patologici/comprimibili
--    e lavorare su una struttura coerente.
-- 2) Se qp è una foglia (C x), l’immagine è 1x1 e non ha quadranti:
--    non c’è un “quadrante marcato”, quindi si restituisce invariata.
-- 3) Se qp è un nodo Q ul ur ll lr:
--    - si costruisce un nuovo QuadTree dove:
--        ul resta invariato
--        ur diventa il risultato di insertPict tra ql e ur usando mask
--        ll resta invariato
--        lr resta invariato
--    - buildNSimplify ricostruisce il nodo e comprime se i 4 quadranti
--      risultano tutti uguali (caso raro ma possibile).
--
-- Eq a serve perché insertPict/zipWithQT e buildNSimplify possono comprimere
-- confrontando i valori.

insertLogo :: Eq a => QT a -> QT a -> QT Bool -> QT a
insertLogo ql qp mask =
    case simplify qp of
        -- Caso immagine 1x1: non esistono quadranti
        C x ->
            C x

        -- Caso con quadranti: inserisco il logo nel superiore-destro (ur)
        Q ul ur ll lr ->
            buildNSimplify
                ul
                (insertPict ql ur mask)
                ll
                lr

{-
Esercizio 24

Si scriva una funzione Haskell commonPoints che data una lista non-vuota di QuadTrees l costruisce il QuadTree “maschera”, a valori booleani, che ha “un pixel” a True se nella medesima posizione tutte le immagini di l hanno pixels uguali, False altrimenti.
-}

-- commonPoints costruisce una “maschera” booleana a partire da una lista non vuota
-- di QuadTree (immagini).
--
-- La maschera risultante vale True in una posizione se, in quella posizione,
-- TUTTE le immagini hanno lo stesso pixel; vale False altrimenti.
--
-- Implementazione:
-- - La lista è non vuota: prendiamo il primo QuadTree `q` come riferimento.
-- - Per ogni altro QuadTree `q2` nella lista:
--     * costruiamo una maschera di uguaglianza pixel-per-pixel tra `q` e `q2`:
--           zipWithQT (==) q q2
--       che produce un QT Bool con True dove i pixel coincidono.
--     * combiniamo questa maschera con l’accumulatore `acc` usando (&&):
--           zipWithQT (&&) acc ...
--       così un pixel resta True solo se era True per tutti i confronti precedenti.
--
-- foldr scorre tutti gli elementi `qs` (cioè la lista senza il primo),
-- accumulando l’AND di tutte le maschere di uguaglianza rispetto a `q`.
--
-- Valore iniziale:
-- - (C True) rappresenta una maschera “tutto True” (immagine omogenea True),
--   neutro rispetto a (&&).
--
-- Eq a serve per il confronto (==) tra pixel nelle immagini.

commonPoints :: Eq a => [QT a] -> QT Bool
commonPoints [] = error "commonPoints: lista vuota"
commonPoints (q:qs) =
    foldr
        (\q2 acc ->
            zipWithQT (&&) acc (zipWithQT (==) q q2)
        )
        (C True)
        qs

{-
Esercizio 25

Si scriva un predicato framed che dato un predicato sui colori p ed un QuadTree determina se il bordo esterno dell’immagine codificata è tutto composto da pixels che soddisfano p.
-}

-- Struttura di supporto che contiene i quattro bordi esterni di un’immagine.
-- Ogni bordo è rappresentato come lista di valori (pixel) lungo quel lato.
data Border a = Border
    { topB    :: [a]  -- bordo superiore, da sinistra a destra
    , botB    :: [a]  -- bordo inferiore, da sinistra a destra
    , leftB   :: [a]  -- bordo sinistro, dall’alto verso il basso
    , rightB  :: [a]  -- bordo destro, dall’alto verso il basso
    } deriving (Eq, Show)

-- borders calcola i bordi esterni dell’immagine codificata dal QuadTree.
-- Restituisce anche l’altezza h del sottoalbero, perché la lunghezza dei bordi
-- dipende dalla risoluzione:
-- - se l’altezza è h, il lato dell’immagine è 2^h
-- - quindi ogni lista di bordo avrà lunghezza 2^h
--
-- Importante: il QuadTree può essere sbilanciato, quindi quando si combinano
-- i quattro quadranti bisogna prima “portare” i bordi dei figli alla stessa
-- risoluzione (altezza h-1) prima di concatenarli.

borders :: QT a -> (Int, Border a)

-- Caso foglia:
-- Altezza 0 => immagine 1x1, quindi ogni bordo è una lista di un solo elemento [x].
borders (C x) =
    (0, Border [x] [x] [x] [x])

-- Caso nodo:
-- Q ul ur ll lr rappresenta un’immagine divisa in 4 quadranti.
-- I bordi esterni del nodo si ottengono combinando i bordi dei figli:
-- - top    = top(ul) ++ top(ur)
-- - bottom = bot(ll) ++ bot(lr)
-- - left   = left(ul) ++ left(ll)
-- - right  = right(ur) ++ right(lr)
--
-- Ma prima serve normalizzare i bordi dei figli alla stessa altezza (h-1),
-- perché ul/ur/ll/lr possono avere altezze diverse.
borders (Q ul ur ll lr) =
    let
        -- Calcolo ricorsivo su ogni quadrante
        (hul, bul) = borders ul
        (hur, bur) = borders ur
        (hll, bll) = borders ll
        (hlr, blr) = borders lr

        -- Altezza del nodo corrente
        h = 1 + maximum [hul, hur, hll, hlr]

        -- Normalizza tutti i bordi dei figli all’altezza (h - 1)
        -- così tutte le liste avranno lunghezza 2^(h-1)
        bul' = liftTo (h - 1) hul bul
        bur' = liftTo (h - 1) hur bur
        bll' = liftTo (h - 1) hll bll
        blr' = liftTo (h - 1) hlr blr

        -- Combina i bordi esterni del nodo corrente concatenando le parti giuste
        top'   = topB  bul' ++ topB  bur'
        bot'   = botB  bll' ++ botB  blr'
        left'  = leftB bul' ++ leftB bll'
        right' = rightB bur' ++ rightB blr'
    in
        (h, Border top' bot' left' right')
    where
        -- liftTo porta un Border calcolato a altezza h0 alla risoluzione di altezza h1.
        -- Se h1 > h0, ogni “pixel” del bordo a risoluzione più bassa rappresenta
        -- più pixel a risoluzione più alta: lungo un lato si espande di un fattore
        -- 2^(h1 - h0).
        --
        -- Per ottenere la lista di lunghezza corretta, si replica ogni elemento
        -- esattamente k = 2^(h1 - h0) volte.
        liftTo :: Int -> Int -> Border a -> Border a
        liftTo h1 h0 b =
            let
                k = pow2 (h1 - h0)
            in
                Border (rep k (topB b))
                        (rep k (botB b))
                        (rep k (leftB b))
                        (rep k (rightB b))

        -- rep k applica replicate k a ogni elemento della lista e poi concatena.
        -- Esempio: rep 3 [a,b] = [a,a,a,b,b,b]
        rep :: Int -> [a] -> [a]
        rep k = concatMap (replicate k)

        -- pow2 n = 2^n
        pow2 :: Int -> Int
        pow2 0 = 1
        pow2 n = 2 * pow2 (n - 1)

-- framed verifica se il bordo esterno dell’immagine soddisfa un predicato p.
--
-- Passi:
-- 1) simplify q per lavorare su un QuadTree normalizzato (niente nodi comprimibili).
-- 2) calcola i bordi esterni con borders, ottenendo un Border a.
-- 3) controlla che tutti gli elementi di ciascun lato soddisfino p.
--
-- Nota: Eq a qui non è strettamente necessario per framed, ma può essere utile
-- altrove (ad esempio se si vuole normalizzare con funzioni che usano Eq).
framed :: Eq a => (a -> Bool) -> QT a -> Bool
framed p q =
    let
        (_, b) = borders (simplify q)
    in
        all p (topB b)
        && all p (botB b)
        && all p (leftB b)
        && all p (rightB b)

{-
Esercizio 26

Si scriva una funzione frame che dato un QuadTree restituisca Just c se il bordo esterno dell’im- magine codificata è tutto composto da pixels di colore c (Nothing altrimenti).
-}

-- frame controlla se il bordo esterno dell’immagine è uniforme, cioè composto
-- interamente dallo stesso colore c.
-- Restituisce:
--   - Just c  se tutti i pixel del bordo sono uguali a c
--   - Nothing altrimenti
--
-- Passi:
-- 1) simplify q normalizza il QuadTree (forma canonica, niente nodi comprimibili).
-- 2) borders estrae i quattro bordi esterni alla risoluzione corretta.
-- 3) si concatena tutto il bordo in un’unica lista:
--      top ++ bottom ++ left ++ right
-- 4) si controlla che tutti gli elementi siano uguali al primo.
--
-- Nota: i quattro lati condividono i pixel degli angoli, quindi nella lista
-- gli angoli compaiono più volte; questo non cambia il risultato perché
-- stiamo solo verificando l’uniformità.

frame :: Eq a => QT a -> Maybe a
frame q =
    let
        -- calcola i bordi del QuadTree normalizzato
        (_, b) = borders (simplify q)

        -- lista di tutti i pixel del bordo (con ripetizione degli angoli)
        allBorder = topB b ++ botB b ++ leftB b ++ rightB b
    in
        case allBorder of
            -- caso teorico: bordo vuoto (non dovrebbe accadere con immagini valide)
            [] ->
                Nothing

            -- prende il primo elemento come candidato colore del frame
            (x:xs) ->
                if all (== x) xs
                    then Just x
                    else Nothing