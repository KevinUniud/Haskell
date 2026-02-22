module MatriceMedianteQuadTrees
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
    ) where

{-
7. MATRICI MEDIANTE QUAD TREES

Grazie ai Quad Trees introdotti nella sezione precedente si possono implementare certe operazioni matriciali, nel caso dei linguaggi funzionali puri ovviamente, in modo molto più efficiente.
Si implementino matrici 2n × 2n utilizzando il seguente tipo di dato astratto (polimorfo)

data ( Eq a , Num a , Show a ) = > Mat a = Mat {
    nexp :: Int ,
    mat :: QT a
    }
    deriving ( Eq , Show )

dove nel campo mat non metteremo mai solo “termini di tipo QT” ma QuadTrees “propri”.
-}

data QT a
    = C a
    -- Foglia: rappresenta una matrice (sotto-blocco) costante, tutta piena del valore a
    | Q (QT a) (QT a) (QT a) (QT a)
    -- Nodo interno: suddivide il blocco in 4 quadranti (ordine usato qui):
    -- Q ul ur ll lr  = upper-left, upper-right, lower-left, lower-right
    deriving (Eq, Show)

buildNSimplify :: Eq a => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify a b c d =
    case (a, b, c, d) of
        -- Se i 4 figli sono foglie e contengono lo stesso valore,
        -- il quadrante completo è uniforme e lo si comprime in una singola foglia.
        (C x, C y, C z, C w) | x == y && y == z && z == w ->
            C x
        -- Altrimenti si mantiene il nodo interno.
        _ ->
            Q a b c d

simplifyQT :: Eq a => QT a -> QT a
simplifyQT (C x) =
    -- Una foglia è già nella forma più semplice.
    C x
simplifyQT (Q a b c d) =
    -- Semplifica ricorsivamente i figli e poi prova a comprimere il nodo.
    buildNSimplify (simplifyQT a) (simplifyQT b) (simplifyQT c) (simplifyQT d)

zipWithQT :: Eq c => (a -> b -> c) -> QT a -> QT b -> QT c
zipWithQT op q1 q2 =
    -- Applica op punto-a-punto tra due quad-tree, poi semplifica il risultato
    -- (es. se dopo l'operazione si ottengono 4 foglie uguali, si comprime).
    simplifyQT (go q1 q2)
    where
        go (C x) (C y) =
            -- Caso base: entrambi foglie -> si applica l'operazione ai valori.
            C (op x y)

        go (Q a b c d) (Q e f g h) =
            -- Entrambi nodi: combina ricorsivamente i quadranti corrispondenti.
            Q (go a e) (go b f) (go c g) (go d h)

        go (C x) q@(Q {}) =
            -- Sinistra foglia, destra nodo:
            -- la foglia rappresenta un blocco uniforme, quindi va "replicata"
            -- per poter combinare quadrante per quadrante.
            let (ul, ur, ll, lr) = explode q
            in Q (go (C x) ul) (go (C x) ur) (go (C x) ll) (go (C x) lr)

        go q@(Q {}) (C y) =
            -- Simmetrico: destra foglia, sinistra nodo.
            let (ul, ur, ll, lr) = explode q
            in Q (go ul (C y)) (go ur (C y)) (go ll (C y)) (go lr (C y))

        explode (Q ul ur ll lr) =
            -- Estrae i 4 quadranti se è un nodo.
            (ul, ur, ll, lr)
        explode (C x) =
            -- Se fosse una foglia, la si “espande” in 4 foglie uguali:
            -- utile per trattare uniformemente i casi misti (foglia vs nodo).
            (C x, C x, C x, C x)

data Mat a = Mat
    { nexp :: Int
    -- Esponente n: la matrice logica ha dimensione 2^n × 2^n
    , mat  :: QT a
    -- QuadTree che rappresenta la matrice (come blocchi ricorsivi).
    }
    deriving (Show)

instance Eq a => Eq (Mat a) where
    Mat n1 q1 == Mat n2 q2 =
        -- Due matrici sono uguali se hanno lo stesso esponente e
        -- i quad-tree sono uguali *a meno di semplificazioni*.
        n1 == n2 && simplifyQT q1 == simplifyQT q2

side :: Mat a -> Int
side (Mat n _) =
    -- Lato della matrice: 2^n
    pow2 n
    where
        pow2 0 =
            1
        pow2 k =
            2 * pow2 (k - 1)

idQT :: (Eq a, Num a) => Int -> QT a
idQT 0 =
    -- Caso n=0: matrice 1×1 identità => [1]
    C 1
idQT n =
    -- Identità 2^n × 2^n in forma a blocchi:
    -- [ I  0 ]
    -- [ 0  I ]
    let z = C 0
    in Q (idQT (n - 1)) z z (idQT (n - 1))

zeroQT :: Num a => Int -> QT a
zeroQT _ =
    -- Matrice (qualunque dimensione richiesta dal contesto) tutta zero:
    -- rappresentabile come singola foglia uniforme.
    C 0

splitQT :: Num a => QT a -> (QT a, QT a, QT a, QT a)
splitQT (Q a b c d) =
    -- Se è un nodo, restituisce direttamente i 4 quadranti.
    (a, b, c, d)
splitQT (C x) =
    -- Se è una foglia, la si interpreta come blocco uniforme e la si “splitta”
    -- replicandola in 4 sotto-blocchi uguali.
    (C x, C x, C x, C x)

{-
Esercizio 1

Si scriva un predicato lowertriangular che determina se una matrice è triangolare inferiore.

Attenti a cosa devono restituire

lowertriangular $ Mat 0 (C 2) e lowertriangular $ Mat 1 (C 2).
-}

isAllZeroQT :: (Eq a, Num a) => QT a -> Bool
isAllZeroQT (C x) =
    -- Un blocco uniforme è tutto zero se il valore della foglia è 0.
    x == 0
isAllZeroQT (Q a b c d) =
    -- Un blocco composto è tutto zero se lo sono tutti i suoi quadranti.
    isAllZeroQT a
        && isAllZeroQT b
        && isAllZeroQT c
        && isAllZeroQT d

lowertriangular :: (Eq a, Num a) => Mat a -> Bool
lowertriangular (Mat 0 _) =
    -- Matrice 1×1: è sempre triangolare inferiore.
    True
lowertriangular (Mat n q) =
    case q of
        C x ->
            -- Se l'intero blocco 2^n×2^n è uniforme:
            -- è triangolare inferiore solo se è tutto zero (per n>0),
            -- perché altrimenti avrebbe elementi non nulli anche sopra la diagonale.
            -- (Per n=0 il caso è già gestito sopra.)
            x == 0 || n == 0

        Q a b c d ->
            -- Per una matrice a blocchi:
            -- [ a  b ]
            -- [ c  d ]
            -- essere triangolare inferiore richiede:
            -- 1) il blocco in alto a destra (b) deve essere tutto zero
            -- 2) i blocchi diagonali (a e d) devono essere a loro volta triangolari inferiori
            -- (il blocco c sotto-diagonale è libero).
            isAllZeroQT b
                && lowertriangular (Mat (n - 1) a)
                && lowertriangular (Mat (n - 1) d)

{-
Esercizio 2

Si scriva un predicato uppertriangular che determina se una matrice è triangolare superiore.
-}

{-
uppertriangular determina se una matrice (2^n × 2^n) rappresentata come QuadTree
è triangolare superiore, cioè ha tutti gli elementi *sotto* la diagonale principale a 0.

Nel caso a blocchi:
    [ a  b ]
    [ c  d ]

la condizione “triangolare superiore” equivale a:
    1) c (blocco in basso a sinistra) deve essere tutto zero
    2) i blocchi diagonali a e d devono essere a loro volta triangolari superiori
(il blocco b sopra-diagonale è libero).
-}

uppertriangular :: (Eq a, Num a) => Mat a -> Bool
uppertriangular (Mat 0 _) =
    -- Matrice 1×1: è sempre triangolare superiore.
    True
uppertriangular (Mat n q) =
    case q of
        C x ->
            -- Blocco uniforme 2^n×2^n: per n>0 è triangolare superiore solo se è tutto zero,
            -- altrimenti avrebbe elementi non nulli anche sotto la diagonale.
            -- (Per n=0 il caso è già gestito sopra.)
            x == 0 || n == 0

        Q a b c d ->
            -- Struttura a quadranti:
            -- [ a  b ]
            -- [ c  d ]
            --
            -- Condizione chiave: il blocco sotto-diagonale (c) deve essere tutto zero.
            -- Poi si verifica ricorsivamente la proprietà sui blocchi diagonali a e d.
            isAllZeroQT c
                && uppertriangular (Mat (n - 1) a)
                && uppertriangular (Mat (n - 1) d)

{-
Esercizio 3

Si scriva un predicato diagonal che determina se una matrice è diagonale.
-}

{-
diagonal determina se una matrice (2^n × 2^n) è diagonale, cioè ha tutti gli elementi
fuori dalla diagonale principale uguali a 0.

In forma a blocchi:
    [ a  b ]
    [ c  d ]

la condizione “diagonale” equivale a:
    1) b (alto-destra) tutto zero
    2) c (basso-sinistra) tutto zero
    3) a e d (blocchi diagonali) devono essere a loro volta diagonali
-}

diagonal :: (Eq a, Num a) => Mat a -> Bool
diagonal (Mat 0 _) =
    -- Matrice 1×1: è sempre diagonale.
    True
diagonal (Mat n q) =
    case q of
        C x ->
            -- Blocco uniforme 2^n×2^n: per n>0 è diagonale solo se è tutto zero,
            -- altrimenti avrebbe valori non nulli anche fuori diagonale.
            -- (Per n=0 il caso è già gestito sopra.)
            x == 0 || n == 0

        Q a b c d ->
            -- Struttura a quadranti:
            -- [ a  b ]
            -- [ c  d ]
            --
            -- Per essere diagonale, entrambi i blocchi fuori diagonale (b e c) devono essere nulli,
            -- e i blocchi diagonali (a e d) devono soddisfare la proprietà ricorsivamente.
            isAllZeroQT b
                && isAllZeroQT c
                && diagonal (Mat (n - 1) a)
                && diagonal (Mat (n - 1) d)

{-
Esercizio 4

Si scriva una funzione matSum che date 2 matrici calcoli la matrice somma.
-}

{-
matSum calcola la somma di due matrici rappresentate come QuadTree.

Precondizione: le due matrici devono avere la stessa dimensione,
cioè lo stesso esponente n (dimensione 2^n × 2^n).

La somma viene eseguita punto-a-punto tramite zipWithQT (+),
che combina ricorsivamente i quadranti corrispondenti e poi
semplifica il risultato (compressione di blocchi uniformi).
-}

matSum :: (Eq a, Num a) => Mat a -> Mat a -> Mat a
matSum (Mat n a) (Mat m b)
    | n /= m =
        -- Non è possibile sommare matrici di dimensione diversa.
        error "matSum: dimensioni diverse"

    | otherwise =
        -- Si costruisce una nuova matrice con lo stesso esponente n
        -- e QuadTree ottenuto dalla somma punto-a-punto dei due alberi.
        --
        -- zipWithQT (+):
        --   - se entrambi foglie: somma i valori
        --   - se entrambi nodi: somma ricorsivamente i 4 quadranti
        --   - se uno è foglia e l'altro nodo: espande la foglia e procede ricorsivamente
        --   - infine semplifica il risultato
        Mat n (zipWithQT (+) a b)

{-
Esercizio 5

Si scriva una funzione matMul che date 2 matrici calcoli la matrice prodotto.
-}

{-
matMul calcola il prodotto tra due matrici 2^n × 2^n rappresentate come QuadTree.

Controlli iniziali:
    - se gli esponenti differiscono (dimensioni diverse) -> errore
    - se n == 0: matrici 1×1, il prodotto coincide con la moltiplicazione scalare

Caso ricorsivo (n > 0):
Si usa la decomposizione a blocchi (quadranti):

    X = [ a  b ]      Y = [ e  f ]
        [ c  d ]          [ g  h ]

e le formule del prodotto tra matrici a blocchi:

    R11 = a·e + b·g
    R12 = a·f + b·h
    R21 = c·e + d·g
    R22 = c·f + d·h

Ogni prodotto/somma tra blocchi è una chiamata ricorsiva a matMul/matSum
su matrici di dimensione dimezzata (esponente n' = n-1).

Alla fine si ricostruisce il QuadTree del risultato con buildNSimplify:
se i quattro quadranti risultano tutti foglie uguali, il nodo viene compresso.
-}

matMul :: (Eq a, Num a) => Mat a -> Mat a -> Mat a
matMul (Mat n x) (Mat m y)
    | n /= m =
        -- Non si possono moltiplicare matrici di dimensioni diverse.
        error "matMul: dimensioni diverse"

    | n == 0 =
        -- Caso base 1×1: prodotto scalare (qui riusiamo zipWithQT (*) sulle foglie).
        Mat 0 (zipWithQT (*) x y)

    | otherwise =
        let
            -- Divide i due QuadTree nei rispettivi quadranti:
            -- x = Q a b c d, y = Q e f g h (oppure foglie replicate via splitQT).
            (a, b, c, d) = splitQT x
            (e, f, g, h) = splitQT y

            -- Nuovo esponente per i sotto-blocchi (dimensione dimezzata).
            n' = n - 1

            -- Prodotto tra due quadranti: si calcola ricorsivamente matMul
            -- e si estrae il QuadTree risultante con `mat`.
            mul p q =
                mat (matMul (Mat n' p) (Mat n' q))

            -- Somma tra due quadranti: analogo, usando matSum.
            add p q =
                mat (matSum (Mat n' p) (Mat n' q))

            -- Applica le formule del prodotto a blocchi.
            r11 = add (mul a e) (mul b g)
            r12 = add (mul a f) (mul b h)
            r21 = add (mul c e) (mul d g)
            r22 = add (mul c f) (mul d h)

        -- Ricostruisce la matrice risultato e prova a semplificarla.
        in Mat n (buildNSimplify r11 r12 r21 r22)

{-
Esercizio 6

Si scriva una funzione zong che, dati due valori x, y e una matrice A, calcola la matrice xA − yI (dove I è la matrice unitaria della giusta dimensione).
-}

{-
zong calcola la matrice:  xA − yI

dove:
    - A è la matrice in input (dimensione 2^n × 2^n, con esponente n)
    - I è la matrice identità della stessa dimensione
    - xA è la matrice A scalata per x
    - yI è la matrice identità scalata per y
    - il risultato è la differenza punto-a-punto tra xA e yI

Nota implementativa:
    - Non c’è una funzione “mapQT” esplicita; qui si ottiene l’effetto di una mappa
        usando zipWithQT con un QuadTree “dummy” (C 0) e una lambda che ignora il
        secondo argomento.
    - zipWithQT semplifica sempre l’albero risultante (compressione di blocchi uniformi).
-}

zong :: (Eq a, Num a) => a -> a -> Mat a -> Mat a
zong x y (Mat n q) =
    let
        -- xA: scala tutti gli elementi di A per x.
        -- La lambda prende v (valore da q) e ignora il secondo valore (_).
        xA = zipWithQT (\v _ -> x * v) q (C 0)

        -- yI: costruisce l'identità della stessa dimensione (idQT n) e la scala per y.
        yI = zipWithQT (\v _ -> y * v) (idQT n) (C 0)

        -- res: sottrazione punto-a-punto tra xA e yI, cioè xA - yI.
        res = zipWithQT (-) xA yI
    in
        -- Ricostruisce la Mat mantenendo lo stesso esponente n.
        Mat n res

{-
Esercizio 7

Si scriva una funzione f che, dati un vettore v e una matrice A, calcola lo scalare vAv T .
Si scelga la struttura dati per i vettori nel modo che si ritiene più opportuno.
-}

{-
Scelta struttura dati vettori:
    type Vec a = [a]
Si rappresenta un vettore come lista di lunghezza 2^n (coerente con la matrice 2^n×2^n).

dot:
    Prodotto scalare tra due vettori: somma dei prodotti componente-per-componente.
    Usa zipWith (*) per moltiplicare le componenti e sum per sommare i risultati.
-}

type Vec a = [a]

dot :: Num a => Vec a -> Vec a -> a
dot xs ys =
    -- dot(xs,ys) = Σ_i xs[i] * ys[i]
    sum (zipWith (*) xs ys)

{-
matVec:
    Moltiplicazione matrice-vettore: dato A (2^n×2^n) e v (lunghezza 2^n),
    restituisce A·v (vettore lunghezza 2^n).

Caso base n==0:
    - La matrice è 1×1, quindi q deve essere una foglia C x.
    - Il risultato è [x * v0].

Caso ricorsivo (n>0):
    Si usa la decomposizione a blocchi della matrice e la partizione del vettore:

    A = [ a  b ]      v = [ v1 ]
        [ c  d ]          [ v2 ]

    dove v1 e v2 hanno lunghezza 2^(n-1).

    Allora:
        A·v = [ a·v1 + b·v2 ]
              [ c·v1 + d·v2 ]

    Ogni prodotto blocco-vettore è una chiamata ricorsiva a matVec
    su una matrice di esponente n' = n-1.

Nota:
    - splitQT gestisce anche il caso in cui q sia una foglia: in quel caso replica la foglia
        nei 4 quadranti, interpretandola come blocco uniforme.
    - pow2 serve per calcolare la dimensione del mezzo vettore (2^(n-1)).
-}

matVec :: (Eq a, Num a) => Mat a -> Vec a -> Vec a
matVec (Mat n q) v
    | n == 0 =
        case q of
            C x ->
                -- A è 1×1: [x]. v deve avere almeno un elemento.
                [x * head v]
            Q{} ->
                -- Per n==0 ci si aspetta una foglia; un nodo qui è incoerente.
                error "matVec: QT non coerente per n=0"

    | otherwise =
        let
            -- Estrae i quadranti della matrice:
            -- [a b; c d]
            (a, b, c, d) = splitQT q
            n' = n - 1

            -- Divide v in due metà v1 e v2, ciascuna lunga 2^(n-1)
            (v1, v2) = splitAt (pow2 n') v

            -- Calcola i 4 prodotti blocco-vettore ricorsivamente
            aV1 = matVec (Mat n' a) v1
            bV2 = matVec (Mat n' b) v2
            cV1 = matVec (Mat n' c) v1
            dV2 = matVec (Mat n' d) v2

            -- Combina i risultati secondo le formule:
            -- top = a·v1 + b·v2
            -- bot = c·v1 + d·v2
            top = zipWith (+) aV1 bV2
            bot = zipWith (+) cV1 dV2
        in
            -- Concatenazione delle due metà del vettore risultato
            top ++ bot
    where
        -- Potenza di 2 usata per calcolare la lunghezza dei sotto-vettori.
        pow2 0 =
            1
        pow2 k =
            2 * pow2 (k - 1)

{-
f calcola lo scalare v A v^T (con v considerato vettore riga).

Implementazione:
    1) av = A·v  (matVec a v)
    2) risultato = v · av  (dot v av)

Questo corrisponde a: v (A v^T) = v A v^T, che è uno scalare.
-}

f :: (Eq a, Num a) => Vec a -> Mat a -> a
f v a =
    let
        -- av = A·v
        av = matVec a v
    in
        -- v·(A·v)
        dot v av

{-
Esercizio 8

Si scriva una funzione colSums che data una matrice calcola il vettore delle somme delle colonne della matrice.

Ad esempio

let z = C 0; u = C 1; d = C 2 in colsums $ Mat 3 $ Q ( Q u d d u ) z u d

deve produrre [10,10,10,10,8,8,8,8].
-}

{-
pow2:
    Calcola 2^k in modo ricorsivo (serve per ottenere la dimensione del lato, 2^n).
-}
pow2 :: Int -> Int
pow2 0 =
    1
pow2 k =
    2 * pow2 (k - 1)

{-
replicateN:
    Alias esplicito di replicate, per rendere più leggibile l’intento:
    creare una lista di lunghezza N con lo stesso valore ripetuto.
-}
replicateN :: Int -> a -> [a]
replicateN = replicate

{-
colsumsQT:
    Calcola il vettore delle somme delle colonne per un QuadTree che rappresenta
    una matrice 2^n × 2^n (n passato esplicitamente).

    Restituisce una lista di lunghezza 2^n:
        - l’i-esima posizione è la somma degli elementi della colonna i-esima.

Caso C x (blocco uniforme):
    Il blocco rappresenta una matrice 2^n×2^n in cui ogni elemento vale x.
    Ogni colonna contiene 2^n elementi, quindi la somma di una colonna è:
        colSum = x * (2^n)
    Poiché tutte le colonne sono uguali, il risultato è una lista lunga 2^n
    che replica colSum.

    Nota: serve fromIntegral perché pow2 n è Int, mentre x è Num a.

Caso Q a b c d (matrice a blocchi):
    La matrice è:
        [ a  b ]
        [ c  d ]
    con a,b,c,d di dimensione 2^(n-1) × 2^(n-1).

    Le somme di colonna si ottengono combinando per “verticale”:
        - le colonne della parte sinistra sono somma delle colonne di a e c
        - le colonne della parte destra sono somma delle colonne di b e d

    Quindi:
        left  = colsums(a) + colsums(c)
        right = colsums(b) + colsums(d)
    e il vettore finale è left ++ right (prima le colonne di sinistra, poi quelle di destra).
-}

colsumsQT :: Num a => Int -> QT a -> [a]
colsumsQT n q =
    case q of
        C x ->
            let
                -- Numero di colonne della matrice/blocco: 2^n
                len = pow2 n
                -- Somma di una singola colonna: x ripetuto 2^n volte
                colSum = x * fromIntegral (pow2 n)
            in
                -- Tutte le colonne hanno la stessa somma
                replicateN len colSum

        Q a b c d ->
            let
                -- Dimensione dimezzata per i quadranti
                n' = n - 1

                -- Colonne del lato sinistro: somma (a sopra) + (c sotto)
                left  = zipWith (+) (colsumsQT n' a) (colsumsQT n' c)

                -- Colonne del lato destro: somma (b sopra) + (d sotto)
                right = zipWith (+) (colsumsQT n' b) (colsumsQT n' d)
            in
                -- Colonne totali: prima metà (sinistra) poi seconda metà (destra)
                left ++ right

{-
colSums:
    Interfaccia a livello Mat: prende l’esponente n dalla Mat e delega a colsumsQT.

    L’Eq nel vincolo non è usato qui direttamente, ma può essere richiesto altrove
    nella consegna per uniformità con altre funzioni su Mat.
-}

colSums :: (Eq a, Num a) => Mat a -> [a]
colSums (Mat n q) =
    colsumsQT n q

{-
Esercizio 9

Si scriva una funzione rowSums che data una matrice calcola il vettore delle somme delle righe della matrice.
-}

{-
rowsumsQT:
    Calcola il vettore delle somme delle righe per un QuadTree che rappresenta
    una matrice 2^n × 2^n.

    Restituisce una lista di lunghezza 2^n:
        - l’i-esima posizione è la somma degli elementi della riga i-esima.

Caso C x (blocco uniforme):
    Il blocco rappresenta una matrice 2^n×2^n in cui ogni elemento vale x.
    Ogni riga contiene 2^n elementi, quindi la somma di una riga è:
        rowSum = x * (2^n)
    Tutte le righe sono uguali, quindi si replica rowSum 2^n volte.

Caso Q a b c d (matrice a blocchi):
    La matrice è:
        [ a  b ]
        [ c  d ]
    con a,b,c,d di dimensione 2^(n-1) × 2^(n-1).

    Le somme di riga si ottengono combinando per “orizzontale”:
        - le righe della parte superiore sono somma delle righe di a e b
        - le righe della parte inferiore sono somma delle righe di c e d

    Quindi:
        top = rowsums(a) + rowsums(b)
        bot = rowsums(c) + rowsums(d)
    e il vettore finale è top ++ bot (prima le righe superiori, poi le inferiori).
-}

rowsumsQT :: Num a => Int -> QT a -> [a]
rowsumsQT n q =
    case q of
        C x ->
            let
                -- Numero di righe della matrice/blocco: 2^n
                len = pow2 n
                -- Somma di una singola riga: x ripetuto 2^n volte
                rowSum = x * fromIntegral (pow2 n)
            in
                -- Tutte le righe hanno la stessa somma
                replicateN len rowSum

        Q a b c d ->
            let
                -- Dimensione dimezzata per i quadranti
                n' = n - 1

                -- Righe della metà superiore: somma (a a sinistra) + (b a destra)
                top = zipWith (+) (rowsumsQT n' a) (rowsumsQT n' b)

                -- Righe della metà inferiore: somma (c a sinistra) + (d a destra)
                bot = zipWith (+) (rowsumsQT n' c) (rowsumsQT n' d)
            in
                -- Righe totali: prima metà superiore poi metà inferiore
                top ++ bot

{-
rowSums:
    Interfaccia a livello Mat: prende l’esponente n dalla Mat e delega a rowsumsQT.
-}

rowSums :: (Eq a, Num a) => Mat a -> [a]
rowSums (Mat n q) =
    rowsumsQT n q

{-
Esercizio 10

Si scriva una funzione colMinMax che data una matrice calcola il vettore delle coppie (minimo,massimo) delle colonne della matrice.
-}

{-
colMinMaxQT:
    Calcola, per una matrice 2^n × 2^n rappresentata come QuadTree,
    il vettore delle coppie (minimo, massimo) per ciascuna colonna.

    Restituisce una lista di lunghezza 2^n:
        - l’i-esimo elemento è (min_i, max_i), dove:
            min_i = minimo della colonna i
            max_i = massimo della colonna i

Caso C x (blocco uniforme):
    Il blocco rappresenta una matrice in cui tutti gli elementi valgono x.
    Ogni colonna contiene solo x, quindi:
        minimo = x
        massimo = x
    e si replica la coppia (x,x) per tutte le 2^n colonne.

Caso Q a b c d (matrice a blocchi):
    Struttura:
        [ a  b ]
        [ c  d ]

    Le colonne della metà sinistra derivano da:
        colonne(a) sopra
        colonne(c) sotto
    quindi si combinano le coppie (min,max) di a e c con:
        min = min mn1 mn2
        max = max mx1 mx2

    Analogamente, le colonne della metà destra derivano da b e d.

    Il risultato finale è:
        left ++ right
    (prima le colonne sinistre, poi le destre).
-}

colMinMaxQT :: (Ord a, Num a) => Int -> QT a -> [(a, a)]
colMinMaxQT n q =
    case q of
        C x ->
            let
                -- Numero di colonne: 2^n
                len = pow2 n
            in
                -- Ogni colonna ha minimo e massimo uguali a x
                replicateN len (x, x)

        Q a b c d ->
            let
                n' = n - 1

                -- Colonne della metà sinistra: combinazione verticale di a e c
                left  = zipWith mm (colMinMaxQT n' a)
                                    (colMinMaxQT n' c)

                -- Colonne della metà destra: combinazione verticale di b e d
                right = zipWith mm (colMinMaxQT n' b)
                                    (colMinMaxQT n' d)
            in
                left ++ right
    where
        -- Combina due coppie (min,max) provenienti dalla parte sopra e sotto
        mm (mn1, mx1) (mn2, mx2) =
            (min mn1 mn2, max mx1 mx2)

{-
colMinMax:
    Interfaccia a livello Mat: usa l’esponente n e delega a colMinMaxQT.
-}

colMinMax :: (Eq a, Ord a, Num a) => Mat a -> [(a, a)]
colMinMax (Mat n q) =
    colMinMaxQT n q

{-
Esercizio 11

Si scriva una funzione colVar che, data una matrice, calcola il vettore delle variazioni (= massimo - minimo) delle colonne della matrice.
-}

{-
colVar:
    Calcola, per ciascuna colonna della matrice, la variazione definita come:

        variazione = massimo - minimo

    Si riutilizza la funzione colMinMax, che restituisce per ogni colonna
    la coppia (minimo, massimo). Per ogni coppia si calcola mx - mn.

    Il risultato è una lista di lunghezza 2^n, dove n è l’esponente della matrice.
-}

colVar :: (Eq a, Ord a, Num a) => Mat a -> [a]
colVar m =
    -- Per ogni colonna: (mn, mx) -> mx - mn
    [mx - mn | (mn, mx) <- colMinMax m]


{-
Esercizio 12

Si scriva una funzione colAltSums che calcola il vettore delle somme a segni alternati delle colonne della matrice.

Detto s_j = sum_{i=1}{n} (−1)^{i+1} a_{ij} , \texttt{colaltsums}(
\begin{pmatrix}
a_{11} & \dots & a_{1m} \\
\vdots & \ddots & \vdots \\
a_{n1} & \dots & a_{nm}
\end{pmatrix}
) = (s_1 \dots s_m)
-}

{-
colAltSumsQT:
    Calcola, per una matrice 2^n × 2^n rappresentata come QuadTree,
    il vettore delle somme a segni alternati delle colonne:

        s_j = Σ_i (-1)^(i+1) a_ij

    cioè: + riga1 − riga2 + riga3 − riga4 ...

    Caso C x (blocco uniforme):
    Il blocco rappresenta una matrice 2^n×2^n con tutti gli elementi uguali a x.
    Per una singola colonna:

        s = x * (1 - 1 + 1 - 1 + ...)

    Se il numero di righe h = 2^n è pari → la somma alternata vale 0.
    Se fosse dispari → varrebbe x (ma per n>0, 2^n è sempre pari).

    Si replica il valore s per tutte le colonne.

Caso Q a b c d:
    Struttura:
        [ a  b ]
        [ c  d ]

    Le colonne della metà sinistra derivano da a (righe sopra) e c (righe sotto).
    Le colonne della metà destra derivano da b e d.

    Se il numero di righe del blocco superiore (topRows = 2^(n-1)) è dispari,
    il segno delle righe nel blocco inferiore si inverte rispetto alla parte alta.
    Se è pari, il segno riparte positivo nel blocco inferiore.

    Quindi:
        - se topRows è dispari → si combinano con sottrazione
        - se topRows è pari   → si combinano con somma

    Risultato finale: left ++ right.
-}

colAltSumsQT :: Num a => Int -> QT a -> [a]
colAltSumsQT n q =
    case q of
        C x ->
            let
                len = pow2 n      -- numero di colonne
                h   = pow2 n      -- numero di righe
                -- Somma alternata di una colonna uniforme
                s = if even h then 0 else x
            in
                replicateN len s

        Q a b c d ->
            let
                n' = n - 1

                -- Somme alternate dei quadranti
                topLeft  = colAltSumsQT n' a
                topRight = colAltSumsQT n' b
                botLeft  = colAltSumsQT n' c
                botRight = colAltSumsQT n' d

                -- Numero di righe nella parte superiore
                topRows = pow2 n'

                -- Combinazione verticale con gestione del segno
                (left, right) =
                    if odd topRows
                    then
                        ( zipWith (-) topLeft  botLeft
                        , zipWith (-) topRight botRight
                        )
                    else
                        ( zipWith (+) topLeft  botLeft
                        , zipWith (+) topRight botRight
                        )
            in
                left ++ right

{-
colAltSums:
    Interfaccia a livello Mat: usa l’esponente n e delega a colAltSumsQT.
-}

colAltSums :: (Eq a, Num a) => Mat a -> [a]
colAltSums (Mat n q) =
    colAltSumsQT n q

{-
Esercizio 13

Si scriva una funzione transpose che calcola la matrice trasposta.
-}

{-
transposeQT:
    Calcola la trasposta di una matrice rappresentata come QuadTree.

Caso C x:
    Una foglia rappresenta un blocco uniforme; la trasposta di un blocco
    uniforme è ancora lo stesso blocco.

Caso Q a b c d:
    La matrice è strutturata come:

        [ a  b ]
        [ c  d ]

    La trasposta scambia righe e colonne:

        [ a^T  c^T ]
        [ b^T  d^T ]

    Quindi si scambiano i blocchi fuori diagonale (b e c)
    e si applica ricorsivamente la trasposta ai quattro quadranti.
-}

transposeQT :: Eq a => QT a -> QT a
transposeQT (C x) =
    -- Blocco uniforme: invariato.
    C x

transposeQT (Q a b c d) =
    -- Scambio dei blocchi fuori diagonale + ricorsione.
    Q (transposeQT a)
      (transposeQT c)
      (transposeQT b)
      (transposeQT d)

{-
transposeMat:
    Interfaccia a livello Mat.

    Mantiene lo stesso esponente n (la trasposta ha stessa dimensione)
    e applica transposeQT al QuadTree.

    simplifyQT viene usata per comprimere eventuali blocchi uniformi
    che possono emergere dopo la trasposizione.
-}

transposeMat :: (Eq a, Num a) => Mat a -> Mat a
transposeMat (Mat n q) =
    Mat n (simplifyQT (transposeQT q))


{-
Esercizio 14

Si scriva un predicato isSymmetric che determina se una matrice è simmetrica.
-}

{-
isSymmetric:
    Una matrice è simmetrica se A = A^T.

    Si calcola la trasposta con transposeMat e si confrontano
    i QuadTree sottostanti tramite l’uguaglianza strutturale.

    L’uguaglianza tra Mat è già definita in modo coerente
    (usa simplifyQT), quindi è sufficiente confrontare i campi mat.
-}

isSymmetric :: (Eq a, Num a) => Mat a -> Bool
isSymmetric m =
    -- Confronto tra la matrice originale e la sua trasposta
    mat m == mat (transposeMat m)

{-
Esercizio 15

Si scriva una funzione foldMat con tipo
foldMat :: ( Num a ) =>
    ( Int -> b -> b -> b -> b -> b ) ->
    ( Int -> a -> b ) -> Mat a -> b
-}

{-
foldMat:
    È un operatore di eliminazione (fold) strutturale per matrici
    rappresentate come QuadTree.

Tipo:

    foldMat
        :: Num a
        => (Int -> b -> b -> b -> b -> b)  -- caso nodo (Q)
        -> (Int -> a -> b)                 -- caso foglia (C)
        -> Mat a
        -> b

Significato dei parametri:

    - qf: funzione da applicare quando si incontra un nodo Q.
            Riceve:
            * l’esponente n del blocco corrente
            * i quattro risultati ricorsivi dei quadranti
            e produce il risultato combinato.

    - cf: funzione da applicare quando si incontra una foglia C.
            Riceve:
            * l’esponente n del blocco corrente
            * il valore scalare x della foglia
            e produce il risultato.

    - Mat a: matrice su cui effettuare il fold.

    L’idea è analoga a un fold su alberi:
    - alle foglie si applica cf
    - ai nodi interni si combinano ricorsivamente i risultati dei figli con qf
-}

foldMat
    :: Num a
    => (Int -> b -> b -> b -> b -> b)
    -> (Int -> a -> b)
    -> Mat a
    -> b

foldMat qf cf (Mat n0 qt0) =
        go n0 qt0
    where
        go n (C x) =
            -- Caso foglia: applica la funzione cf
            -- passando l’esponente corrente e il valore contenuto.
            cf n x

        go n (Q a b c d) =
            let
                -- I quadranti rappresentano blocchi di dimensione 2^(n-1)
                n' = n - 1
            in
                -- Caso nodo: calcola ricorsivamente i risultati dei quattro figli
                -- e li combina con qf.
                qf n
                    (go n' a)
                    (go n' b)
                    (go n' c)
                    (go n' d)