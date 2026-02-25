module Matrici
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
    ) where

{-
3. MATRICI

Le matrici si implementano come liste di liste, per righe o per colonne a seconda delle preferenze.
-}

type Matrice a = [[a]]

{-
Esercizio 1

Si scriva una funzione matrix_dim che data una matrice ne calcola le dimensioni, se la matrice è ben formata, altrimenti restituisce (-1,-1).
-}


{-
checkRows:
    Conta il numero di righe della matrice.
    Usa foldl per incrementare un contatore iniziale r
    di 1 per ogni riga presente nella lista esterna.
-}

checkRows :: (Eq a, Num a) => Matrice a -> a -> a
checkRows xs r =
    foldl (\r _ -> r + 1) r xs

{-
countCols:
    Conta il numero di colonne di una singola riga.
    Usa foldr per sommare 1 per ogni elemento della lista.
-}

countCols :: (Eq a, Num a) => [a] -> a
countCols =
    foldr (\_ acc -> acc + 1) 0

{-
matrixSupp:
    Funzione di supporto che verifica che la matrice sia ben formata,
    cioè che tutte le righe abbiano lo stesso numero di colonne.

    Parametri:
        - lista delle righe
        - r = numero totale di righe (già calcolato)
        - c = numero atteso di colonne
            inizialmente vale -1 (non ancora fissato)

    Logica:
        - Se la lista è vuota -> restituisce (r, c)
        - Se c == -1 -> si fissa c al numero di colonne della prima riga
        - Se la riga corrente ha lo stesso numero di colonne di c -> continua
        - Altrimenti -> matrice mal formata, restituisce (-1, -1)
-}

matrixSupp :: (Eq a, Num a) => Matrice a -> a -> a -> (a, a)
matrixSupp [] r c =
    (r, c)

matrixSupp (x:xs) r c
    | c == (-1) =
        -- Prima riga: fissa il numero di colonne atteso
        matrixSupp xs r (countCols x)

    | countCols x == c =
        -- Riga coerente con il numero di colonne atteso
        matrixSupp xs r c

    | otherwise =
        -- Riga con numero di colonne diverso -> matrice non ben formata
        (-1, -1)

{-
matrixDim:
    Restituisce la coppia (numero_righe, numero_colonne)
    se la matrice è ben formata.

    Caso []:
        Matrice vuota -> dimensione (0,0).

    Caso generale:
        - Calcola il numero di righe con checkRows
        - Verifica la coerenza delle colonne con matrixSupp
        - Se non coerente -> (-1,-1)
-}

matrixDim :: (Eq a, Num a) => Matrice a -> (a, a)
matrixDim [] =
    (0, 0)

matrixDim xs =
    matrixSupp xs (checkRows xs 0) (-1)

{-
Esercizio 2

Si scriva una funzione colsums che data una matrice calcola il vettore delle somme delle colonne.
-}

{-
sumCols:
    Somma elemento per elemento due vettori (liste) della stessa lunghezza.

    È una somma “colonna per colonna” tra due righe:
        [a1,a2,...] + [b1,b2,...] = [a1+b1, a2+b2, ...]

    I casi con lista vuota permettono di gestire correttamente
    l’inizializzazione nel fold.
-}

sumCols :: Num a => [a] -> [a] -> [a]
sumCols xs [] = xs
sumCols [] ys = ys
sumCols (x:xs) (y:ys) =
    (x + y) : sumCols xs ys

{-
colsums:
    Calcola il vettore delle somme delle colonne di una matrice
    rappresentata come lista di righe.

    Idea:
        - La prima riga viene usata come accumulatore iniziale.
        - Ogni riga successiva viene sommata componente per componente
        tramite sumCols.
        - Il risultato finale è un vettore in cui la posizione i-esima
        contiene la somma degli elementi della colonna i-esima.

    Esempio:
        [[1,2,3],
        [4,5,6]]
    -> [5,7,9]
-}

colsums :: Num a => Matrice a -> [a]
colsums [] =
    []

colsums (x:xs) =
    foldl sumCols x xs

{-
Esercizio 3

Si scriva una funzione colaltsums che, data una matrice implementata come liste di liste per righe, calcola il vettore dellesomme a segni alternati delle colonne della matrice.

Detto s_j =
\sum_{i}^{n} (-1)^{i+1} a_{ij}, \texttt{colaltsums} (
\begin{pmatrix}
a_{11} & \dots & a_{1m} \\
\vdots & \ddots & \vdots \\
a_{n1} & \dots & a_{nm}
\end{pmatrix}
) = (s_1 \dots s_m)
-}

{-
sums:
  Combina due vettori (liste) componente-per-componente, scegliendo se
  aggiungere o sottrarre il secondo vettore in base al booleano `neg`.

  Parametri:
    - primo vettore (accumulatore corrente)
    - secondo vettore (riga da combinare)
    - neg:
        * True  -> accumulatore + ( - riga )  cioè accumulatore - riga
        * False -> accumulatore + riga

  I casi base con lista vuota interrompono quando termina uno dei due vettori.
  (In una matrice ben formata le righe hanno stessa lunghezza, quindi finiscono insieme.)
-}
sums :: Num a => [a] -> [a] -> Bool -> [a]
sums [] _ _ = []
sums _ [] _ = []
sums (x:xs) (y:ys) neg =
    if neg
        then
            -- Somma alternata: sottrae la componente corrispondente
            (x + (y * (-1))) : sums xs ys neg
        else
            -- Somma normale: aggiunge la componente corrispondente
            (x + y) : sums xs ys neg

{-
colaltsums:
    Calcola il vettore delle somme a segni alternati delle colonne,
    assumendo la matrice rappresentata come lista di righe.

    Definizione:
        s_j = Σ_i (-1)^(i+1) a_ij
    cioè: + riga1 − riga2 + riga3 − riga4 ...

Strategia:
    - Si parte dall’accumulatore iniziale uguale alla prima riga (x).
    - Si processano le righe successive una per volta, alternando:
        * sottrazione (neg=True)
        * addizione   (neg=False)
        in modo da rispettare i segni alternati per colonna.

Perché l’accumulatore iniziale è la prima riga:
    La formula vuole che la riga 1 sia con segno “+”, quindi si inizia da lì
    e poi si alterna sui restanti termini.

Implementazione:
    colaltsumsSupp xs acc neg:
        - xs  = righe rimanenti da processare
        - acc = vettore accumulato delle somme per colonna
        - neg = indica se la prossima riga va sottratta (True) o aggiunta (False)
-}

colaltsums :: Num a => Matrice a -> [a]
colaltsums [] =
    []

colaltsums (x:xs) =
        colaltsumsSupp xs x True
    where
        colaltsumsSupp [] acc _ =
            -- Finite le righe: acc contiene le somme alternate per colonna
            acc

        colaltsumsSupp (r:rs) acc True =
            -- Prossima riga con segno negativo: acc - r
            colaltsumsSupp rs (sums acc r True) False

        colaltsumsSupp (r:rs) acc False =
            -- Prossima riga con segno positivo: acc + r
            colaltsumsSupp rs (sums acc r False) True

{-
Esercizio 4

Si scriva una funzione colMinMax che, data una matrice implementata come liste di liste per righe, calcola il vettore delle coppie (minimo,massimo) delle colonne della matrice.
-}

{-
findMin:
  Calcola il minimo di una lista non vuota in modo ricorsivo.
-}
findMin :: (Num a, Ord a) => [a] -> a
findMin [x] =
    x
findMin (x:xs) =
    min x (findMin xs)

{-
findMax:
    Calcola il massimo di una lista non vuota in modo ricorsivo.
-}
findMax :: (Num a, Ord a) => [a] -> a
findMax [x] =
    x
findMax (x:xs) =
    max x (findMax xs)

{-
colMinMaxSupp:
    Assume di ricevere una matrice già trasposta, quindi
    ogni lista interna rappresenta una colonna della matrice originale.

    Per ogni “colonna” (ora riga della trasposta) costruisce
    la coppia (minimo, massimo).
-}

colMinMaxSupp :: (Num a, Ord a) => Matrice a -> [(a,a)]
colMinMaxSupp [x] =
    [(findMin x, findMax x)]

colMinMaxSupp (x:xs) =
    (findMin x, findMax x) : colMinMaxSupp xs

{-
colMinMax:
    Calcola il vettore delle coppie (minimo, massimo) per ogni colonna
    di una matrice rappresentata come lista di righe.

    Strategia:
        1) Si trasforma la matrice nella sua trasposta,
        così le colonne diventano righe.
        2) Per ogni riga della trasposta si calcolano minimo e massimo.

    Caso matrice vuota: restituisce lista vuota.
-}

colMinMax :: (Num a, Ord a) => Matrice a -> [(a,a)]
colMinMax [] =
    []

colMinMax xs =
    colMinMaxSupp (trasposta xs)

{-
Esercizio 5

Si scriva un predicato lowertriangular che determina se una matrice (quadrata) è triangolare inferiore.
A titolo di esempio, lowertriangular([[1,0,0],[2,-3,0],[4,5,6]]) restituisce True, mentre lowertriangular([[0,0,1],[2,-3,0],[4,5,6]]) restituisce False.
-}

{-
checkRowZero:
    Verifica che, in una riga, tutti gli elementi con indice < col
    (cioè a sinistra della diagonale) possano essere qualsiasi valore,
    mentre tutti quelli con indice >= col (cioè sopra la diagonale
    rispetto alla riga corrente) siano 0.

    Parametri:
        - lista della riga
        - k   = indice corrente di colonna
        - col = indice a partire dal quale gli elementi devono essere 0
        - n   = dimensione (non usato nel controllo, ma passato per coerenza)
-}

checkRowZero :: (Num a, Ord a) => [a] -> a -> a -> a -> Bool
checkRowZero [] _ _ _ =
    True

checkRowZero (x:xs) k col n
    | k < col =
        -- Prima della diagonale: nessun vincolo
        checkRowZero xs (k + 1) col n

    | x == 0 =
        -- Sulla o sopra la parte che deve essere nulla
        checkRowZero xs (k + 1) col n

    | otherwise =
        -- Trovato un elemento non nullo sopra la diagonale
        False

{-
lowertriangularSupp:
    Verifica ricorsivamente che ogni riga soddisfi la proprietà
    di matrice triangolare inferiore.

    Una matrice è triangolare inferiore se:
        per ogni riga i,
        tutti gli elementi con indice di colonna j > i sono 0.

    Parametri:
        - lista delle righe
        - n   = dimensione della matrice (numero di righe)
        - row = indice riga corrente
        - col = indice a partire dal quale devono esserci solo zeri
                (inizialmente 1 per la prima riga)

    La ricorsione:
        - controlla la riga corrente
        - passa alla successiva incrementando row e col
-}

lowertriangularSupp
    :: (Num a, Ord a)
    => Matrice a -> a -> a -> a -> Bool

lowertriangularSupp [] _ _ _ =
    True

lowertriangularSupp (x:xs) n row col =
    ( row >= (n - 1) )
    ||
    ( checkRowZero x 0 col n
        && lowertriangularSupp xs n (row + 1) (col + 1)
    )

{-
getMatrixDim:
    Conta il numero di righe della matrice (dimensione n).
-}

getMatrixDim :: Num a => Matrice a -> a -> a
getMatrixDim xs i =
    foldl (\i _ -> i + 1) i xs

{-
lowertriangular:
    Predicato principale.

    Calcola la dimensione n della matrice e avvia
    il controllo ricorsivo a partire dalla prima riga (row = 0),
    imponendo che dalla colonna 1 in poi siano presenti solo zeri
    nella prima riga.
-}

lowertriangular :: (Num a, Ord a) => Matrice a -> Bool
lowertriangular a =
    lowertriangularSupp a (getMatrixDim a 0) 0 1

{-
Esercizio 6

Si scriva un predicato uppertriangular che determina se una matrice (quadrata) è triangolare superiore.
-}

{-
checkRowZeroUpTo:
    Verifica che, in una riga, tutti gli elementi con indice < limit
    (cioè sotto la diagonale rispetto alla riga corrente)
    siano uguali a 0.

    Parametri:
        - lista della riga
        - pos   = indice corrente di colonna
        - limit = indice della diagonale (tutti gli indici < limit devono essere 0)

    Se pos >= limit -> non ci sono più vincoli.
-}
checkRowZeroUpTo :: (Num a, Ord a) => [a] -> a -> a -> Bool
checkRowZeroUpTo [] _ _ =
    True

checkRowZeroUpTo (x:xs) pos limit
    | pos >= limit =
        -- Superata la zona che deve essere nulla
        True

    | x == 0 =
        -- Elemento sotto la diagonale corretto
        checkRowZeroUpTo xs (pos + 1) limit

    | otherwise =
        -- Trovato un elemento non nullo sotto la diagonale
        False

{-
uppertriangularSupp:
    Verifica ricorsivamente che la matrice sia triangolare superiore.

    Una matrice è triangolare superiore se:
        per ogni riga i,
        tutti gli elementi con indice di colonna j < i sono 0
        (cioè sotto la diagonale principale).

    Parametri:
        - lista delle righe
        - n   = dimensione della matrice
        - row = indice della riga corrente
        - col = parametro non necessario per il controllo (mantenuto per coerenza)

    Per ogni riga:
        - si controlla che gli elementi prima della diagonale siano 0
        - si procede alla riga successiva incrementando row
-}
uppertriangularSupp
    :: (Num a, Ord a)
    => Matrice a -> a -> a -> a -> Bool

uppertriangularSupp [] _ _ _ =
    True

uppertriangularSupp (x:xs) n row col =
    ( row >= (n - 1) )
    ||
    ( checkRowZeroUpTo x 0 row
        && uppertriangularSupp xs n (row + 1) (col + 1)
    )

{-
uppertriangular:
    Predicato principale.

    Calcola la dimensione della matrice e avvia
    il controllo dalla prima riga (row = 0).
-}
uppertriangular :: (Num a, Ord a) => Matrice a -> Bool
uppertriangular a =
    uppertriangularSupp a (getMatrixDim a 0) 0 1

{-
Esercizio 7

Si scriva un predicato diagonal che determina se una matrice (quadrata) è diagonale.
-}

{-
diagonal:
    Una matrice è diagonale se tutti gli elementi fuori dalla diagonale principale
    sono 0, cioè:
        - sotto la diagonale: tutti 0  -> matrice triangolare superiore
        - sopra la diagonale: tutti 0  -> matrice triangolare inferiore

    Per questo basta verificare entrambe le proprietà:
        diagonal A  <=>  lowertriangular A && uppertriangular A

    Nota: si assume che la matrice sia quadrata.
-}

diagonal :: (Num a, Ord a) => Matrice a -> Bool
diagonal a =
    lowertriangular a && uppertriangular a

{-
Esercizio 8

Una matrice quadrata M di ordine n si dice convergente con raggio r se il modulo della somma degli elementi di ogni riga, escluso quello sulla diagonale, è inferiore a r.
Si scriva un predicato convergent m r che determina se una matrice (quadrata) m è convergente con raggio r.
-}

{-
sumRowExceptDiag:
    Calcola la somma dei moduli degli elementi di una riga,
    escludendo l’elemento sulla diagonale principale.

    Parametri:
        - lista della riga
        - pos      = indice corrente di colonna
        - diagonal = indice della colonna corrispondente alla diagonale (riga corrente)

    Per ogni elemento:
        - se è sulla diagonale -> viene ignorato
        - altrimenti si aggiunge il suo valore assoluto
-}
sumRowExceptDiag :: (Num a, Ord a) => [a] -> Int -> Int -> a
sumRowExceptDiag [] _ _ =
    0

sumRowExceptDiag (x:xs) pos diagonal
    | pos == diagonal =
        -- Elemento diagonale: non va considerato
        sumRowExceptDiag xs (pos + 1) diagonal

    | otherwise =
        -- Somma dei moduli degli elementi fuori diagonale
        abs x + sumRowExceptDiag xs (pos + 1) diagonal

{-
checkConvergent:
    Verifica ricorsivamente che ogni riga soddisfi la condizione
    di convergenza con raggio r:

        | somma elementi fuori diagonale | < r

    Poiché si sta già sommando il valore assoluto degli elementi,
    il confronto è semplicemente:
        somma < r

    Parametri:
        - matrice (lista di righe)
        - r   = raggio
        - row = indice della riga corrente (serve per identificare la diagonale)
-}

checkConvergent :: (Num a, Ord a) => Matrice a -> a -> Int -> Bool
checkConvergent [] _ _ =
    True

checkConvergent (x:xs) r row =
    sumRowExceptDiag x 0 row < r
    && checkConvergent xs r (row + 1)

{-
convergent:
    Predicato principale.

    Una matrice quadrata M è convergente con raggio r
    se per ogni riga vale la condizione precedente.
-}
convergent :: (Num a, Ord a) => Matrice a -> a -> Bool
convergent m r =
    checkConvergent m r 0

{-
Esercizio 9

Si scriva una funzione che data una matrice di dimensioni m × n restituisce la corrispondente matrice trasposta (di dimensioni n × m).
-}

{-
trasposta:
    Calcola la matrice trasposta usando foldr.
    L’idea è inserire ogni riga della matrice originale
    dentro una matrice accumulatore che rappresenta
    progressivamente la trasposta.
-}
trasposta :: (Eq a, Num a) => Matrice a -> Matrice a
trasposta =
    foldr traspose []

{-
traspose:
    Inserisce una riga nella matrice accumulata (che rappresenta
    la trasposta costruita finora).

Casi:

1) traspose [] mxs
    Riga vuota -> nessuna modifica.

2) traspose xs []
    Se l’accumulatore è vuoto, significa che stiamo inserendo
    la prima riga della matrice originale.
    Ogni elemento diventa una nuova riga singola:
        [a,b,c] -> [[a],[b],[c]]

3) traspose (x:xs) (mx:mxs)
    x viene aggiunto in testa alla riga mx (colonna corrente),
    poi si procede ricorsivamente con gli elementi restanti.
-}

traspose :: (Eq a, Num a) => [a] -> Matrice a -> Matrice a
traspose [] mxs =
    mxs

traspose xs [] =
    -- Prima riga: ogni elemento diventa una colonna
    map (:[]) xs

traspose (x:xs) (mx:mxs) =
    -- Inserisce x nella colonna corrente
    (x : mx) : traspose xs mxs

{-
Esercizio 10

Si scriva un predicato isSymmetric che, data una matrice quadrata, determina se è simmetrica.
-}

{-
isSymmetric:
    Una matrice quadrata è simmetrica se coincide con la sua trasposta,
    cioè se per ogni i,j vale:

        a_ij = a_ji

    Implementazione:
        - si calcola la trasposta della matrice
        - si confronta con la matrice originale usando (==)

    Si assume che la matrice sia quadrata come richiesto dall’esercizio.
-}

isSymmetric :: (Eq a, Num a) => Matrice a -> Bool
isSymmetric xs =
    xs == trasposta xs

{-
Esercizio 11

Si scriva una funzione che data una matrice di dimensioni n × k ed una k × m restituisca la matrice prodotto corrispondente (di dimensioni n × m).
Si assuma di moltiplicare matrici con dimensioni compatibili e (se facesse comodo) matrici non degeneri.
-}

{-
dotProduct:
    Calcola il prodotto scalare tra due vettori della stessa lunghezza:

        [a1,...,ak] · [b1,...,bk] = Σ ai*bi

    Si usa zipWith (*) per moltiplicare componente per componente
    e sum per sommare i risultati.
-}
dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys =
    sum (zipWith (*) xs ys)

{-
matrixProduct:
    Calcola il prodotto tra:
        m1 di dimensione n × k
        m2 di dimensione k × m
    restituendo una matrice n × m.

    Idea:
        - Il prodotto tra una riga di m1 e una colonna di m2
        è il loro prodotto scalare.
        - Per ottenere facilmente le colonne di m2,
        si usa la sua trasposta.
        - Per ogni riga di m1 si costruisce una nuova riga
        calcolando il prodotto scalare con ogni colonna di m2.

    Comprensione di lista:
        [[ ... | col <- trasposta m2] | row <- m1]

        - ciclo esterno: per ogni riga di m1
        - ciclo interno: per ogni colonna di m2
-}
matrixProduct :: (Num a, Eq a) => Matrice a -> Matrice a -> Matrice a
matrixProduct m1 m2 =
    [ [ dotProduct row col
        | col <- trasposta m2
        ]
        | row <- m1
    ]
