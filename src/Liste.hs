module Liste
    ( removePairs
    , sumOdds
    , quickSort
    , minOdd
    , fun5
    , fun6
    , shiftToZero
    ) where

{-
2. LISTE

Si ricordi che si dispone di varie funzioni del Prelude, come
foldr :: (a - >b - > b ) -> b -> [ a ] -> b
che accumula, a partire da un opportuno elemento neutro, tutti gli elementi di una lista applicando un operatore binario da destra a sinistra
foldr f z [ x1 , x2 ,..., xn ] = ( x1 ‘f ‘ ( x2 ‘f ‘ ... ( xn ‘f ‘ z )...))
-}

{-
Esercizio 1

Scrivere una funzione che data una lista ne costruisce una rimuovendo gli elementi di posizione pari (si conti partendo da 1).
-}

-- removePairsSupp è una funzione ausiliaria che riceve:
--   1) la lista da elaborare
--   2) un contatore k che rappresenta la posizione corrente (a partire da 1)
removePairsSupp :: Integral a => [a] -> a -> [a]

-- Caso base: lista vuota -> restituisce lista vuota.
removePairsSupp [] _ = []

removePairsSupp (x:xs) k =
    -- Se la posizione k è pari, l'elemento x viene scartato
    -- e si prosegue con il resto della lista incrementando k.
    if even k
    then removePairsSupp xs (k + 1)
    else
        -- Se la posizione è dispari, x viene mantenuto
        -- e aggiunto alla lista risultante.
        x : removePairsSupp xs (k + 1)

-- Funzione principale: avvia la funzione ausiliaria
-- con contatore iniziale pari a 1.
removePairs :: Integral a => [a] -> [a]
removePairs xs = removePairsSupp xs 1

{-
Esercizio 2

Scrivere una funzione che calcola la somma degli elementi di posizione dispari di una lista.
-}

-- sumOddsSupp è una funzione ausiliaria che riceve:
--   1) la lista da elaborare
--   2) un contatore k che rappresenta la posizione corrente (a partire da 1)
sumOddsSupp :: Integral a => [a] -> a -> a

-- Caso base: lista vuota -> la somma è 0.
sumOddsSupp [] _ = 0

sumOddsSupp (x:xs) k =
    -- Se la posizione k è pari, l'elemento x non viene sommato
    -- e si prosegue con il resto della lista incrementando k.
    if even k
    then sumOddsSupp xs (k + 1)
    else
        -- Se la posizione è dispari, x viene aggiunto alla somma
        -- e si continua ricorsivamente.
        x + sumOddsSupp xs (k + 1)

-- Funzione principale: avvia la funzione ausiliaria
-- con contatore iniziale pari a 1.
sumOdds :: Integral a => [a] -> a
sumOdds xs = sumOddsSupp xs 1

{-
Esercizio 3

Scrivere il QuickSort (polimorfo).
-}

-- quickSort implementa l’algoritmo QuickSort in modo polimorfo.
-- Funziona per qualsiasi tipo che appartiene alla classe Ord
-- (cioè per cui è definito un ordinamento).
quickSort :: Ord a => [a] -> [a]

-- Caso base: la lista vuota è già ordinata.
quickSort [] = []

-- Caso ricorsivo:
-- Si sceglie il primo elemento x come pivot.
quickSort (x:xs) =
    -- Si costruiscono due sottoliste tramite list comprehension:
    -- 1) tutti gli elementi minori del pivot
    quickSort [y | y <- xs, y < x]
    -- 2) il pivot seguito dagli elementi maggiori o uguali
    ++ (x : quickSort [y | y <- xs, x <= y])

-- L’idea è:
-- - dividere la lista rispetto al pivot
-- - ordinare ricorsivamente le due parti
-- - concatenare: (minori ordinati) ++ [pivot] ++ (maggiori ordinati)
--
-- La correttezza deriva dal fatto che:
-- - tutti gli elementi della prima parte sono < x
-- - tutti quelli della seconda parte sono >= x
-- - la ricorsione termina perché le sottoliste sono strettamente più piccole.

{-
Esercizio 4

Scrivere una funzione che calcola i 2 minori elementi dispari di una lista (se esistono).
Ad esempio minOdd([2,3,4,6,8,7,5]) riduce a (3,5)
-}

-- minOdd restituisce i due minori elementi dispari di una lista (se esistono).
-- 1) filter odd xs seleziona solo gli elementi dispari.
-- 2) quickSort ordina la lista risultante in ordine crescente.
-- 3) take 2 prende i primi due elementi (i più piccoli).
--
-- Se gli elementi dispari sono meno di due:
-- - restituisce lista vuota se non ce ne sono
-- - restituisce lista con un solo elemento se ce n’è uno solo
minOdd :: Integral a => [a] -> [a]
minOdd xs = take 2 (quickSort (filter odd xs))

{-
Esercizio 5

Scrivere una funzione che costruisce, a partire da una lista di numeri interi, una lista di coppie in cui
(a) il primo elemento di ogni coppia è uguale all’elemento di corrispondente posizione nella lista originale e
(b) il secondo elemento di ogni coppia è uguale alla somma di tutti gli elementi conseguenti della lista originale.
-}

-- fun5 costruisce una lista di coppie (a, b) tale che:
--   a è l’elemento corrente della lista originale
--   b è la somma di tutti gli elementi successivi
--
-- Esempio:
-- [1,2,3] -> [(1,5),(2,3),(3,0)]
fun5 :: Num a => [a] -> [(a,a)]

-- Caso base: lista vuota -> nessuna coppia.
fun5 [] = []

fun5 (x:xs) =
    -- x è il primo elemento della lista
    -- sum xs calcola la somma di tutti gli elementi successivi
    -- Si costruisce la coppia (x, sommaRestanti)
    -- e si procede ricorsivamente sul resto della lista.
    (x, sum xs) : fun5 xs

{-
Esercizio 6

Scrivere una funzione che costruisce, a partire da una lista di numeri interi (provate poi a generalizzare), una lista di coppie in cui
(a) il primo elemento di ogni coppia è uguale all’elemento di corrispondente posizione nella lista originale e
(b) il secondo elemento di ogni coppia è uguale alla somma di tutti gli elementi antecedenti della lista originale.

Farlo con foldr o foldl è difficile.
-}

-- fun6 costruisce una lista di coppie (a, b) tale che:
--   a è l’elemento corrente della lista originale
--   b è la somma di tutti gli elementi precedenti
--
-- Esempio:
-- [1,2,3,4] -> [(1,0),(2,1),(3,3),(4,6)]
--
-- Si utilizza un accumulatore che tiene traccia
-- della somma degli elementi già visitati.

fun6 :: Num a => [a] -> [(a,a)]
fun6 xs = fun6Supp xs 0
    where
        -- Caso base: lista vuota.
        fun6Supp [] _ = []

        -- x è l’elemento corrente
        -- acc è la somma di tutti gli elementi precedenti
        fun6Supp (x:xs) acc =
            -- La coppia è (x, acc)
            -- Per il passo successivo, il nuovo accumulatore diventa acc + x
            (x, acc) : fun6Supp xs (acc + x)

{-
Esercizio 7

Si scriva una funzione Haskell shiftToZero che data una lista costruisce un nuova lista che contiene gli elementi diminuiti del valore minimo.
A titolo di esempio, shiftToZero [5,4,2,6] =⇒ [3,2,0,4].
La funzione non deve visitare gli elementi della lista più di una volta (si sfrutti la laziness).
-}

-- minimum xs calcola il valore minimo.
-- Grazie alla laziness di Haskell, minVal viene condiviso:
-- la lista non viene riesaminata esplicitamente nel codice,
-- ma il valore minimo è calcolato una sola volta e riutilizzato.
shiftToZero :: (Ord a, Num a) => [a] -> [a]
shiftToZero xs = map (\x -> x - minVal) xs
    where
        minVal = minimum xs
