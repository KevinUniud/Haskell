module Numeri
    ( fact
    , binom
    , allComb
    ) where

{-
1. NUMERI

Si ricordi che si dispone di varie funzioni aritmetiche polimorfe nel Prelude, come(+) , (*) :: ( Num a ) = > a -> a -> a
( div ) :: ( Integral a ) = > a -> a -> a
e quindi si cerchi di scrivere i programmi nel modo più generico possibile in modo da poter usare l’aritmetica a precisione illimitata.
-}

{-
Esercizio 1

Si scriva la funzione fattoriale.
Si verifichi il funzionamento calcolando 10000!.
-}

{-
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

fac n =
    if n <= 1 then
        1
    else
        n * fac (n-1)

fac n
    | n <= 1    = 1
    | otherwise = n * fac (n-1)

fac n = aux n 1
    where
        aux n acc
            | n <= 1    = acc
            | otherwise = aux (n-1) (n*acc)
-}

-- La funzione è polimorfa su qualsiasi tipo appartenente alla classe Integral
-- (ad esempio Int o Integer).
fact :: (Integral a) => a -> a

-- Caso di errore: il fattoriale non è definito per numeri negativi.
fact n
    | n < 0     = error "Argomento negativo"

    -- Caso normale: per n >= 0 si calcola il prodotto di tutti i numeri
    -- da 1 a n usando la funzione 'product' del Prelude.
    -- product [1..n] moltiplica tutti gli elementi della lista [1,2,...,n].
    -- Se n == 0, la lista è vuota e product [] restituisce 1,
    -- che è coerente con la definizione 0! = 1.
    | otherwise = product [1..n]

{-
Esercizio 2

Si scriva la funzione \binom{n}{k}, combinazioni di k elementi su n.
-}

{-
binomiale :: Int -> Int -> Int
binomiale n k =
    if k > n
        then -1
        else fact n `div` (fact (n - k) * fact k)
-}

{-
binom :: (Integral a) => a -> a -> a
binom n k
    | k < 0 || k > n  = 0
    | otherwise       = fact n `div` (fact k * fact (n - k))
-}

-- binom calcola il coefficiente binomiale (n su k)
-- È polimorfa su qualsiasi tipo appartenente alla classe Integral
-- (ad esempio Int o Integer).
binom :: (Integral a) => a -> a -> a

-- Se k è negativo oppure maggiore di n, il coefficiente binomiale vale 0.
binom n k
    | k < 0 || k > n = 0

    -- Casi base: scegliere 0 elementi oppure tutti gli n elementi
    -- dà sempre risultato 1.
    | k == 0 || k == n = 1

    -- Caso generale.
    -- Si sfrutta la simmetria: C(n, k) = C(n, n-k).
    -- In questo modo si riduce il numero di moltiplicazioni,
    -- scegliendo il valore più piccolo tra k e (n-k).
    | otherwise =
        let k' = min k (n - k)
        -- Si calcola:
        --   C(n, k) = (n-k'+1 * ... * n) / (1 * ... * k')
        -- evitando di calcolare tre fattoriali completi.
        -- Questo è più efficiente e riduce il rischio di overflow
        -- (soprattutto usando tipi a precisione arbitraria come Integer).
        in product [n - k' + 1 .. n] `div` product [1 .. k']

{-
Esercizio 3

Si scriva una funzione che calcoli una lista con tutte le combinazioni su n elementi.
Si usi opportunamente
map :: ( a -> b ) -> [ a ] -> [ b ]
-}

-- allComb n restituisce la lista di tutti i coefficienti binomiali
-- C(n, k) per k che varia da 0 a n.
-- Il risultato corrisponde alla riga n-esima del triangolo di Pascal.
allComb :: (Integral a) => a -> [a]

allComb n =
    -- [0..n] genera la lista degli indici k.
    -- map (binom n) applica la funzione (binom n) a ciascun k,
    -- producendo [C(n,0), C(n,1), ..., C(n,n)].
    map (binom n) [0..n]