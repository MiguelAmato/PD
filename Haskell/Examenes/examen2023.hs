{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data Ind = Cero | Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete deriving (Show, Eq, Enum)

data Hash a = H [(Ind, [a])]

natToInd :: Int -> Ind
natToInd x = [Cero .. Siete] !! x -- 0,25

hashWord :: String -> Ind
hashWord x = natToInd $ mod (length x) 8 -- Puse modulo 7, 0,15? maybe

indToNat ::  Ind -> Int
indToNat Cero = 0
indToNat Uno = 1
indToNat Dos = 2
indToNat Tres = 3
indToNat Cuatro = 4
indToNat Cinco = 5
indToNat Seis = 6
indToNat Siete = 7

agregaPalabra :: String -> [String] -> [String] 
agregaPalabra p = foldl (\acc x -> if p < x then acc++[p]++[x] else acc++[x]) [] -- -0.1 esta mal ordenado

insertarHash :: String -> Hash a -> Hash a
insertarHash p (H xs) =  (H nuevaLista)
    where nuevaLista = foldl (\acc (ind, e) -> foldl ) [] xs
            (IndAg, ListaAg) = 