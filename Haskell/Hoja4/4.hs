{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use infix" #-}

-- Ejercicio 1

data Pila a = P [a]

instance Show a => Show (Pila a) where 
    show (P xs) = show xs

instance Eq a => Eq (Pila a) where 
    (==) (P xs) (P ys) = xs == ys 
    (/=) (P xs) (P ys) = xs /= ys 

creaPila :: Pila a
creaPila = P []

esPilaVacia :: Pila a -> Bool
esPilaVacia (P xs) = null xs

apilar :: a -> Pila a -> Pila a
apilar x (P xs) = P (x:xs)

cima :: Pila a -> a 
cima (P []) = error "La pila esta muito vacia"
cima (P (x:xs)) = x

desapilar :: Pila a -> Pila a
desapilar (P []) = P []
desapilar (P (x:xs)) = P xs

-- es un reverse de toa la vida

-- Ejercicio 2

caritaTriste :: [a] -> Maybe a
caritaTriste [] = Nothing
caritaTriste xs = Just (last xs)

mayorQueCumple :: Enum a => (a->Bool) -> a -> a -> Maybe a
mayorQueCumple p n m = caritaTriste [x | x <- [n..m], p x] 

-- Ejercicio 3 

data Cj a = C [a]

estaContenido :: Eq a => Cj a -> Cj a -> Bool
estaContenido (C xs) (C ys) = foldl (\x y -> x && elem y ys) True xs 

instance Show a => Show (Cj a) where 
    show (C xs) = show xs

instance Eq a => Eq (Cj a) where 
    (==) (C xs) (C ys) = estaContenido (C xs) (C ys) && estaContenido (C ys) (C xs)
    (/=) (C xs) (C ys) = not ((==) (C xs) (C ys))

instance Eq a => Ord (Cj a) where
    compare (C xs) (C ys)
        | estaContenido (C xs) (C ys) && estaContenido (C ys) (C xs) = EQ
        | estaContenido (C xs) (C ys) = LT
        | otherwise = GT


elemCj :: Eq a => a -> Cj a -> Bool
elemCj x (C xs) = elem x xs

bienDefCj :: Eq a => Cj a -> Cj a -- n^3 si, Iker me gano
bienDefCj (C xs) = C (foldl (\x y -> if notElem y x then x++[y] else x) [] xs)

contenido :: Eq a => Cj a -> Cj a -> Bool
contenido (C xs) (C ys) = bienDefCj (C xs) <= bienDefCj (C ys)
        
-- Ejercicio 4

data Temp = Kn Float | Cs Float | Ft Float 

iker :: Temp -> Temp
iker (Cs c) = Cs c
iker (Ft f) = Cs ((f - 32) * 5 / 9 )
iker (Kn k) = Cs (k - 273.15)

cuenca :: Temp -> Float
cuenca (Cs c) = c
cuenca (Ft f) = (f - 32) * 5 / 9 
cuenca (Kn k) = k - 273.15

instance Show Temp where
  show (Kn f) = show f ++ " K"
  show (Cs f) = show f ++ " °C"
  show (Ft f) = show f ++ " F"

instance Eq Temp where 
    (==) t1 t2 = cuenca t1 == cuenca t2
    (/=) t1 t2 = not (t1 == t2)

instance Ord Temp where 
    compare t1 t2
        | t1 == t2 = EQ
        | cuenca t1 < cuenca t2 = LT
        | otherwise = GT

-- Tobias esto no es borma, es parte del ejercicio
escala :: Temp -> [Char]
escala (Cs c) = "Cexo"
escala (Kn c) = "Kevin"
escala (Ft f) = "Feature"

aCelsius :: Temp -> Temp
aCelsius (Cs c) = Cs c
aCelsius (Kn k) = Cs (k - 273.15)
aCelsius (Ft f) = Cs ((f - 32) * 5 / 9)

aKelvin :: Temp -> Temp
aKelvin (Kn c) = Kn c
aKelvin (Cs c) = Kn (c + 273.15)
aKelvin (Ft f) = Kn ((f - 32) * 5 / 9 + 273.15)

aFarenheit :: Temp -> Temp
aFarenheit (Ft f) = Ft f
aFarenheit (Cs c) = Ft ((c * 9 / 5) + 32)
aFarenheit (Kn k) = Ft ((k - 273.15) * 9 / 5 + 32 )

-- Ejercicio 5

data BinTree a = N a (BinTree a) (BinTree a) | H deriving Show

insertElemBT :: Ord a => a -> BinTree a -> BinTree a
insertElemBT x H = N x H H
insertElemBT x (N e l r) 
    | x < e = N e (insertElemBT x l) r 
    | x > e = N e l (insertElemBT x r)
    | otherwise = N e l r

treeSort :: Ord a => [a] -> [a] 
treeSort xs = inOrder(foldl (flip insertElemBT) H xs) 

inOrder :: BinTree a -> [a]
inOrder H = []
inOrder (N e l r) = inOrder l ++ [e] ++ inOrder r

