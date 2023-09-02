{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
import Distribution.Simple.Utils (xargs)
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

--Ejercicio 1
data Pila a = P [a]


instance Show a => Show (Pila a) where
  show (P xs) = show xs


instance Eq a => Eq (Pila a) where
  (==) (P xs) (P ys) = xs == ys
  (/=) (P xs) (P ys) = xs /= ys

crearPila :: Pila a
crearPila = P []

esPilaVacia :: Pila a -> Bool
esPilaVacia (P []) = True
esPilaVacia p = False

apilar :: a -> Pila a -> Pila a
apilar x (P xs) = P (x:xs)

cima :: Pila a -> a
cima (P []) = error "La pila esta vacia"
cima (P (x:xs)) = x

desapilar :: Pila a -> Pila a
desapilar (P []) = P []
desapilar (P (x:xs)) = P xs

{-¿Qué hace esta función?-}
r :: [a] -> [a]
r xs = ys where P ys = foldl (\p x -> apilar x p) crearPila xs

{-Va leyendo cada elemento de la lista xs y lo añade al top de la pila P ys,
  la cual posee todos los elementos de xs invertidos. Se devuelve la lista ys    
-}

--Ejercicio 2
mayorQueCumple :: Enum a => (a->Bool)-> a-> a->Maybe a
mayorQueCumple p x y = let xs = [Just x | x <- [x..y], p x] in
    if null xs then Nothing else last xs

--Ejercicio 3
data Cj a = Conjunto [a]


instance Show a => Show (Cj a) where
    show (Conjunto xs) = show xs


instance Eq a => Eq (Cj a) where
  (==) (Conjunto xs) (Conjunto ys) = cContenido xs ys && cContenido ys xs
  (/=) (Conjunto xs) (Conjunto ys) = not (xs == ys)


instance Eq a => Ord (Cj a) where
  compare (Conjunto xs) (Conjunto ys)
    | cContenido xs ys && cContenido ys xs = EQ
    | cContenido xs ys = LT
    | otherwise = GT

cContenido :: Eq a => [a] -> [a] -> Bool
cContenido xs ys = all (\x -> elem x ys) xs

cContenido2 :: Eq a => Cj a -> Cj a -> Bool
cContenido2 (Conjunto xs) (Conjunto ys) = all (\x -> elem x ys) xs

elemCj :: Eq a => Cj a -> a -> Bool
elemCj (Conjunto ys) x = elem x ys

bienDefCj :: Eq a => Cj a -> Cj a
bienDefCj (Conjunto ys) = Conjunto (foldr (\x ys ->  x : remove x ys) ys ys)

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys)
  |x == y = remove x ys
  |otherwise = y : remove x ys

contenido :: Eq a =>  Cj a -> Cj a -> Bool
contenido c1 c2 = c1 <= c2

--Ejercicio 4
data Temp = Kelvin Float | Celsius Float | Fahrenheit Float


instance Show Temp where
  show (Kelvin f) = show f ++ " K"
  show (Celsius f) = show f ++ " °C"
  show (Fahrenheit f) = show f ++ " F"


instance Eq Temp where
  (==) t1 t2 = valor(aKelvin t1) == valor(aKelvin t2)


instance Ord Temp where
  compare t1 t2
    | valor(aKelvin t1) < valor(aKelvin t2) = LT
    | valor(aKelvin t1) == valor(aKelvin t2) = EQ
    | otherwise = GT

valor :: Temp -> Float
valor (Kelvin f) = f
valor (Celsius f) = f
valor (Fahrenheit f) = f

escalaTemp :: Temp -> String
escalaTemp (Kelvin f) = "Kelvin"
escalaTemp (Celsius f) = "Celsius"
escalaTemp (Fahrenheit f) = "Fahrenheit"

aKelvin :: Temp -> Temp
aKelvin (Kelvin f) = Kelvin f
aKelvin (Celsius f) = Kelvin (f + 273.15)
aKelvin (Fahrenheit f) = Kelvin ((f - 32) * 5 / 9 + 273.15)

aCelsius :: Temp -> Temp
aCelsius (Kelvin f) = Celsius (f - 273.15)
aCelsius (Celsius f) = Celsius f
aCelsius (Fahrenheit f) = Celsius ((f - 32) * 5 / 9)

aFahrenheit :: Temp -> Temp
aFahrenheit (Kelvin f) = Fahrenheit ((f - 273.15) * 9 / 5 + 32)
aFahrenheit (Celsius f) = Fahrenheit (f * 9 / 5 + 32)
aFahrenheit (Fahrenheit f) = Fahrenheit f

--Ejercicio 5
data BinTree a = Hoja | Nodo a (BinTree a) (BinTree a) deriving Show

treeSort :: Ord a => [a] -> [a]
treeSort xs = inOrder (foldl insertar Hoja xs)

insertar :: Ord a => BinTree a -> a -> BinTree a
insertar Hoja x = Nodo x Hoja Hoja
insertar (Nodo e i d) x
  | e == x = Nodo e i d
  | x > e = Nodo e i (insertar d x)
  | otherwise = Nodo e (insertar i x) d

inOrder :: BinTree a -> [a]
inOrder Hoja = []
inOrder (Nodo e i d) = inOrder i ++ [e] ++ inOrder d

--Ejercicio 5 con AVL
--Arbol de busqueda AVL

data AVL a = L| N a Int (AVL a) (AVL a) deriving Show

treeSort2 :: Ord a => [a] -> [a]
treeSort2 xs = inOrder2 (foldl insertA L xs)

insertA :: Ord a => AVL a -> a -> AVL a
insertA L x = N x 1 L L
insertA (N e h l r)  x
  | e == x = N e h l r
  | x > e = let nR = insertA r x in equil(N e (max (getH l) (getH nR) + 1) l nR)
  | otherwise = let nL = insertA l x in equil(N e (max (getH nL) (getH r) + 1) nL r)

equil :: Ord a => AVL a -> AVL a
equil L = L
equil (N e h l r)
  | abs(getH l - getH r) <= 1 = N e h l r
  | getH l > getH r && getH lL >= getH rL = rotLL (N e h l r)
  | getH l > getH r = rotLR (N e h l r)
  | getH l < getH r && getH lR >= getH rR = rotRL (N e h l r)
  | otherwise = rotRR (N e h l r)
    where (N eL hL lL rL) = l
          (N eR hR lR rR) = r

rotLL :: AVL a -> AVL a
rotLL (N e h (N e1 h1 l1 r1) r) = N e1 altura2 l1 (N e altura1 r1 r)
  where altura1 = max (getH r1) (getH r) + 1
        altura2 = max (getH l1) altura1 + 1
rotLL _ = error "No se puede aplicar rotLL"

rotLR :: AVL a -> AVL a
rotLR (N e h (N e1 h1 l1 (N e2 h2 l2 r2)) r) = N e2 altura3 (N e1 altura1 l1 l2) (N e altura2 r2 r)
  where altura1 = max (getH l1) (getH l2) + 1
        altura2 = max (getH r2) (getH r) + 1
        altura3 = max altura1 altura2 + 1
rotLR _ = error "No se puede aplicar rotLR"

rotRR :: AVL a -> AVL a
rotRR (N e h l (N e1 h1 l1 r1)) = N e1 altura2 (N e altura1 l l1) r1
  where altura1 = max (getH l) (getH l1) + 1
        altura2 = max altura1 (getH r1) + 1
rotRR _ = error "No se puede aplicar rotRR"

rotRL :: AVL a -> AVL a
rotRL (N e h l (N e1 h1 (N e2 h2 l2 r2) r1)) = N e2 altura3 (N e altura1 l l2) (N e1 altura2 r2 r1)
  where altura1 = max (getH l) (getH l2) + 1
        altura2 = max (getH r2) (getH r1) + 1
        altura3 = max altura1 altura2 + 1
rotRL _ = error "No se puede aplicar rotRL"

getH :: AVL a -> Int
getH L = 0
getH (N e h l r) = h

inOrder2 :: AVL a -> [a]
inOrder2 L = []
inOrder2 (N e h l r) = inOrder2 l ++ (e:inOrder2 r)

lineaALista :: [Char] -> [Int]
lineaALista = foldr (\y ts -> if y == '\n' || y == ' ' then ts else (read [y]::Int):ts) []

leer :: IO()
leer = do putStr "Escribe un conjunto de numero (Cada numero debe estar separado por un espacio):\n"
          xs <- getLine
          putStr "Escribe un conjunto de numero (Cada numero debe estar separado por un espacio):\n"
          xs2 <- getLine
          let z = lineaALista xs
          let c = bienDefCj (Conjunto z)
          let z2 = lineaALista xs2
          let c2 = bienDefCj (Conjunto z2)
          print (contenido c c2)

-- Tipo de dato que sea una pila de numeros enteros

