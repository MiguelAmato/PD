-- Practica Final 
-- Hecha por: Miguel Antonio Amato Hermo

-- Notas de la practica:
-- Cada parte de la practica se encuentra separada por un comentario que indica el numero de la parte.
-- Cada apartado de la parte 1 esta separada con guiones.
-- Al final de cada apartado habran comentarios (en bloque) con algunas pruebas que se pueden hacer con el menu (Las pruebas estan enumeradas y tienen lo que se supone que tendria que devolver)
-- Tambien he dejado al final de cada apartado una funcion auxiliar IO() que al ejecutarla se pueden probar todas a la vez 

-- ======================================================================================== PARTE 1 ========================================================================================

-- Define un tipo de dato Prop para los simbolos de las proposiciones, que sea instancia de la clase Show, Read y Eq (usando Deriving)

data Prop = P | Q | R | S deriving (Show, Read, Eq)

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define un tipo de dato Formula (recursivo) para representar las formulas permitidas. Tiene que ser instancia de Read y Eq (usando deriving) e instancia de Show 
-- de manera que se muestre la formula de una forma lo mas parecida a la notacion habitual

data Formula = Atomo Prop | Negacion Formula | Conjuncion Formula Formula | Disyuncion Formula Formula deriving (Read, Eq)

instance Show Formula where
    show (Atomo p) = show p
    show (Negacion (Atomo p)) = "¬" ++ show p
    show (Negacion f) = "¬" ++ "(" ++ show f ++ ")"
    show (Conjuncion f1 f2) = "(" ++ show f1 ++ " ^ " ++ show f2 ++ ")"
    show (Disyuncion f1 f2) = "(" ++ show f1 ++ " V " ++ show f2 ++ ")"

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define una funcion esClausula que dada una expresion de tipo Formula compruebe si se trata de
-- una clausula o no.

esLiteral :: Formula -> Bool -- Funcion auxiliar que se usara en esClausula 
esLiteral (Atomo p) = True
esLiteral (Negacion (Atomo p)) = True
esLiteral _ = False

esClausula :: Formula -> Bool
esClausula (Atomo p) = True
esClausula (Negacion (Atomo p)) = True
esClausula (Disyuncion f1 f2) = esClausula f1 && esLiteral f2 -- Las clausulas al ser disyunciones de literales y al asociar por la izquierda, se comprueba que f1 sea clausula y f2 literal
esClausula _ = False

{-
  Pruebas en el menu:
  1- (Atomo P) -> True
  2- Negacion (Atomo P) -> True
  3- Disyuncion (Negacion (Atomo P)) (Negacion (Atomo Q)) -> True
  4- Negacion (Disyuncion (Atomo P) (Atomo Q)) -> False
  5- Conjuncion (Negacion (Atomo P)) (Negacion (Atomo Q)) -> False
-}

pruebaEsClausula :: IO() -- Si se quieren probar todas a la vez
pruebaEsClausula = do
  print (esClausula (Atomo P)) -- True
  print (esClausula (Negacion (Atomo P))) -- True
  print (esClausula (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo Q)))) -- True
  print (esClausula (Negacion (Disyuncion (Atomo P) (Atomo Q)))) -- False
  print (esClausula (Conjuncion (Negacion (Atomo P)) (Negacion (Atomo Q)))) -- False

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define una funcion fncAlista que dada una expresion f de tipo Formula, que esta en FNC, devuelve
-- una lista cuyos elementos son las clausulas que son las componentes de la conjuncion f.

fncAlista :: Formula -> [Formula]
fncAlista (Atomo p) = [Atomo p]
fncAlista (Negacion (Atomo p)) = [Negacion (Atomo p)]
fncAlista (Disyuncion f1 f2) = if esClausula (Disyuncion f1 f2) then [Disyuncion f1 f2] else error "No es una clausula" 
fncAlista (Conjuncion f1 f2) = fncAlista f1 ++ fncAlista f2 -- Si es conjuncion se hace llamada de la funcion a las dos formulas para ver si hay clausulas
fncAlista _ = []

{-
  Pruebas en el menu:
  1- (Atomo P) -> [P]
  2- Negacion (Atomo P) -> [¬P]
  3- Disyuncion (Atomo P) (Atomo Q) -> [(P V Q)]
  4- Conjuncion (Disyuncion (Atomo P) (Atomo Q)) (Disyuncion (Negacion (Atomo P)) (Atomo R)) -> [(P V Q),(¬P V R)]
  5- Disyuncion (Atomo P) (Disyuncion (Atomo R) (Atomo S)) -> ERROR
-}

pruebasFncAlista :: IO() -- Si se quieren probar todas a la vez
pruebasFncAlista = do 
  print (fncAlista (Atomo P)) -- [P]
  print (fncAlista (Negacion (Atomo P))) -- [¬P]
  print (fncAlista (Disyuncion (Atomo P) (Atomo Q))) -- [(P V Q)]
  print (fncAlista (Conjuncion (Disyuncion (Atomo P) (Atomo Q)) (Disyuncion (Negacion (Atomo P)) (Atomo R)))) -- [(P V Q),(¬P V R)]
  print (fncAlista (Disyuncion (Atomo P) (Disyuncion (Atomo R) (Atomo S)))) -- ERROR

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define una funcion clausulaLista que dada una expresion c de tipo Formula que es clausula,
-- devuelva la lista de literales que forman la clausula.

clausulaLista :: Formula -> [Formula] -- Se asume que siempre se le pasa una clausula
clausulaLista (Atomo p) = [Atomo p] -- Si es literal se agrega a la lista
clausulaLista (Negacion (Atomo p)) = [Negacion (Atomo p)] 
clausulaLista (Disyuncion f1 f2) = clausulaLista f1 ++ clausulaLista f2 -- Si es disyuncion se hace llamada de la funcion a las dos formulas para ver si hay literales
clausulaLista _ = []

{-
  Pruebas en el menu:
  1- (Atomo P) -> [P]
  2- Negacion (Atomo P) -> [¬P]
  3- Disyuncion (Atomo P) (Atomo Q) -> [P,Q]
  4- Disyuncion (Atomo P) (Disyuncion (Atomo R) (Atomo S)) -> [P,R,S]
  5- Conjuncion (Disyuncion (Atomo P) (Atomo Q)) (Disyuncion (Negacion (Atomo P)) (Atomo R)) -> []
-}

pruebaClausulaLista :: IO()
pruebaClausulaLista = do
  print (clausulaLista (Atomo P)) -- [P]
  print (clausulaLista (Negacion (Atomo P))) -- [¬P]
  print (clausulaLista (Disyuncion (Atomo P) (Atomo Q))) -- [P,Q]
  print (clausulaLista (Disyuncion (Atomo P) (Disyuncion (Atomo R) (Atomo S)))) -- [P,R,S]
  print (clausulaLista (Conjuncion (Disyuncion (Atomo P) (Atomo Q)) (Disyuncion (Negacion (Atomo P)) (Atomo R)))) -- []

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define una funcion esClausulaHorn que dada una expresion de tipo Formula que es clausula, compruebe si se trata de una clausula de Horn o no.

esPositivo :: Formula -> Integer -- Funcion auxiliar que se usara en esClausulaHorn para contar el numero de literales positivos que hay en una clausula 
esPositivo (Atomo p) = 1
esPositivo (Negacion (Atomo p)) = 0
esPositivo _ = 0

esClausulaHorn :: Formula -> Bool
esClausulaHorn (Atomo p) = True 
esClausulaHorn (Negacion (Atomo p)) = True
esClausulaHorn (Disyuncion f1 f2) = sum ([esPositivo x | x <- clausulaLista (Disyuncion f1 f2)]) <= 1 -- Se comprueba que el numero de literales positivos en la clausula sea menor o igual a 1
esClausulaHorn _ = False

{-
  Pruebas en el menu:
  1- (Atomo P) -> True
  2- Negacion (Atomo P) -> True
  3- Disyuncion (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo R))) (Negacion (Atomo Q)) -> True
  4- Disyuncion (Disyuncion (Atomo P) (Negacion (Atomo R))) (Disyuncion (Negacion (Atomo Q)) (Negacion (Atomo S))) -> True
  5- Disyuncion (Disyuncion (Disyuncion (Atomo Q) (Negacion (Atomo S))) (Atomo R)) (Negacion (Atomo P)) -> False
-}

pruebasClausulaHorn :: IO()
pruebasClausulaHorn = do 
  print (esClausulaHorn (Atomo P)) -- True
  print (esClausulaHorn (Negacion (Atomo P))) -- True
  print (esClausulaHorn (Disyuncion (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo R))) (Negacion (Atomo Q)))) -- True
  print (esClausulaHorn (Disyuncion (Disyuncion (Atomo P) (Negacion (Atomo R))) (Disyuncion (Negacion (Atomo Q)) (Negacion (Atomo S))))) -- True
  print (esClausulaHorn (Disyuncion (Disyuncion (Disyuncion (Atomo Q) (Negacion (Atomo S))) (Atomo R)) (Negacion (Atomo P)))) -- False

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Define una funcion resolvente xs1 xs2 que dadas dos expresiones xs1 y xs2, que son listas de literales asociadas a clausulas que se pueden resolver, 
-- devuelva la lista de literales asociada al resolvente de dichas clausulas.

esNegacion :: Formula -> Formula -> Bool -- Funcion auxiliar que usara la resolvente para filtrar
esNegacion (Negacion (Atomo p1)) (Atomo p2) = p1 == p2
esNegacion (Atomo p1) (Negacion (Atomo p2)) = p1 == p2
esNegacion _ _ = False

auxResolvente :: Formula -> [Formula] -> [Formula] -- Esta funcion auxiliar lo que dado un literal se compara con una clausula en forma de lista de literales y elimina todos los negados que encuentre del mismo
auxResolvente e = filter (not . esNegacion e)

resolvente :: [Formula] -> [Formula] -> [Formula] -- Eliminas para cada literal de la primera clausula los negados de la segunda clausula y viceversa, luego haces un control de repetidos (El primer foldl)
resolvente xs1 xs2 = foldl (\x y -> if notElem y x then x++[y] else x) [] (foldl (flip auxResolvente) xs2 xs1 ++ foldl (flip auxResolvente) xs1 xs2)

{-
  Pruebas en el menu:
  1-  Negacion (Atomo P)
      (Atomo P)
      -------------------
      []
  2-  Disyuncion (Disyuncion (Atomo R) (Atomo S)) (Atomo P)
      Disyuncion (Negacion (Atomo P)) (Atomo R)
      -------------------
      [R,S]
  3-  Disyuncion (Disyuncion (Atomo P) (Atomo Q)) (Atomo P)
      Disyuncion (Negacion (Atomo P)) (Atomo Q)
      -------------------
      [Q] * El ¬P de la segunda clausula elimina las dos P de la primera clausula
  4-  Disyuncion (Disyuncion (Atomo P) (Atomo Q)) (Atomo P)
      Disyuncion (Disyuncion (Negacion (Atomo P)) (Atomo Q)) (Atomo P)
      -------------------
      [Q,P] * El ¬P elimina las dos P de la primera clausula y la P de la segunda no es eliminada porque en la primera no hay ninguna ¬P
  5-  Disyuncion (Disyuncion (Disyuncion (Negacion (Atomo P)) (Atomo Q)) (Atomo S)) (Negacion (Atomo R))
      Disyuncion (Disyuncion (Disyuncion (Atomo P) (Atomo Q)) (Negacion (Atomo S))) (Atomo R)
      -------------------
      [Q]
-}

pruebasResolvente :: IO()
pruebasResolvente = do
  print (resolvente [Negacion (Atomo P)] [Atomo P]) -- []
  print (resolvente [Atomo R, Atomo S, Atomo P] [Negacion (Atomo P), Atomo R]) -- [R,S]
  print (resolvente [Atomo P, Atomo Q, Atomo P] [Negacion (Atomo P), Atomo Q]) -- [Q]
  print (resolvente [Atomo P, Atomo Q, Atomo P] [Negacion (Atomo P), Atomo Q, Atomo P]) -- [Q,P]
  print (resolvente [Negacion (Atomo P), Atomo Q, Atomo S, Negacion (Atomo R)] [Atomo P, Atomo Q, Negacion (Atomo S), Atomo R]) -- [Q]

-- ======================================================================================== PARTE 2 ========================================================================================

resolucion :: [[Formula]] -> [Formula] -> [Formula]
resolucion xs x = resolucionAux xs x [] -- Se llama a esta funcion auxiliar para tener un control de repetidos

negado :: Formula -> Formula -- Funcion que devuelve el negado de un atomo
negado (Atomo p) = Negacion (Atomo p)
negado (Negacion (Atomo p)) = Atomo p
negado x = Negacion x

esResolvente :: [Formula] -> [Formula] -> Bool -- Funcion que comprueba si dos clausulas se pueden resolver, nos servira para encontrar la clausula
esResolvente xs ys = foldl (\x y -> x || elem (negado y) ys) False xs

encuentraClausula :: [Formula] -> [[Formula]] -> [Formula] -- Funcion que encuentra la clausula que se puede resolver
encuentraClausula g [] = g -- Si no has encontrado ninguno devuelves g para que termine el algoritmo
encuentraClausula g (x:xs) = if esResolvente g x then resolvente g x else encuentraClausula g xs

resolucionAux :: [[Formula]] -> [Formula] -> [[Formula]] -> [Formula]
resolucionAux xs x rep
  | null x = [] -- Si la clausula es vacia, se devuelve vacio y termina el algoritmo
  | null nuevoG = nuevoG -- Si el resolvente es vacio, devuelves ese resolvente.
  | elem nuevoG rep = x -- Si el resolvente ya esta en la lista de repetidos, devuelves el G sin resolver y terminas el algoritmo. Si no encontraste ninguno devuelves el G sin resolver
  | otherwise = resolucionAux xs nuevoG (rep ++ [nuevoG]) -- En caso contrario, regresas al paso 1 con el nuevo resolvente y la lista de repetidos actualizada
  where nuevoG = encuentraClausula x xs

{-
  Pruebas en el menu:
  1-  [Atomo P]
      Negacion (Atomo P)
      -------------------
      [] * El algoritmo para porque la resolvente encontrada es []
  2-  [Disyuncion (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo Q))) (Atomo S), Disyuncion (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo S))) (Atomo Q)]
      Disyuncion (Negacion (Atomo Q)) (Negacion (Atomo S))
      -------------------
      [¬P, ¬S] * El algoritmo para por control de repetidos
  3-  [Disyuncion (Disyuncion (Negacion (Atomo P)) (Negacion (Atomo Q))) (Atomo S), Disyuncion (Disyuncion (Atomo P) (Negacion (Atomo Q))) (Atomo S)]
      Disyuncion (Negacion (Atomo P)) (Negacion (Atomo Q))
      -------------------
      [¬Q, S] * El algoritmo para porque no encuentra ninguna clausula que se pueda resolver
-}

pruebasResolucion :: IO() -- Si se quieren probar todas a la vez *En estas pruebas las clausulas estan ya transformadas a lista 
pruebasResolucion = do
  print (resolucion [[Atomo P]] []) -- [] * Este caso de prueba en el menu no puede ocurrir ya que si introduces un salto de linea da parse error
  print (resolucion [[Atomo P]] [Negacion (Atomo P)]) -- []
  print (resolucion [[Negacion(Atomo P),Negacion(Atomo Q),Atomo S],[Negacion(Atomo P),Negacion(Atomo S),Atomo Q]] [Negacion(Atomo P),Negacion(Atomo Q),Negacion(Atomo S)]) -- [¬P,¬S]
  print (resolucion [[Negacion (Atomo P), Negacion (Atomo Q), Atomo S], [Atomo P, Negacion (Atomo Q), Atomo S]] [Negacion (Atomo P), Negacion (Atomo Q)]) -- [¬Q, S]

-- ======================================================================================== PARTE 3 ========================================================================================

resolver :: [Formula] -> [Formula] -> Bool
resolver x xs = count > 0
    where count = foldl (\acc y -> if negado y `elem` x then acc + 1 else acc) 0 xs

resoluciond :: [[Formula]] -> [Formula] -> [Formula]
resoluciond xs x = resolucionAuxd xs x []

resolucionAuxd :: [[Formula]] -> [Formula] -> [[Formula]] -> [Formula]
resolucionAuxd xs x rep =
    let cumplen = filter (resolver x) xs
        resol = resolvente x (head cumplen)
    in if null cumplen then x else if null resol then resol else if resol `elem` rep then x else resolucionAuxd xs resol (rep ++ [resol])

pruebasResoluciond :: IO() -- Si se quieren probar todas a la vez *En estas pruebas las clausulas estan ya transformadas a lista 
pruebasResoluciond = do
  print (resoluciond [[Atomo P]] []) -- [] * Este caso de prueba en el menu no puede ocurrir ya que si introduces un salto de linea da parse error
  print (resoluciond [[Atomo P]] [Negacion (Atomo P)]) -- []
  print (resoluciond [[Negacion(Atomo P),Negacion(Atomo Q),Atomo S],[Negacion(Atomo P),Negacion(Atomo S),Atomo Q]] [Negacion(Atomo P),Negacion(Atomo Q),Negacion(Atomo S)]) -- [¬P,¬S]
  print (resoluciond [[Negacion (Atomo P), Negacion (Atomo Q), Atomo S], [Atomo P, Negacion (Atomo Q), Atomo S]] [Negacion (Atomo P), Negacion (Atomo Q)]) -- [¬Q, S]

menu :: IO ()
menu = do putStrLn "1. Comprobar si una formula es una clausula"
          putStrLn "2. Comprobar si una formula es una clausula de Horn"
          putStrLn "3. Convertir una formula a lista"
          putStrLn "4. Introducir una formula en FNC y obtener su lista de clausulas"
          putStrLn "5. Resolvente de dos clausulas"
          putStrLn "6. Resolucion de una lista de clausulas y una clausula"
          putStrLn "0. Salir\n"
          putStr "Introduzca una opcion: "
          opcion <- getLine
          putStr "\n"
          if opcion == "1" then do putStr "Introduzca una formula: "
                                   formula <- getLine
                                   let f = read formula :: Formula
                                   if esClausula f then putStrLn "\nLa formula es una clausula"
                                   else putStrLn "\nLa formula no es una clausula"
                                   putStr "\n"
                                   menu
          else if opcion == "2" then do putStr "Introduzca una formula: "
                                        formula <- getLine
                                        let f = read formula :: Formula
                                        if esClausulaHorn f then 
                                          putStrLn "\nLa formula es una clausula de Horn"
                                        else 
                                          putStrLn "\nLa formula no es una clausula de Horn"
                                        putStr "\n"
                                        menu
          else if opcion == "3" then do putStr "Introduzca una formula: "
                                        formula <- getLine
                                        let f = read formula :: Formula
                                        putStr "\nLa lista resultante es: "
                                        print (clausulaLista f)
                                        putStr "\n"
                                        menu
          else if opcion == "4" then do  putStr "Introduzca una formula en FNC: "
                                         formula <- getLine
                                         let f = read formula :: Formula
                                         putStr "\nLa lista resultante es: "
                                         print (fncAlista f)
                                         putStr "\n"
                                         menu
          else if opcion == "5" then do putStr "Introduzca la primera clausula: "
                                        clausula1 <- getLine
                                        putStr "Introduzca la segunda clausula: "
                                        clausula2 <- getLine
                                        let c1 = read clausula1 :: Formula
                                        let c2 = read clausula2 :: Formula
                                        putStr "\nLa resolvente es: "
                                        print (resolvente (clausulaLista c1) (clausulaLista c2))
                                        putStr "\n"
                                        menu
          else if opcion == "6" then do putStr "Introduzca la lista de clausulas: "
                                        clausula1 <- getLine
                                        putStr "Introduzca la clausula: "
                                        clausula2 <- getLine
                                        let c1 = read clausula1 :: [Formula]
                                        let c2 = read clausula2 :: Formula
                                        putStr "\nLa resolucion es: "
                                        print (resolucion (map clausulaLista c1) (clausulaLista c2))
                                        putStr "\n"
                                        menu
          else if opcion == "0" then do return ()
          else do putStrLn "\nOpcion no valida\n"
                  menu