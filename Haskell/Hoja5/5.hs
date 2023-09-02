import Control.Monad
-- Hoja 5

-- Ejercicio 1

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

palabrasFrase :: IO Int
palabrasFrase = do frase <- getLine
                   return (length (words frase))

-- Ejercicio 2

palabrasFichero :: String -> IO Int
palabrasFichero fichero = do texto <- readFile fichero
                             return (length (words texto))

-- Ejercicio 3


-- Ejercicio 4

   
promedia :: IO ()
promedia = aux 0 0


aux :: Int -> Int -> IO ()
aux x cont = do n <- getInt
                if n == -1 then return ()
                    else do let suma = x + n
                            let numeros = cont + 1
                            let media = div suma numeros 
                            putStrLn ("La suma es " ++ show suma ++ " y la media es " ++ show media)
                            aux suma numeros

