{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

cosa :: [[Int]]
cosa = [take x (iterate id x) | x <- [0..10]]

data SecAB a b = S [(a,b)]

numElemA :: SecAB a b -> Int
numElemA (S xs) = length xs

paraElem :: SecAB a b -> Int -> (a, b)
paraElem (S []) _ = error "NOOOOOOO"
paraElem (S xs) e = xs !! e

instance (Eq a, Eq b)  => Eq (SecAB a b) where
    (S xs) == (S ys) = xs == ys

instance (Show a, Show b) => Show (SecAB a b) where
    show (S xs) = "[" ++ aux xs ++ "]" 
        where aux [] = ""
              aux [(a,b)] = show a ++ ", " ++ show b
              aux ((a,b):xs) = show a ++ ", " ++ show b ++ ", " ++ aux xs 

pot3Fin67 :: Int -> Int
pot3Fin67 n = length [x | x <- take n (iterate(3*) 1), mod x 100 == 67]

numCuestasPos :: (Ord a1, Num a2) => [a1] -> (Int, [a2])
numCuestasPos [] = (0, [])
numCuestasPos [x] = (0, []) 
numCuestasPos (x:xs) = (length l, l)
    where (_, _, l) = foldl (\(ant, cont, l) x -> if x > ant then (x, cont + 1, cont:l) else (x, cont + 1, l)) (x, 1, []) xs


