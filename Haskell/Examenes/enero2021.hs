import Data.Time (picosecondsToDiffTime)

{-
1.
(Bool -> Bool) -> Int -> Int -> Bool
-}



posUltPico :: (Ord a, Num pico) => [a] -> pico
posUltPico [] = 0
posUltPico [_] = 0
posUltPico [_,_] = 0
posUltPico (x:(y:xs)) = p
    where (_, _, _, p) = foldl (\(ant, act, ind, pico) e -> if (ant > act && act < e) then (act, e, ind + 1, ind) else (act, e, ind + 1, pico)) (x, y, 1, -1) xs

data ArbolBin = H | A ArbolBin Int ArbolBin deriving (Show, Read)

simetricos :: ArbolBin -> ArbolBin -> Bool
simetricos H H = True
simetricos (A i1 r1 d1) (A i2 r2 d2) = r1 == r2 && simetricos d1 i2 && simetricos i1 d2
simetricos _ _ = False

identicos :: ArbolBin -> ArbolBin -> Bool
identicos H H = True
identicos (A i1 r1 d1) (A i2 r2 d2) = r1 == r2 && identicos i1 i2 && identicos d1 d2
identicos _ _ = False

instance Eq ArbolBin where 
    H == H = True
    a1 == a2 = simetricos a1 a2 || identicos a1 a2

inorder :: ArbolBin -> [Int]
inorder H = []
inorder (A i r d) = inorder i ++ [r] ++ inorder d

datosHojas :: ArbolBin -> [Int]
datosHojas H = []
datosHojas (A H r H) = [r]
datosHojas (A i r d) = datosHojas i ++ datosHojas d

recorrerArbol :: ArbolBin -> ([Int], [Int])
recorrerArbol H = ([],[])
recorrerArbol (A i r d) = (inorder (A i r d), datosHojas (A i r d))



