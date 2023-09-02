


main::IO()
main = do
    -- Ejercicio 1
    print(nPares 5) -- [0,2,4,6,8]
    print(nParesCuadrado 5) -- [(5,25),(4,16),(3,9),(2,4),(1,1),(0,0)]
    print(nPotencias3 5) -- [1,3,9,27,81]
    print(nMenoresMult35 10) -- 33
    print(e 0) -- 2
    print(e 8) -- 11
    -- Ejercicio 2
    print(iguales f g 1 5) -- False
    print(iguales f g 3 5) -- True
    print(menorA 8 12 primo) -- 11 
    print(menor 8 primo) -- 11
    print(mayorA 1 10 primo) -- 7
    print(pt 1 10 mayorCero) -- True
    print(pt (-1) 10 mayorCero) -- False
    -- Ejercicio 3
    print(filter2 [1..10] odd even) -- [[1,3,5,7,9],[2,4,6,8,10]]
    print(participation even [1..10]) -- ([2,4,6,8,10],[1,3,5,7,9])
    print(mapx 1 [(+1),(+3)]) -- [2,4]
    print(filter1 [[1..10],[2,4],[1,3]] even) -- [[2,4,6,8,10],[2,4],[]]
    print(filters [1..10] [odd,even]) -- [[1,3,5,7,9],[2,4,6,8,10]]
    

-- Funciones auxiliares que se usan para probar

primo :: Integral a => a -> Bool
primo n | n <= 1 = False | otherwise = 0 `notElem` map (mod n)[2.. n-1]

f :: (Ord a, Num a) => a -> Bool
f n | n >= 1 && n <= 5 = True | otherwise = False

g :: (Ord a, Num a) => a -> Bool
g n | n >= 3 && n <= 5 = True | otherwise = False

mayorCero :: (Ord a, Num a) => a -> Bool
mayorCero n | n > 0 = True | otherwise = False 