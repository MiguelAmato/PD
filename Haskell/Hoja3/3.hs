a :: [[Integer]]
a = [[ x^n | x <- [1..20]]| n <- [1..10]]

-- b
b :: [[Integer]]
b = [[ n^x | x <- [1..10]]| n <- [1..20]]

-- c 
c :: [(Integer, Integer)]
c = [((2^x) - 1, 2 * ((2^x) - 1)) | x <-[0..10]]

-- d 
d :: [Integer]
d = [x * ((-1)^(x+1)) | x <- [1..]]

-- Ejercicio 2

divisores19a50 :: [(Integer, [Integer])]
divisores19a50 = [(x, [d | d<-[1..(x - 1)], mod x d == 0]) | x <- [19..50]]

perfectos :: [Integer]
perfectos = [x | x <- [1..20000], sum([d | d<-[1..(x - 1)], mod x d == 0]) == x]

-- c
divisoresBien :: Integer -> Integer -> [(Integer, [Integer])] 
divisoresBien n m = [(x, [d | d<-[1..(x - 1)], mod x d == 0]) | x <- [n..m]]

perfectosBien :: Integer -> [Integer]
perfectosBien n = [x | x <- [1..n], sum([d | d<-[1..(x - 1)], mod x d == 0]) == x]

-- Ejercicio 3

ejercicio3 :: Num b => b -> [b] -> [(b, b)]
ejercicio3 x y = [(g x (last y) u , g x (last y) (u + 1)) | u <- y]

g :: Num a => a -> a -> a -> a
g x y u = (x + (x * y)) * u

-- Ejercicio 4 

a4 :: Integral a => a -> [a]
a4 n = filter even (map(\x-> x*x )[1..n])

-- concat(map(\x->map(+x) (filter p [x..m]))[1..n]);
b4 :: Integral a => (a -> Bool) -> a -> a -> [a]
b4 p n m = concatMap(\x->map(+x) (filter p [x..m]))[1..n]

c4 :: Integral a => (a -> Bool) -> (a -> Bool) -> a -> a -> [a]
c4 p q n m = concatMap(\x->map(+x) (filter q [x..m]))(filter (\x->p(n-x))[1..n]);

-- Ejercicio 5

minimoDesde :: Integral a => (a -> Bool) -> a -> a
minimoDesde p n = head[x | x <- [n..], p x]

primo :: Integral a => a -> Bool
primo n | n <= 1 = False | otherwise = 0 `notElem` map (mod n)[2.. n-1]

paresHasta :: Integral a => a -> [a]
paresHasta n = [x | x <- [0..n], even x]

listpares :: Integral a => a -> [a]
listpares n = [2*x | x <- [0..n-1]]

mezclaParImpar :: Integral a => [a] -> [a] -> [(a, a)]
mezclaParImpar xs ys = [(x, y) | x <- xs, even x, y <- ys, odd y]

prefijos :: [Integer] -> [[Integer]]
prefijos xs = [ take x xs | x <- [0..length xs]] 

-- HOJA 2 CON LISTAS MAMADAS

-- 1
-- a
nPrimerosPares :: Integer -> [Integer]
nPrimerosPares n = [2*x | x <- [0..n-1]]

-- b
paresCuadrado :: Integer -> [(Integer, Integer)]
paresCuadrado n = [(x, x^2) | x <- [n, n-1..0]]

-- c 
sumaCos :: (Floating a, Enum a) => a -> a
sumaCos n = sum [abs(i * cos i) | i <- [1..n]]

-- d
sumaMult3y5 :: Integral a => a -> a
sumaMult3y5 n = sum([ x | x <- [n-1, n-2..0], mod x 5 == 0 || mod x 3 == 0])

-- e 
potencias3 :: Integral a => a -> Int
potencias3 n = length[ x | x <-[ 3^y | y <- [1..n-1]], mod x 100 == 43]

-- 2
-- a

--(head (drop x xs), head (drop x ys))

mcdList :: [[a] -> a] -> a
mcdList xs = foldl gcd last xs xs

-- sip :: [a] -> [b] -> [(a, b)]


-- masimo :: [a] -> [a] -> [a] -> Int
-- masimo xs ys zs  = (length (xs++ys++zs) * max (length xs) (max (length ys) (length zs)))

-- , e <- [1..(length (xs++ys++zs) * max (length xs) (max (length ys) (length zs)))]

-- sip xs ys zs = [ head[ tuplas | tuplas <- [ (x,y,z) | x<-xs,y<-ys,z<-zs]], head(drop (* max (length xs) (max (length ys) (length zs))))]

-- b
imparesEn :: [Integer] -> [Integer]
imparesEn xs = [x | x <- xs, odd x]


-- escalar :: Integral a => [a] -> [a] -> a
-- escalar xs ys = sum[x * y | x y <- xs, ys]

--[5,5,5,5,5, 5,5,5, 5,5,(3,3,1),(3,3,2),(3,3,3),(3,3,4),(3,3,5),(3,4,1),(3,4,2),(3,4,3),(3,4,4),(3,4,5)]
-- [5,5,5,5, 5,5*,5,5, 5,5,5*,5]
--



--numeroDivideLista :: Integral a => a -> [a] -> Bool
-- numeroDivideLista n xs = foldr and (last([mod x n == 0| x <- xs])) ([mod x n == 0| x <- xs])

