
b1 :: (Num a, Enum a) => [a]
b1 = concat(zipWith(\x y -> [x, y]) [0,-1..] [1..])

-- 2 a

data Pila a b = Empty | P [Either a b] 

apilaL :: Pila a b -> a -> Pila a b
apilaL Empty x = P [Left x]
apilaL (P xs) x = P (Left x:xs)

alterna :: Pila a b -> Bool
alterna Empty = True
alterna (P [_]) = True
alterna (P []) = True
alterna (P (Left _:Right x:xs)) = alterna (P (Right x:xs))
alterna (P (Right _:Left x:xs)) = alterna (P (Left x:xs))
alterna _ = False 

esDerecha :: Either a b -> Bool 
esDerecha (Right _) = True 
esDerecha (Left _) = False

eliminaR :: Pila a b -> Pila a b
eliminaR Empty = Empty
eliminaR (P xs) = P (foldl (\x y -> if esDerecha y then x else x++[y]) [] xs)

instance Eq (Pila a b) where
    Empty == Empty = True
    Empty == _ = False
    _ == Empty = False 
    (P xs) == (P ys) = eliminaR (P xs) == eliminaR (P ys)

instance (Show a, Show b) => Show (Pila a b) where 
    show Empty = []   
    show (P []) = [] 
    show (P [Right x]) = show x
    show (P [Left x]) = show x
    show (P ((Right x):xs)) = show x ++ " " ++ show (P xs) 
    show (P ((Left x):xs)) = show x ++ " " ++ show (P xs) 

prefijoNeg :: (Ord a, Num a) => [a] -> Bool
prefijoNeg xs = foldl (\x y -> y >= 0 && x) True (dropWhile (<0) xs)

colocaParImpar :: Integral a => [a] -> [a]
colocaParImpar [] = []
colocaParImpar xs = if length pares == length impares then concat(zipWith(\x y -> [x,y]) pares impares) else error "NOOOOO"
    where pares = filter even xs
          impares = filter odd xs

