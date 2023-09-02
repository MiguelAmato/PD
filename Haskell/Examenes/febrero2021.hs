{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}

f :: (Num a, Enum a) => (a -> Bool) -> a -> (a -> Bool) -> a -> [[a]]
f p1 a p2 b = concat (map (\x -> map (\y -> [x,y]) (filter p2 [x + 1 .. b])) (filter p1 [1..a]))

data Rac = F Int Int deriving Show

simplifica :: Rac -> Rac
simplifica (F n 0) = error "NOOO"
simplifica (F n d) = if (n > 0 && d > 0) || (n < 0 && d < 0) then F (div (abs n) m) (div (abs d) m) else F (-(div (abs n) m)) (div (abs d) m)
    where m = gcd (abs n) (abs d)

suma :: Rac -> Rac -> Rac
suma (F n1 d1) (F n2 d2) = if (d1 == d2) then simplifica (F (n1+n2) d2) else simplifica (F ((div mcm d1*n1) + (div mcm d2*n2)) mcm)
    where mcm = lcm d1 d2

multiplicacion :: Rac -> Rac -> Rac
multiplicacion (F n1 d1) (F n2 d2) = simplifica (F (n1*n2) (d1*d2))

absoluto :: Rac -> Rac
absoluto (F n d) = simplifica (F (abs n) (abs d))


instance Eq Rac where
    (F n1 d1) == (F n2 d2) = n1s == n2s && d1s == d2s
        where (F n1s d1s) = simplifica(F n1 d1)
              (F n2s d2s) = simplifica(F n2 d2)

instance Num Rac where
    (F n1 d1) + (F n2 d2) = suma (F n1 d1) (F n2 d2)
    (F n1 d1) * (F n2 d2) = multiplicacion (F n1 d1) (F n2 d2)
    signum (F n d) | (n > 0 && d > 0) || (n < 0 && d < 0) = 1 | n == 0 = 0 | otherwise = -1 
    fromInteger n = F (fromInteger n) 1

parteLista :: [a] -> [([a], [a])]
parteLista xs = [(take x xs, drop x xs) | x <- [0..length xs]]


