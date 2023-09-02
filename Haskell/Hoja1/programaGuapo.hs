puta :: [[Char]]
puta = ["puta" | x <- [1..(9^9^9)]]

main :: IO() 
main = do 
    print puta 