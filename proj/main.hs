import Data.List (sortOn)
import Data.Function(on)
import Data.Char(digitToInt)


-- Auxiliar Functions
fst' (a,_,_) = a
snd' (_,a,_) = a
third' (_,_,a) = a

-- (a) 
simplify :: (Num a, Eq b , Eq c) => [(a,b,c)] -> [(a,b,c)]
simplify [] = []
simplify ((a,b,c):xs) = (a + vs, b , c) : simplify resto
    where 
        vs = sum (map (fst') ks)
        ks = filter (\(_,y,z) -> y == b && z == c) xs
        resto = filter (\(_,y,z) -> y /= b || z /= c) xs

sortByChar :: (Num a, Ord b, Eq c) =>  [(a,b,c)] -> [(a,b,c)]
sortByChar [(a,b,c)] = sortOn snd' (simplify [(a,b,c)])

-- (b)

addPoly :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
addPoly [] ys = ys
addPoly xs [] = xs
addPoly ((a,b,c):xs) ((d,e,f):ys) 
    | (b == e && c == f) = ((a+d,b,c):(addPoly xs ys))
{-  | a < c = ((a,b):(addPoly xs ((c,d):ys)))
    | a > c = ((c,d):(addPoly ((a,b):xs) ys)) -}

-- (c)

multPoly :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
multPoly [] ys = ys
multPoly xs [] = xs
multPoly ((a,b,c):xs) ((d,e,f):ys) 
    | (b == e && c == f) = ((a*d,b,c):(multPoly xs ys))

main :: IO() 
main = do

    print("Original:")
    print([(1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)])
    print("Simplificada:")
    print(simplify [(1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)])
    putStr("\n")
    
    let l1 = simplify [(1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4), (6,"z", 7)]
    let l2 = simplify [(1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4)]

    print(l1)
    print("+")
    print(l2)
    print("=")
    print(addPoly l1 l2)
    putStr("\n")
    print(l1)
    print("*")
    print(l2)
    print("=")
    print(multPoly l1 l2)


    -- TESTES RANDOM

    -- print(sortByChar [(1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4)]) -- tá errado para já, tirar dúvida

    -- print(simplify' [(1,3), (1,3), (4,5)])
    -- print(partition ((1 ==) . fst) [(1,2), (1,3), (4,5)]) -- ([(1,2),(1,3)],[(4,5)])
    -- print(partition ((1 ==) . fst' ) [(1,2,3), (1,2,3), (4,5,7)])