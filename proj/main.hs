import Data.List (sortBy, sort)
import Data.Ord (comparing)
import Data.Function (on)


data Poly = Poly ((Integer, Integer, Integer)) deriving (Eq)

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


remove_zeros :: (Num a, Eq a) => [(a,b,c)] -> [(a,b,c)]
remove_zeros xs = [c | c <- xs, (fst' c /= 0)]

-- Sort by order of 
poly_sorter l = sortBy ((compare `on` snd') <> (flip compare `on` fst')) l

-- Wrapper Function for the complete treatment of polynomial
simplify_total l = poly_sorter (remove_zeros (simplify l))

-- (b)

addPoly :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
addPoly [] ys = ys
addPoly xs [] = xs
addPoly ((a,b,c):xs) ((d,e,f):ys) 
    | (b == e && c == f) = ((a+d,b,c):(addPoly xs ys))
    | otherwise = ((a,b,c) : addPoly xs ys)

-- (c)

multPoly :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
multPoly [] ys = ys
multPoly xs [] = xs
multPoly ((a,b,c):xs) ((d,e,f):ys) 
    | (b == e && c == f) = ((a*d,b,c):(multPoly xs ys))
    | otherwise = ((a,b,c):(d,e,f):addPoly xs ys)

main :: IO() 
main = do

    print("Original:")
    print([(-1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)])
    print("Simplificada:")
    print(simplify_total [(-1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)])
    putStr("\n")
    
    let l1 = simplify_total [(1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4), (6,"z", 7)]
    let l2 = simplify_total [(-1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4)]

    print(l1)
    print("+")
    print(l2)
    print("=")
    print(addPoly l1 l2)
    putStr("\n")

    {- 
    print(l1)
    print("*")
    print(l2)
    print("=")
    print(multPoly l1 l2) 
    -}


    -- TESTES RANDOM

    -- print(poly_sorter (simplify_total [(-1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)]))

    -- print(simplify' [(1,3), (1,3), (4,5)])
    -- print(partition ((1 ==) . fst) [(1,2), (1,3), (4,5)]) -- ([(1,2),(1,3)],[(4,5)])
    -- print(partition ((1 ==) . fst' ) [(1,2,3), (1,2,3), (4,5,7)])