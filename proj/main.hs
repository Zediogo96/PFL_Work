import Data.List (sortBy, sort, insert, unfoldr)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Char (isSpace)

type Poly = (Integer, Char, Integer)

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

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: (Num a, Eq a) => [(a,b,c)] -> [(a,b,c)]
remove_zeros xs = [c | c <- xs, (fst' c /= 0)]

-- Sort by order of first element and then comparing the first
poly_sorter l = sortBy ((compare `on` snd') <> (flip compare `on` fst')) l

-- Wrapper Function for the complete treatment of polynomial
simplify_total l = poly_sorter (remove_zeros (simplify l))

-- (b)

{-
addPoly :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
addPoly [] ys = ys
addPoly xs [] = xs
addPoly ((a,b,c):xs) ((d,e,f):ys) 
    | (b == e && c == f) = ((a+d,b,c):(addPoly xs ys))
    | otherwise = (a,b,c) : (d,e,f) : (addPoly (drop 1 xs) ys)
-}

addPoly' :: (Num a, Ord b, Eq c) => [(a,b,c)] -> [(a,b,c)] -> [(a,b,c)]
addPoly' xs ys = simplify (xs ++ ys)

result' :: [(Integer, [Char], Integer)] -> [(Integer, [Char], Integer)] 
result' xs = map (\(a,b,c) -> if (b == " ") then ((a^c, " ", 1)) else (a,b,c)) xs

-- (c)

-- FALTA ESTA BUFFAIGE

-- (d)

derive xs = [(x * z, y, z - 1) | (x,y,z) <- xs]

derivePoly xs = simplify_total (derive xs)

main :: IO() 
main = do
    
    let l1 = simplify_total [(1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4), (6,"z", 7)]
    let l2 = simplify_total [(-1,"x",2), (1,"x",2),(1,"y",3),(2,"z",4), (5,"y",4), (7," ", 3)]
    let l3 = []
    let l4 = l1 ++ l2
    let l5 = l1 ++ l3
    let l6 = [(0,"x",1),(20,"y",0),(3,"y",2),(42,"z",6),(8,"z",3),(20,"y",3),(3,"y",2),(8,"z",3)]

    let l7 = derivePoly l4

    putStr("\n")
    print("Originals:")
    print(l1)
    print(l2)
    print("Sum of Polynomials:")
    print(addPoly' l1 l2)

    putStr("\n")
    print("Before Derivation:")
    print(l4)
    print("After Derivation:")
    print(l7)
    print("Final Result:")
    print(result' l7)

    -- TESTES RANDOM
    -- print(poly_sorter (simplify_total [(-1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)]))

    -- print(simplify' [(1,3), (1,3), (4,5)])
    -- print(partition ((1 ==) . fst) [(1,2), (1,3), (4,5)]) -- ([(1,2),(1,3)],[(4,5)])
    -- print(partition ((1 ==) . fst' ) [(1,2,3), (1,2,3), (4,5,7)])



