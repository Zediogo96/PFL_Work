import PolyParser

import Data.List (sortBy, sort, insert, unfoldr)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Char (isSpace)

type Poly = (Integer, Char, Integer)

-- TÁ A VOLTARIIIII

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

    putStr("\n-- Alínea (a) -----------------\n\n")

    let l1 = parse_poly "5*xyz^3 - 10*y^4 - 5*z^5 - x^2 - 5 - x + 30*y^4"

    putStr("Polynomials Antes\n")
    print(l1)
    putStr("Polynomial Simplificado:\n")
    print(simplify' l1)

    putStr("\n-- Alínea (b) -----------------\n\n")

        putStr("\n-- Alínea (c) -----------------\n\n")

    putStr("\n-- Alínea (d) -----------------\n\n")
    
    putStr("Polynomials Antes\n")
    
    putStr("Derivado:\n")    
    








    -- TESTES RANDOM
    -- print(poly_sorter (simplify_total [(-1,"x",2), (1, "x",2),(1, "y",3),(2, "z" ,4), (5,"y",4)]))

    -- print(simplify' [(1,3), (1,3), (4,5)])
    -- print(partition ((1 ==) . fst) [(1,2), (1,3), (4,5)]) -- ([(1,2),(1,3)],[(4,5)])
    -- print(partition ((1 ==) . fst' ) [(1,2,3), (1,2,3), (4,5,7)])



