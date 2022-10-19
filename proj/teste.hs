import PolyParser
import ReverseParser

import Data.List(nub, sort, group, groupBy, partition, sortBy)
import Data.Function

-- 5xy^3 + y-> [(5, [('x',1), ('y', 3)]), (1, [('y'], 1)]
type Poly = [(Integer, [(Char, Integer)])]

simplify' :: Poly -> Poly
simplify' [] = []
simplify' (x:xs) = [(fst x + vs, snd x)] ++ simplify' resto
    where 
        vs = sum (map (fst) ks)
        (ks, resto) =  partition (\y -> (lisEquals (snd x) (snd y) == True)) xs
        -- resto = filter (\y -> (lisEquals (snd x) (snd y) == False)) xs

addPoly' :: Poly -> Poly -> Poly 
addPoly' xs ys = simplify' (xs ++ ys)

sumByKey :: (Eq k, Num v) => [(k, v)] -> [(k, v)]
sumByKey []         = []
sumByKey ((k,v):xs) = (k,v + sum vs) : sumByKey bs
  where
    vs       = map snd ks
    (ks, bs) = partition ((k ==) . fst) xs

interweave :: [(a,b)] -> [(a,b)] -> [a]
interweave [] [] = []
interweave [] ys = [fst (head ys)]
interweave xs [] = [fst (head xs)]
interweave (x:xs) (y:ys) = (fst x) : (fst y) : interweave xs ys

count :: [(Char, Integer)] -> Char -> Integer
count xs x = case lookup x xs of
  Nothing -> 0 -- x is not in the list
  Just n  -> n -- x is in the list associated with n

-- Extract all keys by taking the first value in each pair
keys :: [(Char, Integer)] -> [Char]
keys xs = map fst xs 

-- Extract the union of all keys of two lists
allKeys :: [(Char, Integer)] -> [(Char, Integer)] -> [Char]
allKeys xs ys = nub (keys xs ++ keys ys)

lisEquals :: [(Char, Integer)] -> [(Char, Integer)] -> Bool
lisEquals xs ys = all test (allKeys xs ys) 
  where
    -- Check that a key maps to the same value in both lists
    test k = count xs k == count ys k

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: Poly -> Poly
remove_zeros xs = [c | c <- xs, (fst c /= 0)]

-- Sort by order of first element and then comparing the first
poly_sorter l = sortBy (flip compare `on` fst) l --((compare `on` (snd) <> 

-- Wrapper Function for the complete treatment of polynomial
simplify_total l = poly_sorter (remove_zeros (simplify' l))

derive (x:xs) a = 
  
[(x * z, y, z - 1) | (x,y,z) <- xs]

main :: IO() 
main = do
    print(derive )
    putStr("\n-- Alínea (a) -----------------\n\n")

    let p1 = [(5,[('x',1),('y',1),('z',3)]),(20,[('y',4)]),(-5,[('z',5)]),(-1,[('x',2)]),(-5,[]),(-1,[('x',1)])]
    print(simplify_total p1)
    
    putStr("\n-- Alínea (b) -----------------\n\n")
    let p2 = [(10,[('x',1),('y',1),('z',3)]),(-20,[('y',4)]),(-5,[('z',5)]),(-1,[('x',2)]),(-5,[]),(-1,[('x',1)])]
    let p3 = addPoly' p1 p2
    print(remove_zeros p3)  
