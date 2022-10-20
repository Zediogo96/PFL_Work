module PolyCalc where

import Data.List(nub, sort, group, groupBy, partition, sortBy, sortOn)
import Data.Function

import PolyParser

-- 5xy^3 + y-> [(5, [('x',1), ('y', 3)]), (1, [('y'], 1)]
simp :: Poly -> Poly
simp [] = []
simp (x:xs) = [(fst x + vs, snd x)] ++ simp resto
    where 
        vs = sum (map (fst) ks)
        (ks, resto) =  partition (\y -> (lisEquals (snd x) (snd y) == True)) xs

addPoly :: Poly -> Poly -> Poly 
addPoly xs ys = (remove_exp_zero (remove_zeros( simp (xs ++ ys))))

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

count :: [Var] -> Char -> Integer
count xs x = case lookup x xs of
  Nothing -> 0 -- x is not in the list
  Just n  -> n -- x is in the list associated with n

-- Extract all keys by taking the first value in each pair
keys :: [Var] -> [Char]
keys xs = map fst xs 

-- Extract the union of all keys of two lists
allKeys :: [Var] -> [Var] -> [Char]
allKeys xs ys = nub (keys xs ++ keys ys)

lisEquals :: [Var] -> [Var] -> Bool
lisEquals [] [] = True
lisEquals xs ys = all test (allKeys xs ys) 
  where
    -- Check that a key maps to the same value in both lists
    test k = count xs k == count ys k

-- 
remove_exp_zero :: Poly -> Poly
remove_exp_zero xs = [(a, [c | c <- b, ((snd c) /= 0)]) | (a,b) <- xs]

-- Returns max expoent of a list of variables 
max_exp :: [Var] -> Integer
max_exp [] = 0
max_exp l = foldr1 (\x y ->if x >= y then x else y) (map (snd) l) 

-- Sort by order of the maximum expoent of the Polynomial and if it's equal sort by it's coefficient
poly_sorter :: Poly -> Poly
poly_sorter l = sortBy ((flip compare `on` max_exp . snd) <> (flip compare `on` fst)) l

-- Wrapper Function for the complete treatment of polynomial
simplify l = (remove_exp_zero (poly_sorter (remove_zeros (simp l))))

helper_func :: [Var] -> Var
helper_func [] = (' ',1)
helper_func l = head l

-- Falta remover zeros, espaços e melhorar derive qd é vazio
derive :: Poly -> Char -> Poly
derive [] _ = []
derive (x:xs) c = (coef, (fst to_derive, (snd (to_derive) - 1)) : diff) : derive xs c
  where 
    (equal, diff) = partition (\(a,b) -> (a == c)) (snd x)
    to_derive = helper_func equal
    coef = (fst x) * ((snd to_derive))

multiply_monoid :: Mono -> Mono -> Mono
multiply_monoid x y = (coef, sumThem (variables))
  where   
    coef = (fst x) * (fst y)
    variables = (snd x) ++ (snd y)

sumThem = map sumGroup . groupBy fstEq . sortOn fst
  where
    sumGroup (x:xs) = (fst x, sum $ map snd (x:xs))
    sumGroup _ = error "This can never happen - groupBy cannot return empty groups"
    fstEq (a, _) (b, _) = a == b


multiply_Poly :: Poly -> Poly -> Poly
multiply_Poly l1 l2 = (remove_exp_zero (concatMap (\x -> map (\y -> multiply_monoid x y) l1) l2))
