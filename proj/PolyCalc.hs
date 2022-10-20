module PolyCalc where

import Data.List(nub, sort, group, groupBy, partition, sortBy, sortOn)
import Data.Function

import PolyParser

-- Simplifies two polinomials, adding of the ones who have the exact same variables 
simp :: Poly -> Poly
simp [] = []
simp (x:xs) = [(fst x + vs, snd x)] ++ simp resto
    where 
        vs = sum (map (fst) ks)
        (ks, resto) =  partition (\y -> (lisEquals (snd x) (snd y) == True)) xs

-- Wrapper function to add poly, using the
addPoly :: Poly -> Poly -> Poly 
addPoly xs ys = (remove_exp_zero (remove_zeros( simp (xs ++ ys))))

-- Sum Variables that have the same key
sumByKey :: (Eq k, Num v) => [(k, v)] -> [(k, v)]
sumByKey []         = []
sumByKey ((k,v):xs) = (k,v + sum vs) : sumByKey bs
  where
    vs       = map snd ks
    (ks, bs) = partition ((k ==) . fst) xs

-- Counts the number of times a variable appears in a list of variables
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

-- Checks if two lists are equal, by checking the number of different variables
lisEquals :: [Var] -> [Var] -> Bool
lisEquals [] [] = True
lisEquals xs ys = all test (allKeys xs ys) 
  where
    -- Check that a key maps to the same value in both lists
    test k = count xs k == count ys k

-- Removes variables from a Polynomial that have a power of zero
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
simplify :: Poly -> Poly
simplify l = (remove_exp_zero (poly_sorter (remove_zeros (simp l))))

helper_func :: [Var] -> Var
helper_func [] = (' ',1)
helper_func l = head l

-- Derives a polynomial using a chosen Variable
derive' :: Poly -> Char -> Poly
derive' [] _ = []
derive' (x:xs) c = (coef, (fst to_derive', (snd (to_derive') - 1)) : diff) : derive' xs c
  where 
    (equal, diff) = partition (\(a,b) -> (a == c)) (snd x)
    to_derive' = helper_func equal
    coef = (fst x) * ((snd to_derive'))

-- Wrapper function to derive
derive :: Poly -> Char -> Poly
derive l c = simplify (derive' l c)
  
-- Multiplies two monomials
multiply_monoid :: Mono -> Mono -> Mono
multiply_monoid x y = (coef, sumThem (variables))
  where   
    coef = (fst x) * (fst y)
    variables = (snd x) ++ (snd y)

-- Used to list of variables, summing the ones that have the same key
sumThem :: [Var] -> [Var]
sumThem = map sumGroup . groupBy fstEq . sortOn fst
  where
    sumGroup (x:xs) = (fst x, sum $ map snd (x:xs))
    sumGroup _ = error "This can never happen - groupBy cannot return empty groups"
    fstEq (a, _) (b, _) = a == b

-- Wrapper function to multiply two polynomials
multiply_Poly :: Poly -> Poly -> Poly
multiply_Poly l1 l2 = (remove_exp_zero (concatMap (\x -> map (\y -> multiply_monoid x y) l1) l2))