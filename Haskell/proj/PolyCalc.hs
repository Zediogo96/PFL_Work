module PolyCalc where

import Data.List(nub, sort, group, groupBy, partition, sortBy, sortOn, intercalate)


import PolyParser

-- Simplifies two polinomials, adding of the ones who have the exact same variables 
simplify' :: Poly -> Poly
simplify' [] = []
simplify' (x:xs) = [(fst x + vs, snd x)] ++ simplify' resto
    where 
        vs = sum (map (fst) ks)
        (ks, resto) =  partition (\y -> (lisEquals (snd x) (snd y) == True)) xs

-- sort by first element of tuple of var (Char)
sortVar :: [Var] -> [Var]
sortVar [] = []
sortVar (x:xs) = sortVar (filter (\y -> (fst y) < (fst x)) xs) ++ [x] ++ sortVar (filter (\y -> (fst y) >= (fst x)) xs)

-- check if two lists of vars are the same
lisEquals :: [Var] -> [Var] -> Bool
lisEquals [] [] = True
lisEquals (x:xs) (y:ys) = if ((fst x) == (fst y) && (snd x) == (snd y)) then lisEquals xs ys else False
lisEquals _ _ = False

-- Removes variables from a Polynomial that have a power of zero
remove_exp_zero :: Poly -> Poly
remove_exp_zero xs = [(a, [c | c <- b, ((snd c) /= 0)]) | (a,b) <- xs]

-- Simplifies the process of extracting the list of Vars to derive
to_derivePoly' :: [Var] -> Var
to_derivePoly' [] = (' ',0) -- ISTO FICOU ERRADO PROVAVELMENTE POR CAUSA DE UM COMMIT
to_derivePoly' l = head l

-- derivePolys a polynomial using a chosen Variable
derivePoly' :: Poly -> Char -> Poly
derivePoly' [] _ = []
derivePoly' (x:xs) c = (coef, (fst (to_derivePoly' equal), (snd (to_derivePoly' equal) - 1)) : diff) : derivePoly' xs c
  where 
    (equal, diff) = partition (\(a,b) -> (a == c)) (snd x)
    coef = (fst x) * ((snd (to_derivePoly' equal)))
 
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
multiplyPoly' :: Poly -> Poly -> Poly
multiplyPoly' l1 l2 = (remove_exp_zero (concatMap (\x -> map (\y -> multiply_monoid x y) l1) l2))


-- WrapperFunctions  -----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Polynomial Simplification
simplify :: Poly -> Poly
simplify l = (remove_exp_zero (poly_sorter (remove_zeros (simplify' l))))

-- Polynomial Addition 
addPoly :: Poly -> Poly -> Poly 
addPoly xs ys = simplify (remove_exp_zero (remove_zeros( simplify' (xs ++ ys))))


-- Polynomial Multiplication
multiplyPoly :: Poly -> Poly -> Poly
multiplyPoly p1 p2 = simplify (multiplyPoly' p1 p2)

--Polynomial derivation according to a variable
derivePoly :: Poly -> Char -> Poly
derivePoly l c = simplify (derivePoly' l c)

