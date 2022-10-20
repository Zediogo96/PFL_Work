module PolyParser where

import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)
import Data.List (intercalate)

type Var = (Char, Integer)
type Mono = (Integer, [Var])
type Poly = [Mono]

-- NORMAL PARSER ---------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- Helper function just to make the process of reading an integer less verbose on another functions
read_Int :: String -> Integer
read_Int s = read s :: Integer 

-- Splits lists by chosen Char, only used with '+' in this project
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

-- Remove all spaces from a string, for easier parsing
formatSpace :: String -> String
formatSpace = filter (not . isSpace)

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: Poly -> Poly
remove_zeros xs = [c | c <- xs, (fst c /= 0)]

-- Wrapper function to apply remove empty to all monoids
remove_empty :: Poly -> Poly
remove_empty ls = [(a, [c | c <- b, ((fst c) /= ' ')]) | (a,b) <- ls]

-- Clever way to parse the polynomial, add an extra '+' before every '-'
-- so after we split the string by '+', it helps us keep the '-'
simplify_minus :: String -> String
simplify_minus [] = ""
simplify_minus (x:xs)
        | x == '^' = x : head xs : simplify_minus (tail xs)
        | x == '-' = "+-" ++ simplify_minus xs
        | otherwise =  x : simplify_minus xs

-- Splits an String by occurrences of '+' and creates a list of those sub-strings
split_by_plus :: String -> [String]
split_by_plus s =  split '+' s

-- Removes multiplication on substrings
remove_mult :: [String] -> [String]
remove_mult l = map (filter (not . (=='*'))) l 

-- Function used to separate a variable that has an power. This translates ["y^2] to [["y", "2"]] 
remove_power :: [String] -> [String]
remove_power  [] = []
remove_power (x:xs) = (split '^' x) ++ remove_power xs

-- Parses the coefficient, handling negative values and the absence of coefficient
parseCoef :: String -> Integer
parseCoef [] = 1
parseCoef s 
        | (head s == '-' && (length s) > 1) = negate (read (tail s) :: Integer)
        | (head s == '-') = -1
        | otherwise = read s :: Integer 

-- Error proof function to help the absence of digits in the expoent of the monoid
handle_absence_exp :: String -> Integer
handle_absence_exp s
        | s == [] = 1
        | otherwise = read s :: Integer

-- Parses
parseUnitMonomial :: String -> [Var]
parseUnitMonomial s = map parseVar (remove_power (remove_mult (split_by_plus (simplify_minus s))))

-- Parses the variables and their exponents, handling the absence of exponents
parseVar :: String -> Var
parseVar s = (head s, handle_absence_exp (tail s))


-- Wrapper function to parse an Monomial: separates coefficient of the monomial and then parses the rest of the variables using parseUnitMonomial
parseMonomial :: String -> Mono
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (0, parseUnitMonomial varPows)
        (a, []) -> (read_Int a, [])
        (coef, varPows) -> (parseCoef coef, parseUnitMonomial varPows) 

-- Wrapper function that handles all the necessary steps for parsing a Polynomial
parsePoly :: String -> Poly
parsePoly s = remove_empty (remove_zeros (map parseMonomial (split_by_plus (simplify_minus (formatSpace s)))))

-- REVERSE PARSER ---------------------------------------------------------------------
---------------------------------------------------------------------------------------

remove_only_first_plus :: String -> String
remove_only_first_plus [] = []
remove_only_first_plus (x:xs)
        | x == '+' = xs
        | otherwise = x:xs

-- Auxiliar function to parse a variable into a string, handling cases when expoent is 1
reverse_var :: Var -> String
reverse_var (a, 1) = [a]
reverse_var (a, b) = [a] ++ "^" ++ show b 

-- Handles the display of signals in the case positive number, since '+' is generally ommitted
handle_signal :: Integer -> String
handle_signal a
        | a < 0 = show a
        | otherwise = "+" ++ show a

-- Auxiliar function to parse a monomial into a string, handling cases when there is no expoents (monomial is a constant)
reverse_mono :: Mono ->String
reverse_mono (a, []) = handle_signal a
reverse_mono (a, b) = handle_signal a ++ (intercalate "" (map (reverse_var) b))

-- Wrapper function to parse a polynomial into a string
rev_parse :: Poly -> String
rev_parse [] = ""
rev_parse (x:xs) = reverse_mono x ++ rev_parse xs

reverse_parser :: Poly -> String
reverse_parser s = remove_only_first_plus (rev_parse (s))

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

