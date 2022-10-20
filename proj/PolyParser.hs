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

-- Wrapper function to apply remove empty to all monoids
remove_empty :: Poly -> Poly
remove_empty ls = [(a, [c | c <- b, ((fst c) /= ' ')]) | (a,b) <- ls]

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: Poly -> Poly
remove_zeros xs = [c | c <- xs, (fst c /= 0)]

-- Clever way to parse the polynomial, add an extra '+' before every '-'
-- so after we split the string by '+', it helps us keep the '-'
simplify_minus :: String -> String
simplify_minus [] = ""
simplify_minus (x:xs)
        | x == '^' = x : head xs : simplify_minus (tail xs)
        | x == '-' = "+-" ++ simplify_minus xs
        | otherwise =  x : simplify_minus xs

-- Removes first element from list without blowing up if the list is empty (like regular init)
pop :: [a] -> [a]
pop [] = []
pop xs = init xs

-- Splits an String by occurrences of '+' and creates a list of those sub-strings
remove_plus :: String -> [String]
remove_plus s =  split '+' s

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

-- Cheeky way to handle all if a monoid is made of multiple variables with the help of zip, like xyz turns into (1, [('x',1),('y',1),('z',1)])
cheeky_zipper :: (String, String) -> [(Char, Integer)]
cheeky_zipper (l,p) 
    | (p == "") =  zip l (repeat 1)
    | ((length l) == 1) = [(head l, 1)]
    | otherwise = zip (pop l) (repeat 1)

-- Separates coefficient of the monomial and then parses the rest of the variables using parseUnitMonomial
parseMonomial :: String -> (Integer, [(Char, Integer)])
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (1, parseUnitMonomial varPows)
        (coef, varPows) -> (parseCoef coef, parseUnitMonomial varPows) 

-- Wrapper function that handles all the necessary steps for parsing a the variables of the monoid
parseUnitMonomial :: String -> [(Char, Integer)]
parseUnitMonomial [] = []
parseUnitMonomial l 
        | (l == []) = []
        | (all isAlpha l) = cheeky_zipper (vars,pows)
        | (length (vars) > 1) = (cheeky_zipper (vars,pows)) ++ [(last vars, read_Int (takeWhile (isDigit) (tail fx)))] ++ parseUnitMonomial rest
        | otherwise = [(head vars, handle_absence_exp (takeWhile (isDigit) (tail pows)))] ++ parseUnitMonomial rest
        where 
        (vars, pows) = span isAlpha l
        (fx, rest) = break isAlpha pows

-- Wrapper function for all the functions necessary to the parser
parsePoly :: String -> [(Integer, [(Char, Integer)])]
parsePoly [] = []
parsePoly s = map (parseMonomial) (remove_mult (remove_plus (simplify_minus (formatSpace s))))

-- REVERSE PARSER ---------------------------------------------------------------------
---------------------------------------------------------------------------------------

-- Removes the plus before the first element if he has one (kinda a workaround)
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

-- Wrapper function to completely parse a polynomial into a string
reverse_parser :: Poly -> String
reverse_parser s = remove_only_first_plus (rev_parse (s))

---------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------

