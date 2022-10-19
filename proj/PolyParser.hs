module PolyParser where

import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)
import Data.List

type Var = (Char, Integer)
type Mono = (Integer, [Var])
type Poly = [Mono]

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

-- Auxiliar function to remove arguments in which their variables is an empty space
remove_empty :: [Var] -> [Var]
remove_empty xs = [c | c <- xs, ((fst c) /= ' ')]

-- Wrapper function to apply remove empty to all monoids
wrapRemEmpty :: Poly -> Poly
wrapRemEmpty ls = [(fst x, remove_empty (snd x)) | x <- ls]

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
parse_poly :: String -> [(Integer, [(Char, Integer)])]
parse_poly [] = []
parse_poly s = map (parseMonomial) (remove_mult (remove_plus (simplify_minus (formatSpace s))))


-- REVERSE PARSER ---------------------------------------------------------------------
---------------------------------------------------------------------------------------

reverse_ps_monoid :: Mono -> String 
reverse_ps_monoid (a,b) 
    | (a == 1)  = "+ " ++ parse_variables b
    | (a == -1) = "- " ++ parse_variables b
    | (a < 0)   = "- " ++ (show (abs a)) ++ parse_variables b
    | otherwise = "+ " ++ (show a) ++ parse_variables b

parse_variables :: [(Char, Integer)] -> String
parse_variables [] = ""
parse_variables (x:xs) = [(fst x)] ++ parse_var (snd x) ++ parse_variables xs

parse_var :: Integer -> String
parse_var i
    | i == 1 = ""
    | i == -1 = "-"
    | otherwise = "^" ++ show i

handle_first_plus :: String -> String
handle_first_plus s 
    | ((head s) == '+') = (drop 2 s)


reverse_parser :: [Mono] -> String
reverse_parser l = handle_first_plus (intercalate " " (map (reverse_ps_monoid)  l))