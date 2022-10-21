module PolyParser where
import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)
import Data.Function
import Data.List


type Var = (Char, Integer)
type Mono = (Integer, [Var])
type Poly = [Mono]

-- Returns max expoent of a list of variables 
max_exp :: [Var] -> Integer
max_exp [] = 0
max_exp l = foldr1 (\x y ->if x >= y then x else y) (map (snd) l) 

-- Sort by order of the maximum expoent of the Polynomial and if it's equal sort by it's coefficient
poly_sorter :: Poly -> Poly
poly_sorter l = sortBy ((flip compare `on` max_exp . snd) <> (flip compare `on` fst)) l

-- NORMAL PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Remove all spaces from a string, for easier parsing
formatSpace :: String -> String
formatSpace = filter (not . isSpace)

-- Wrapper function just to make the process of reading an integer less verbose on another functions
read_Int :: String -> Integer
read_Int s = read s :: Integer 

-- Clever way to parse the polynomial, add an extra '+' before every '-'
-- so after we split the string by '+', it helps us keep the '-'
simplify_minus :: String -> String
simplify_minus [] = ""
simplify_minus (x:xs)
        | x == '^' = x : head xs : simplify_minus (tail xs)
        | x == '-' = "+-" ++ simplify_minus xs
        | otherwise =  x : simplify_minus xs

-- Removes multiplication on a string
remove_mult :: String -> String
remove_mult l = filter (/='*') l

-- Slipts lists by chosen Char, only used with '+' in this project
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

-- Splits an String by occurrences of '+' and creates a list of those sub-strings
remove_plus :: String -> [String]
remove_plus s =  split '+' s

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: Poly -> Poly
remove_zeros xs = [c | c <- xs, (fst c /= 0)]

-- filter parenthesis from string
filReqSymbols :: String -> String
filReqSymbols s = filter (not . (`elem` "()^")) s

-- switch empty string for 1
extractExp :: String -> Integer
extractExp s 
    | (stripped == []) = 1
    | otherwise = read stripped :: Integer
    where stripped = filReqSymbols s

-- safe version of head, adapted to our needs
safe_head :: String -> Char
safe_head [] = ' '
safe_head xs = head xs

-- safe version of tail, adapted to our needs
safe_tail :: String -> String
safe_tail [] = []
safe_tail xs = tail xs

-- Parses the coefficient, handling negative values and the absence of coefficient
parseCoef :: String -> Integer
parseCoef [] = 1
parseCoef s 
        | (head s == '-' && (length s) == 1) = -1
        | (head s == '-' && (length s) > 1) = negate (read (tail s) :: Integer)
        | otherwise = read s :: Integer

-- turn string into list of tuple with every second element as 1
parseVar :: String -> [Var]
parseVar [] = []
parseVar s = [(safe_head s, 1)] ++ parseVar (safe_tail s)

-- Converts a String containing only content for variables and converts into a list of [(Char, Integer)] -> e.g "xy"
parseMonomialVars :: String -> [Var]
parseMonomialVars [] = []
parseMonomialVars s 
    | ((all isAlpha rem) && (rem == [])) =  parseVar var ++ parseMonomialVars rem
    | otherwise = [(safe_head var, extractExp tst)] ++ parseMonomialVars rest
    where 
        (var, rem) = (takeWhile (isAlpha) s, dropWhile (isAlpha) s)
        (tst, rest) = ((takeWhile (\n -> (isDigit n) || ((`elem` "-()^")) n) rem), (dropWhile(\n -> (isDigit n) || ((`elem` "-()^")) n) rem))

-- By using simplify minus, with the case of negative expoent (-2), would end up like (+-2), and that was not initially intended, this works it around
replacePattern :: String -> String
replacePattern [] = []
replacePattern ('(':'+':'-':a:')':xs) = '(':'-':a:')': replacePattern xs
replacePattern (x:xs) = x:replacePattern xs

-- Separates coefficient of the monomial and then parses the rest of the variables using parseUnitMonomial
parseMonomial :: String -> (Mono)
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (1, parseMonomialVars s)
        (a, []) -> (read a :: Integer, [])
        (coef, varPows) -> (parseCoef coef, parseMonomialVars varPows) 


-- END NORMAL PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- REVERSE PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- check if number is negative
isNegative :: Integer -> Bool
isNegative n = n < 0

-- Workaround to remove the + that occurs in strings, e.g. "+ 1 + x^2" -> "1 + x^2"
handle_first_plus :: String -> String
handle_first_plus s 
    | ((head s) == '+') = (drop 2 s)
    | otherwise = s

-- Adds parenthesis to a string, to be used in the negative expoents e.g "x^-2" -> "x^(-2)"
add_parenthesis :: String -> String
add_parenthesis s = "(" ++ s ++ ")"

-- Parses a lists of variables and returns a string, e.g. [(x,2), (y,3)] -> "x^2y^3"
parse_variables :: [Var] -> String
parse_variables [] = ""
parse_variables ((a, b):xs)
    | (b == 1) = a : parse_variables xs
    | (isNegative b) = a : '^' : add_parenthesis (show b) ++ parse_variables xs
    | otherwise = a : '^' : show b ++ parse_variables xs

-- Parses a monomial and returns a string, e.g. (2, [(x,2), (y,3)]) -> "2x^2y^3"
reverseMono :: Mono -> String
reverseMono (a, b) 
    | (a == 1 && b == []) = "+ 1" ++ parse_variables b
    | (a == -1 && b == []) = "- 1" ++ parse_variables b
    | (a == 1) = "+ " ++ parse_variables b
    | (a == -1) = "- " ++ parse_variables b
    | (isNegative a) = "- " ++ show (abs a) ++ parse_variables b
    | otherwise = "+ " ++ show a ++ parse_variables b

-- END REVERSE PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- WrapperFunctions  -----------------------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Converts a String into a Polynomial
parsePoly :: String -> Poly
parsePoly [] = []
parsePoly s = remove_zeros (map (parseMonomial) (remove_plus (remove_mult (replacePattern ((simplify_minus (formatSpace s)))))))

-- Converts a Polynomial back into a String
reverseParser :: Poly -> String
reverseParser l = handle_first_plus (intercalate " " (map (reverseMono) (poly_sorter l)))
