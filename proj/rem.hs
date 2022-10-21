import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)
import Data.List

type Var = (Char, Integer)
type Mono = (Integer, [Var])
type Poly = [Mono]


-- NORMAL PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

-- Wrapper function just to make the process of reading an integer less verbose on another functions
read_Int :: String -> Integer
read_Int s = read s :: Integer 

-- Slipts lists by chosen Char, only used with '+' in this project
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

-- Auxiliar function to remove arguments that their first element is equal to zero
remove_zeros :: Poly -> Poly
remove_zeros xs = [c | c <- xs, (fst c /= 0)]

-- Remove all spaces from a string, for easier parsing
formatSpace :: String -> String
formatSpace = filter (not . isSpace)

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

-- Separates coefficient of the monomial and then parses the rest of the variables using parseUnitMonomial
parseMonomial :: String -> (Integer, [(Char, Integer)])
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (0, parseUnitMonomial varPows)
        (coef, varPows) -> (parseCoef coef, parseUnitMonomial varPows) 

-- Parses the coefficient, handling negative values and the absence of coefficient
parseCoef :: String -> Integer
parseCoef [] = 1
parseCoef s 
        | (head s == '-') = -1
        | (head s == '-' && (length s) > 1) = negate (read (tail s) :: Integer)
        | otherwise = read s :: Integer 

-- Parses the variables and their powers, handling the absence of power zipping every variable with 1
cheeky_zipper :: (String, String) -> [(Char, Integer)]
cheeky_zipper (l,p) 
    | (p == "") =  zip l (repeat 1)
    | ((length l) == 1) = [(head l, 1)]
    | otherwise = zip (pop l) (repeat 1)

-- Handles cases for parseUnitMonomial where reading expoents returns an empty string, returns 1
handleSmth :: String -> Integer
handleSmth s
        | s == [] = 1
        | otherwise = read s :: Integer

-- Safer version of the tail funcions
safe_tail :: [a] -> [a]
safe_tail [] = []
safe_tail xs = tail xs


parseUnitMonomial :: String -> [Var]
parseUnitMonomial [] = []
parseUnitMonomial l 
        | (l == []) = []
        | (all isAlpha l && (length l > 1)) = cheeky_zipper (vars,pows)
        | (length (vars) > 1) = (cheeky_zipper (vars,pows)) ++ [(last vars, read_Int (takeWhile (isDigit) (tail fx)))] ++ parseUnitMonomial rest
        | otherwise = [(head vars, handleSmth (takeWhile (isDigit) (safe_tail pows)))] ++ parseUnitMonomial rest
        where 
        (vars, pows) = span isAlpha l
        (fx, rest) = break isAlpha pows

-- Wrapper function for all the functions necessary to the parser
parsePoly :: String -> Poly
parsePoly [] = []
parsePoly s = remove_zeros (map (parseMonomial) (remove_mult (remove_plus (simplify_minus (formatSpace s)))))

-- REVERSE PARSER -----------------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

parse_variables :: [(Char, Integer)] -> String
parse_variables [] = ""
parse_variables (x:xs) = [(fst x)] ++ parse_var (snd x) ++ parse_variables xs

-- Handles the parsing of a Monoid with extra care to present it nicely in a String
reverse_parser :: Mono -> String 
reverse_parser (a,b) 
    | (a == 1)  = "+ 1" ++ parse_variables b
    | (a == 1 && b /= [])  = "+ " ++ parse_variables b
    | (a == -1) = "- 1" ++ parse_variables b
    | (a == -1 && b /= []) = "- " ++ parse_variables b
    | (a < 0)   = "- " ++ (show (abs a)) ++ parse_variables b
    | otherwise = "+ " ++ (show a) ++ parse_variables b
       
parse_var :: Integer -> String
parse_var i
    | i == 1 = ""
    | i == -1 = "-"
    | otherwise = "^" ++ show i

handle_first_plus :: String -> String
handle_first_plus s 
    | ((head s) == '+') = (drop 2 s)
    | otherwise = s

wrap_reverse_parser :: [Mono] -> String
wrap_reverse_parser l = handle_first_plus (intercalate " " (map (reverse_parser) l))