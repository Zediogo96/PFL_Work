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

-- Splits an String by occurrences of '+' and creates a list of those sub-strings
remove_plus :: String -> [String]
remove_plus s =  split '+' s

-- Removes multiplication on substrings
remove_mult :: String -> String
remove_mult l = filter (/='*') l

-- Function used to separate a variable that has an power. This translates ["y^2] to [["y", "2"]] 
remove_power :: [String] -> [String]
remove_power  [] = []
remove_power (x:xs) = (split '^' x) ++ remove_power xs

-- Safer version of the tail funcions
safe_tail :: [a] -> [a]
safe_tail [] = []
safe_tail xs = tail xs

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

acceptReqSymbols :: String -> String
acceptReqSymbols s = filter (isAlpha) s

-- Parses the coefficient, handling negative values and the absence of coefficient
parseCoef :: String -> Integer
parseCoef [] = 1
parseCoef s 
        | (head s == '-' && (length s) == 1) = -1
        | (head s == '-' && (length s) > 1) = negate (read (tail s) :: Integer)
        | otherwise = read s :: Integer

-- parse y^4x to ["y^4", "x"]
parseMonomialVars :: String -> [Var]
parseMonomialVars [] = []
parseMonomialVars s = [(safe_head var, extractExp tst)] ++ parseMonomialVars rest
    where 
        (var, rem) = (takeWhile (isAlpha) s, dropWhile (isAlpha) s)
        (tst, rest) = ((takeWhile (\n -> (isDigit n) || ((`elem` "-()^")) n) rem), (dropWhile(\n -> (isDigit n) || ((`elem` "-()^")) n) rem))

-- By using simplify minus, with the case of negative expoent (-2), would end up like (+-2), and that was not initially intended, this works it around
filterPattern :: String -> String
filterPattern s = filter (not . (`elem` "(+-)")) s

-- Separates coefficient of the monomial and then parses the rest of the variables using parseUnitMonomial
parseMonomial :: String -> (Integer, [(Char, Integer)])
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (1, parseMonomialVars s)
        (a, []) -> (read a :: Integer, [])
        (coef, varPows) -> (parseCoef coef, parseMonomialVars varPows) 

parsePoly :: String -> Poly
parsePoly [] = []
parsePoly s = remove_zeros (map (parseMonomial) (remove_plus (remove_mult (filterPattern ((simplify_minus (formatSpace s)))))))

main :: IO() 
main = do

    putStrLn "########## RANDOM TESTING #############"
    let t1 = "x^(-2)y^3"
    let t2 = "x^(2)y^3"
    let (var, rem) = (takeWhile (isAlpha) t1, dropWhile (isAlpha) t1)
    let (tst, rest) = ((takeWhile (\n -> (isDigit n) || ((`elem` "-()^")) n) rem), (dropWhile(\n -> (isDigit n) || ((`elem` "-()^")) n) rem))

    print(var, rem)
    print(tst, rest)

    putStrLn "########## SERIOUS TESTING #############"

    print(parsePoly "x^2y^3")
    print(parsePoly "x^(-2)y^3")


