import Data.Char (isSpace, isLetter)
import Data.List

-- TÁ A VOLTARIIIII
{- 
split :: String -> [String]
split "" = []
split str 
    |
    start : (split (drop 1 rest))
    where (start, rest) = (break (isOperand) str)
-}

-- Helper functions, to better allows to use "truples" to represent our monomials
-- ghc STL only supports tuples

fst' (a,_,_) = a
snd' (_,a,_) = a
third' (_,_,a) = a

-- Easy way to check if an element is an operand

isOperand :: Char -> Bool
isOperand c = c `elem` ['+', '-']

hasMult :: String -> Integer
hasMult l 
    | (length l == 0 ) = 1
    | (l == "-") = 0 - read (drop 1 l ):: Integer
    | otherwise = read l :: Integer

hasVar :: String -> Char
hasVar l 
    | (l == []) = ' '
    | (isLetter (head l)) = (head l)

hasExp :: String -> Integer
hasExp l
    | (l == []) = 1
    | otherwise = read (drop 1 l) :: Integer 

-- Slipts lists by chosen Char, only used with '+' in this project
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

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
remove_mult :: [String] -> [[String]]
remove_mult [] = []
remove_mult (x:xs) =  (remove_power (split '*' x)) : remove_mult xs

-- Function used to separate a variable that has an power. This translates ["y^2] to [["y", "2"]] 
remove_power :: [String] -> [String]
remove_power  [] = []
remove_power (x:xs) = (split '^' x) ++ remove_power xs

-- Wrapper function for all the functions necessary to the parser
parse_poly :: String -> [[String]]
parse_poly [] = [[[]]]
parse_poly s = remove_mult (remove_plus (simplify_minus (formatSpace s)))

main :: IO() 
main = do

    putStr("\nRANDOM TESTING ON THE WAE\n")

    putStr("\n Poly 1: 5*x^3 - 10*y^4 - 5z^5 - x^2 - 5 - x\n")
    print(parse_poly "5xyk^3 + 10y^4 + 5z^5 - x^2 - 5 + x")

    putStr("\n Poly 2: 5x*y*z^3 + 10*y^4 + 5z^5 - x^2 - 5 + x\n")
    print(parse_poly "5xyk^3 + 10y^4 + 5z^5 - x^2 - 5 + x")
