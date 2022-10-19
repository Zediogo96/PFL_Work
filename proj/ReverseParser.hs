module ReverseParser where

import Data.List

reverse_parser :: (Integer, [(Char, Integer)]) -> String 
reverse_parser (a,b) 
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

wrap_reverse_parser :: [(Integer, [(Char, Integer)])] -> String
wrap_reverse_parser l = handle_first_plus (intercalate " " (map (reverse_parser) l))


{- main :: IO() 
main = do
    putStr("\n---- ----- TESTING OVERLOAD ----- ----\n\n")
    print("Esperado: 5*xyz^3 + 10*xy^4 + 5*z^5 - x^2 - 5 - x - xyz - 14*x^2y^4z^55")
    print("Resultado: " ++ wrap_reverse_parser [(5,[('x',1),('y',1),('z',3)]),(10,[('x',1),('y',4)]),(5,[('z',5)]),(-1,[('x',2)]),(-5,[]),(-1,[('x',1)]),(-1,[('x',1),('y',1),('z',1)]),(-14,[('x',2),('y',4),('z',55)])]) -}