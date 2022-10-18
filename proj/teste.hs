import Data.Char (isSpace, isLetter, isAlpha, isDigit)

parseMonomial :: String -> (Integer, [(Char, Integer)])
parseMonomial s = case break isAlpha s of 
        ([], varPows) -> (1, parseUnitMonomial varPows)
        (coef, varPows) -> (parseCoef coef, parseUnitMonomial varPows) 


parseCoef :: String -> Integer
parseCoef [] = 1
parseCoef s 
        | ((head s == '-')) = negate (read (drop 1 s) :: Integer)
        | otherwise = read s :: Integer 

handleSmth :: String -> Integer
handleSmth s
        | s == [] = 1
        | otherwise = read s :: Integer

cheeky_zipper :: String -> [(Char, Integer)]
cheeky_zipper l = (zip (take (length l) l) (repeat 1))

read_Int :: String -> Integer
read_Int s = read s :: Integer 

parseUnitMonomial :: String -> [(Char, Integer)]
parseUnitMonomial [] = []
parseUnitMonomial l 
        | (l == []) = []
        | (all isAlpha l) = cheeky_zipper vars
        | (length (vars) > 1) = (cheeky_zipper vars) ++ [(last vars, read_Int (takeWhile (isDigit) fx))] ++ parseUnitMonomial rest
        | otherwise = [(head vars, handleSmth (takeWhile (isDigit) (drop 1 pows)))] ++ parseUnitMonomial rest
        where 
        (vars, pows) = span isAlpha l
        (fx, rest) = break isAlpha pows

main :: IO() 
main = do
    putStr("\n-- TESTING ----------------------------------------\n")
--     print(break isAlpha "-5yz^2")

    putStr("\n-- RANDOM TESTING ON THE WAE ---------------------\n")
    let s1 = "5y^3x^2z^4k"
    let s2 = "5xyz^3"
    print("Original Poly s1", s1)
    print("After parsing s1", parseMonomial s1)
    print("Original Poly s2", s2)
    print("After parsing s2", parseMonomial s2)


    
