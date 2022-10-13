import Data.Char (isSpace, isLetter, isDigit,)
import Data.List

-- Auxiliar Functions
fst' (a,_,_) = a
snd' (_,a,_) = a
third' (_,_,a) = a

isOperand :: Char -> Bool
isOperand c = c `elem` ['+', '-']

split :: String -> [String]
split "" = []
split str 
    |
    start : (split (drop 1 rest))
    where (start, rest) = (break (isOperand) str)
        --   signal = whatSignal (head rest)

formatSpace :: [String] -> [String]
formatSpace = map (filter (not . isSpace))

hasMult :: String -> Integer
hasMult l 
    | (l == [] ) = 1
    | (all isDigit l) = read l :: Integer

hasVar :: String -> Char
hasVar l 
    | (l == []) = ' '
    | (isLetter (head l)) = (head l)

hasExp :: String -> Integer
hasExp l
    | (l == []) = 1
    | (all isDigit (drop 1 l)) = read (drop 1 l) :: Integer

whatSignal :: Char -> Char
whatSignal c 
    | c == '-' = '-'
    | c == '+' = ' '

teste :: [String] -> [(Integer, Char, Integer)]
teste [] = []
teste (x:xs) = (hasMult primeiro, hasVar segundo, hasExp resto2) : teste xs
    where (primeiro, resto1) = break (isLetter) x
          (segundo, resto2) = break (=='^') resto1

main :: IO() 
main = do

    let s1 = "5x^3 + 10y^4 + 5z^5 + x^2 + 5"
    let s2 = formatSpace(split s1)
    let s3 = teste s2

    putStr("\nBefore Parse:  ")
    print(s1)
    putStr("\nFormat Space:")
    print(s2)
    {- putStr("After Parse:   ")
    print(s3) -}
    
    putStr("\n------------------------\n")

    {- 
    let s4 = "5x^3 + 10y^4 - 10 - x^5"
    let s5 = formatSpace (split s4)
    
    putStr("\nBefore Parse:  ")
    print(s4)
    putStr("After Parse:   ")
    print(s5) -}

    {- print(break (isOperand) "5x^3 + 3y^2 - 1")
    print(span (isOperand) "+ 3y^2 - 1")

    let l1 = ["5x^3"," ", "3y^2","-", "1"]
    let t1 = tst l2
    print(l2)
    print(fst t1)


    putStr("----------------------------------------")
    let s1 = "5x^3 + 6x^4"
    let s2 = teste 
    let s3 = formatSpace s2
    
    print("Before:")
    print(s1)
    print("Tokenize:")
    print(s2)
    -}