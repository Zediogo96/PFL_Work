import PolyCalc
import PolyParser

import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)
import Data.List(partition)

-- check if two strings are equal
strEquals :: String -> String -> String
strEquals [] [] = "TRUE"
strEquals (x:xs) (y:ys) = if x == y then strEquals xs ys else "FALSE"
strEquals _ _ = "FALSE"


main :: IO() 
main = do

    -- PARSER TESTING ------------------------------------------- 
    let p1 = "1 - 1 + 3x^4 - 3x^4 + 12x^(-2) - 44z^(-2)y^4z^2"
    let p2 = "2 - 3x^4 - 5y^4"
    let p3 = "5 + 1 + 2"

    let t1 = parsePoly p1
    let t2 = parsePoly p2
    let t3 = parsePoly p3

    let r1 = reverseParser t1
    let r2 = reverseParser t2
    let r3 = reverseParser t3

    -- SIMPLIFY TESTING ------------------------------------------- 

    let s1 = parsePoly "5 + 3x^4y^4 - 5x^4y^4 - 5 + 3 + x^2 - x"
    let simp1  = simplify s1
    let rev_s_1 = reverseParser simp1
    let expected_rev_s_1 = "- 2x^4y^4 + x^2 - x + 3"

    let s2 = parsePoly "x - 2x - 3x + 3x^2z^(-3) + 3x^2z^(-3) - z + 2z"
    let simp2  = simplify s2
    let rev_s_2 = reverseParser simp2
    let expected_rev_s_2 = "6x^2z^(-3) + z - 4x"

    -- ADDITION TESTING ------------------------------------------- 

    let a1 = parsePoly "1 + 2x^2 + 3x^3 - 2 + yz"
    let a2 = parsePoly "1 + 2x^2 + 3x^3 - 2 + 3yz"

    let add1 = addPoly a1 a2

    let rev_add_1 = reverseParser add1
    
    









      -- PARSER TESTING ------------------------------------------- 

    putStr("\n#### PARSER TESTING ################### \n\n")

    print("Original String: " ++ p1)
    print("Parsed Polynomial: " ++ show t1)
    print("Reversed Parse: " ++ r1)
    print("Are the string equal? " ++ strEquals p1 r1)

    putStr("\n ----- ----- ----- ----- -----\n\n")
    print("Original String: " ++ p2)
    print("Parsed Polynomial: " ++ show t2)
    print("Reversed Parse: " ++ r2)
    print("Are the string equal? " ++ strEquals p2 r2)

    putStr("\n ----- ----- ----- ----- -----\n\n")
    print("Original String: " ++ p3)
    print("Parsed Polynomial: " ++ show t3)
    print("Reversed Parse: " ++ r3)
    print("Are the string equal? " ++ strEquals p3 r3)

    putStr("\n#### SIMPLIFY TESTING ################### \n\n")
    
    print("Original String: " ++ "5 + 3x^4y^4 - 5x^4y^4")
    print("Parsed Polynomial: " ++ show s1)
    print("Simplified Polynomial: " ++ show simp1)
    print("Reversed Parse: " ++ rev_s_1)
    print("Are the string equal? " ++ strEquals expected_rev_s_1 rev_s_1)

    putStr("\n ----- ----- ----- ----- -----\n\n")

    print("Original String: " ++ "x - 2x - 3x + 3x^2z^(-3) + 3x^2z^(-3) - z + 2z")
    print("Parsed Polynomial: " ++ show s2)
    print("Simplified Polynomial: " ++ show simp2)
    print("Reversed Parse: " ++ rev_s_2)
    print("Are the string equal? " ++ strEquals expected_rev_s_2 rev_s_2)

    putStr("\n-- TESTING ADDITION -----------------\n\n")
    print(rev_add_1)
 
    {- putStr("\n-- TESTING MULTIPLY -----------------\n\n")
    let m1 = multiplyPoly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]
    let m2 = reverseParser m1
    print(m2)

    putStr("\n-- TESTING DERIVE -----------------\n\n")
    let d1 = parsePoly "2x^2 + 4x^(-3)y^4 + 1"
    let d2 = derive d1 'x'
    let d3 = reverseParser d2
    print(d3) -}