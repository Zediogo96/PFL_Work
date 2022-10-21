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

    putStr("\n#### PARSER TESTING ################### \n\n")

    let p1 = "1 - 1 + 3x^4 - 3x^4 + 12x^(-2) - 44z^(-2)y^4z^2"
    let p2 = "2 - 3x^4 - 5y^4"
    let p3 = "5 + 1 + 2"

    let t1 = parsePoly p1
    let t2 = parsePoly p2
    let t3 = parsePoly p3

    let r1 = reverseParser t1
    let r2 = reverseParser t2
    let r3 = reverseParser t3

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

    putStr("\n#### TESTING SIMPLIFY ################### \n\n")

    let s1 = parsePoly "5 + 3x^4y^4 - 5x^4y^4 - 5 + 3 + x^2 - x"
    let simp1  = simplify s1
    let rev_s_1 = reverseParser simp1
    let expected_rev_s_1 = "- 2x^4y^4 + x^2 - x + 3"

    let s2 = parsePoly "x - 2x - 3x + 3x^2z^(-3) + 3x^2z^(-3) - z + 2z"
    let simp2  = simplify s2
    let rev_s_2 = reverseParser simp2
    let expected_rev_s_2 = "6x^2z^(-3) + z - 4x"
   
    print("Original String: " ++ "5 + 3x^4y^4 - 5x^4y^4 - 5 + 3 + x^2 - x")
    print("Simplified Polynomial: " ++ show simp1)
    print("Reversed Parse: " ++ rev_s_1)
    print("Are the string equal? " ++ strEquals expected_rev_s_1 rev_s_1)

    putStr("\n ----- ----- ----- ----- -----\n\n")

    print("Original String: " ++ "x - 2x - 3x + 3x^2z^(-3) + 3x^2z^(-3) - z + 2z")
    print("Parsed Polynomial: " ++ show s2)
    print("Simplified Polynomial: " ++ show simp2)
    print("Reversed Parse: " ++ rev_s_2)
    print("Are the string equal? " ++ strEquals expected_rev_s_2 rev_s_2)

    putStr("\n#### TESTING ADDITION ################### \n\n")

    let a1 = parsePoly "1 + 2x^2 + 3x^3 - 2 + yz"
    let a2 = parsePoly "1 + 2x^2 + 3x^3 - 2 + 3yz"
    let add1 = addPoly a1 a2 
    let rev_add1 = reverseParser add1
    let exp_rev_add_1 = "6x^3 + 4x^2 + 4yz - 2"
    
    let a3 = parsePoly "yz"
    let a4 = parsePoly "3yz"
    let add2 = addPoly a3 a4
    let rev_add2 = reverseParser add2
    let exp_rev_add_2 = "4yz"

    print("Original Poly 1: " ++ show a1)
    print("Original Poly 2: " ++ show a2)
    print("Added Polynomials: " ++ show add1)
    print("Reversed Parse of Add: " ++ rev_add1)
    print("Are the string equal? " ++ strEquals expected_rev_s_2 rev_s_2)

    putStr("\n ----- ----- ----- ----- -----\n\n")

    print("Original Poly 1: " ++ show a3)
    print("Original Poly 2: " ++ show a4)
    print("Added Polynomials: " ++ show add2)
    print("Reversed Parse of Add: " ++ rev_add2)
    print("Are the string equal? " ++ strEquals exp_rev_add_2 rev_add2)

    putStr("\n#### TESTING MULTIPLY ################### \n\n")

    let m1 = parsePoly "4xy + 2"
    let m2 = parsePoly "2x + 3"
    let mul1 = multiplyPoly m1 m2
    let rev_mul1 = reverseParser mul1
    let exp_rev_mul_1 = "8x^2y + 12xy + 4x + 6"

    print("Original Poly 1: " ++ show m1)
    print("Original Poly 2: " ++ show m2)
    print("Multiplied Polynomials: " ++ show mul1)
    print("Reversed Parse of Multiply: " ++ rev_mul1)
    print("Are the string equal? " ++ strEquals exp_rev_mul_1 rev_mul1)

    putStr("\n ----- ----- ----- ----- -----\n\n")

    let m3 = parsePoly "x + xy"
    let m4 = parsePoly "x + xy"
    let mul2 = multiplyPoly m3 m4
    let rev_mul2 = reverseParser mul2
    let exp_rev_mul_2 = "2x^2y + x^2 + x^2y^2"

    print("Original Poly 1: " ++ show m3)
    print("Original Poly 2: " ++ show m4)
    print("Multiplied Polynomials: " ++ show mul2)
    print("Reversed Parse of Multiply: " ++ rev_mul2)
    print("Are the string equal? " ++ strEquals exp_rev_mul_2 rev_mul2)

    putStr("\n#### TESTING DERIVE ################### \n\n")

    let d1 = parsePoly "2x + 2x^(-2)y"
    let deriv1 = derive d1 'x'
    let rev_deriv1 = reverseParser deriv1
    let exp_rev_deriv1 = "- 4x^(-3)y + 2"

    let d2 = parsePoly "2x + 2x"
    let deriv2 = derive d2 'x'
    let rev_deriv2 = reverseParser deriv2
    let exp_rev_deriv2 = "4"


    print("Original Poly: " ++ show d1)
    print("Derived Polynomials: " ++ show deriv1)
    print("Reversed Parse of Derive: " ++ rev_deriv1)
    print("Are the string equal? " ++ strEquals exp_rev_deriv1 rev_deriv1)

    putStr("\n ----- ----- ----- ----- -----\n\n")

    print("Original Poly: " ++ show d2)
    print("Derived Polynomials: " ++ show deriv2)
    print("Reversed Parse of Derive: " ++ rev_deriv2)
    print("Are the string equal? " ++ strEquals exp_rev_deriv2 rev_deriv2)

