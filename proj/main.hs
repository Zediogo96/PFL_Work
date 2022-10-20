import PolyCalc
import PolyParser

import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)

main :: IO() 
main = do

    putStr("\n-- TESTING PARSER -----------------\n\n")
    let t1 = parsePoly "-1 + 3x^4"
    let t2 = parsePoly "2 - 3x^4 - 5y^4"
    let t3 = parsePoly "5 + 1 + 2"
    
    print(t1)
    print(t2)
    print(t3)

    putStr("\n-- REVERSE PARSER -----------------\n\n")
    let r1 = reverse_parser t1
    let r2 = reverse_parser t2
    let r3 = reverse_parser t3

    print(r1)
    print(r2)
    print(r3)

    putStr("\n-- TESTING SIMPLIFY -----------------\n\n")
    let t2 = parsePoly "5 + 3x^4 - 5y^4"
    print(simplify t2)

    putStr("\n-- TESTING ADDITION -----------------\n\n")
    let a4 = parsePoly "2 - 3x^4"
    let a5 = parsePoly "-1 + 3x^4"

    print(addPoly a4 a5)
    print(reverse_parser (addPoly a4 a5))
 
    putStr("\n-- TESTING MULTIPLY -----------------\n\n")
    let m1 = multiply_Poly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]
    let m2 = reverse_parser m1
    print(m2)

    putStr("\n-- TESTING DERIVE -----------------\n\n")
    let d1 = remove_exp_zero(derive [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',5)])] 'x')
    let d2 = reverse_parser d1
    print(d2)
