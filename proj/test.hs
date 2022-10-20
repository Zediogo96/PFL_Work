import PolyCalc
import PolyParser

import Data.Char (isSpace, isLetter, isAlpha, isDigit, isSymbol)

main :: IO() 
main = do

    -- takeWhile is digit or -
    let a = takeWhile (\x -> isDigit x || x == '-' || x == ')' || x == '(') "x^(-2)"
    print(a)


    let h1 = parsePoly "-4x^-3 + 6x^2 + 2x"

    let h2 = [(3,[('x',-4)]),(-1,[('y',-4)]),(5,[])]
    print (wrap_reverse_parser (addPoly h2 h2))


    putStr("\n-- TESTING PARSER -----------------\n\n")
    let t1 = parsePoly "-1 + 3x^4 - 1"
    let t2 = parsePoly "2 - 3x^4 - 5y^4"
    let t3 = parsePoly "5 + 1 + 2"
    
    print(t1)
    print(t2)
    print(t3)

    putStr("\n-- REVERSE PARSER -----------------\n\n")
    let r1 = wrap_reverse_parser t1
    let r2 = wrap_reverse_parser t2
    let r3 = wrap_reverse_parser t3

    print(r1)
    print(r2)
    print(r3)

    putStr("\n-- TESTING SIMPLIFY -----------------\n\n")
    let t2 = parsePoly "5 + 3x^4 - 5y^4"
    print(simplify t2)

    print(wrap_reverse_parser (simplify (parsePoly "5x^2 + 5x^2 + y^3")))
    

    putStr("\n-- TESTING ADDITION -----------------\n\n")
    let a4 = parsePoly "2 - 3x^4"
    let a5 = parsePoly "-1 + 3x^4"

    print(a4)
    print(a5)

    print(addPoly a4 a5)
    print(wrap_reverse_parser (addPoly a4 a5))
 
    putStr("\n-- TESTING MULTIPLY -----------------\n\n")
    let m1 = multiply_Poly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]
    let m2 = wrap_reverse_parser m1
    print(m2)

    putStr("\n-- TESTING DERIVE -----------------\n\n")
    let d1 = parsePoly "2x^2"
    let d2 = derive d1 'x'
    let d3 = wrap_reverse_parser d2
    print(d3)
