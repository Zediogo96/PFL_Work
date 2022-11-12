import PolyCalc
import PolyParser

main :: IO() 
main = do

    putStr("\n--------- MULT 1 -----------------------\n")
    let a1 = parsePoly "x + 2x^2 + 3x^3"
    let a2 = parsePoly "4 + 5x + 6x^2"

    print(reverseParser $ multiplyPoly a1 a2)

    putStr("\n--------- MULT 2 -----------------------\n")

    let a3 = parsePoly "3x^2 - 1"
    let a4  = parsePoly "x^2 - 2x + 1"

    print(reverseParser $ multiplyPoly a3 a4)

    putStr("\n--------- DERIVADA 1 -----------------------\n")

    let a5 = parsePoly "3x^2 + 9"
    print(reverseParser $ derivePoly a5 'x')

    putStr("\n________ DERIVADA 2 -------------------------\n")

    let a6 = parsePoly "18x^5y + 5x + 9"
    print(reverseParser $ derivePoly a6 'x')

    putStr("\n________ DERIVADA 3 -------------------------\n")

    let a6 = parsePoly "18x^5y + 5x + 9"
    print(reverseParser $ derivePoly a6 'y')

    putStr("\n________ EXP NEGATIVO -------------------------\n")

    let a7 = parsePoly "x^(-1) + y"
    let a8 = parsePoly "x^(-2) + y"

    print(reverseParser $ addPoly a7 a8)

    putStr("\n-------------------------\n")
    





    