import PolyCalc
import PolyParser

main :: IO() 
main = do

    -- ASK USER FOR FIRST POLY
    -- ASK USER FOR SECOND POLY

    let k1 = multiply_Poly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]

    let k2 = wrap_reverse_parser (simplify' k1)
    print(wrap_reverse_parser [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])])
    print(wrap_reverse_parser [(2,[('x',3),('y',1)]), (1,[('x',3)])])
    print(k2)
    putStr("\n-- MULTIPLY -----------------\n\n")
    print(multiply_monoid (5, [('x', 1),('y', 4), ('z', 5)]) (5, [('x', 1),('y', 4)]))