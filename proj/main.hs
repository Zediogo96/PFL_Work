import PolyCalc
import PolyParser

main :: IO() 
main = do

    putStr("\n-- TESTING SIMPLIFY -----------------\n\n")
    let s1 = simplify [(5,[('x',1),('y',1),('z',3)]), (5,[('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])]
    let s2 = reverse_parser s1
    print(s2)

    putStr("\n-- TESTING ADDITION -----------------\n\n")
    let a1 = addPoly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]
    let a2 = reverse_parser a1
    print(a2)

    putStr("\n-- TESTING MULTIPLY -----------------\n\n")
    let m1 = multiply_Poly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]
    let m2 = reverse_parser m1
    print(m2)

    putStr("\n-- TESTING DERIVE -----------------\n\n")
    let d1 = derive [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] 'x'
    let d2 = reverse_parser d1
    print(d2)




    {- 

    let k2 = wrap_reverse_parser (simplify' k1)
    print(wrap_reverse_parser [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])])
    print(wrap_reverse_parser [(2,[('x',3),('y',1)]), (1,[('x',3)])])
    print(k2)
    
    print(multiply_monoid (5, [('x', 1),('y', 4), ('z', 5)]) (5, [('x', 1),('y', 4)])) -}