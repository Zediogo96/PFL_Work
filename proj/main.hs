import PolyCalc
import PolyParser

main :: IO() 
main = do

    {- let s1 = parse_poly "0x^2"
    let s2 = remove_zeros s1
    print(s1)
    let s3 = remove_empty [(' ',1),('y',1),('z',3)]
    print(s3) -}
    {- let s4 = w_remove_empty (5,[('x',1),('y',1),('z',3))
    print(s4) -}

    let t1 = [(5,[('x',1),('y',1),('z',3)])]
    print(wrapRemEmpty [(5,[(' ',1),('y',1),('z',3)]), (5,[(' ',1),(' ',1),(' ',3)])])
    



    {- let k1 = multiply_Poly [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])] [(2,[('x',3),('y',1)]), (1,[('x',3)])]

    let k2 = wrap_reverse_parser (simplify' k1)
    print(wrap_reverse_parser [(5,[('x',1),('y',1),('z',3)]), (5,[('x',1),('y',1),('z',3)])])
    print(wrap_reverse_parser [(2,[('x',3),('y',1)]), (1,[('x',3)])])
    print(k2)
    putStr("\n-- MULTIPLY -----------------\n\n")
    print(multiply_monoid (5, [('x', 1),('y', 4), ('z', 5)]) (5, [('x', 1),('y', 4)])) -}