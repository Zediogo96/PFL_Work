import PolyCalc
import PolyParser

normalize = do 
    putStrLn "\nNormalize a Polynomial\nPlease input a polynomial of your choice:\n"
    user_polystring <- getLine
    let ret = reverse_parser(simplify(parsePoly (user_polystring)))
    putStrLn ("Your simplified polynomial is: \n" ++ ret ++ "\n" ++ "Type 0 to return to the menu \nPress any other key to try again ")  
    user_choice <- getLine
    if user_choice == "0" then main else normalize

derivate = do 
    putStrLn "\nDerivate a Polynomial\nPlease input a polynomial of your choice:\n"
    user_polystring <- getLine
    putStrLn "\nInput the variable you wish to derivate:\n"
    user_derivar <- getLine
    let ret = reverse_parser(derive (parsePoly (user_polystring)) (head user_derivar))
    putStrLn ("\nThe derivate of your polynomial in order to " ++ user_derivar ++ " is: " ++ ret ++ "\n" ++ "Type 0 to return to the menu \nPress any other key to try again") 
    user_choice <- getLine
    if user_choice == "0" then main else derivate

add = do 
    putStrLn "\nAdd Polynomials\nPlease input a polynomial of your choice:\n"
    user_polystring1 <- getLine
    putStrLn "\nPlease input a polynomial of your choice:\n"
    user_polystring2 <- getLine
    let ret = reverse_parser (addPoly (parsePoly (user_polystring1)) (parsePoly (user_polystring2))) 
    putStrLn ("\nThe sum of your polynomials is: " ++ ret ++ " \n" ++ "Type 0 to return to the menu \nPress any other key to try again ")
    user_choice <- getLine
    if user_choice == "0" then main else add


multiply = do 
    putStrLn "\nMultiply Polynomials\nPlease input a polynomial of your choice:\n"
    user_polystring1 <- getLine
    putStrLn "Please input a polynomial of your choice:\n"
    user_polystring2 <- getLine
    let ret = reverse_parser (multiply_Poly (parsePoly (user_polystring1)) (parsePoly (user_polystring2)))
    putStrLn ("The product of your polynomials is: " ++ ret ++ " \n" ++ "Type 0 to return to the menu \nPress any other key to try again ")
    user_choice <- getLine
    if user_choice == "0" then main else multiply
    

main :: IO() 
main = do

    putStrLn "\nWelcome to Haskell Polynomials!\n"
    putStrLn "What would you like to do?\n \n Simplify a polynomial - Type \"normalize\"\n Add polynomials - Type \"add\"\n Multiply polynomials - Type \"multiply\"\n Derivate a polynomial - Type \"derivate\"\n"
