-- 1.1

-- Only the last argument (Bool) refers to the return type of the function, the other 3 are function parameters

{- testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a < b + c && b < a + c && c < b + a

main = do print(testaTriangulo 1 2 3) -}

-- 1.2

{- areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt(s * (s - a) * (s - b) * (s - c))
                        where s = (a + b + c)/2 -}

-- 1.3

-- TAKE: reates a list, the first argument determines, how many items should be taken from the list passed as the second argument
-- DROP: its the same as take, but removes the elements up to first argument index from the list passed as second argument and returns the others
metades :: [a1] -> ([a1], [a1])
metades a1 = (take s a1, drop s a1)
    where s = (length a1) `div` 2

main :: IO()
main = do

    print(metades [1..20])
