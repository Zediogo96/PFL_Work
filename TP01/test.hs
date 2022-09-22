-- 1.1

-- Only the last argument (Bool) refers to the return type of the function, the other 3 are function parameters

testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = a < b + c && b < a + c && c < b + a


-- 1.2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt(s * (s - a) * (s - b) * (s - c))
                        where s = (a + b + c)/2

-- 1.3

-- TAKE: reates a list, the first argument determines, how many items should be taken from the list passed as the second argument
-- DROP: its the same as take, but removes the elements up to first argument index from the list passed as second argument and returns the others
metades :: [a1] -> ([a1], [a1])
metades a1 = (take s a1, drop s a1)
    where s = (length a1) `div` 2

-- 1.4

-- Reverse têm de retornar uma lista, daí o argumento ter de ser lista mesmo sendo só um valor

-- (a)
last' :: [a] -> [a]
last' a = take 1 (reverse a)
last'' :: [a] -> a
last'' a = head (reverse a)
last''' :: [a] -> a
last''' a = head(drop (length a - 1) a)

-- (b)
init' :: [a] -> [a]
init' a = reverse (drop 1 (reverse a))
init'' :: [a] -> [a]
init'' a = take (length a - 1) a 

-- 1.5

-- (a) 

-- Maneira recursiva de efectuar o loop
factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

binom :: Integer -> Integer -> Integer
binom n k = (factorial n) `div` ((factorial k) * factorial (n - k))

-- Sem factorial
{- binom'' :: Integer -> Integer -> Integer
binom'' n k = (product [1..n]) `div` ((product [1..k]) * product [1..(n-k)]) -}

-- (b) 

-- Os ifs/else são representados por | e pela keyword otherwise
binom' :: Integer -> Integer -> Integer 
binom' n k 
    | k < (n - k) = (product [(n-k+1)..n]) `div` product[1..k]
    | otherwise = (product [(k+1)..n]) `div` product[1..(n-k)]



main :: IO()
main = do
    print(binom' 10 2);

-- 1.6 

