import Data.List
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

-- 1.6 

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c 
    | b_desc >= 0 = ((-b + sqrt(b_desc)) / (2 * a), (-b - sqrt(b_desc)) / (2 * a)) 
    | otherwise = error "negative delta"
    where b_desc = (b^2 - 4*a*c)

-- 1.7
    -- (a) String ou [Char], pois em haskell uma string é uma lista de chars
    -- Testado com print("na" == ['n', 'a'])
    -- (b) Tuplo de Char
    -- (c) Lista de Tuplos
    -- (d) Tuplo de Listas
    -- (e) ? ? ? ? ? ? ? ?
    -- (f) ? ? ? ? ? ? ? ? 

-- 1.8 
--(a) Integer
segundo :: [a] -> a
segundo xs = head (tail xs)
--(b)
trocar :: (a,b) -> (b,a)
trocar (x, y) = (y, x)
--(c)
par :: a -> b -> (a,b)
par x y = (x,y)
--(d)
dobro :: Int -> Int
dobro x = 2 * x
--(e)
metade :: Float -> Float
metade x = x / 2
--(f)
minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'
-- (g) 
intervalo :: Int -> Int -> Int -> Bool
intervalo x a b = x >= a && x <= b
--(h)
palindromo :: String -> Bool -- ou palindromo :: [Char] -> Bool
palindromo xs = reverse xs == xs
--(i) ?????????????

-- 1.9 

classifica :: Int -> String
classifica a
    | (a <= 9) = "reprovado"
    | (a >= 10 && a <= 12) = "suficiente"
    | (a >= 13 && a <= 15) = "bom"
    | (a >= 16 && a <= 18) = "muito bom"
    | otherwise = "muito com com distinção"

-- 1.10

-- Peso em KG e altura em m
classifica_IMC :: Float -> Float -> String
classifica_IMC a b
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = (a / (b^2))

-- 1.11

min' :: Ord a => a -> a -> a -> a
min' x y = min a
        where a = min x y


max' :: Ord a => a -> a -> a -> a
max' x y = max a
        where a = max x y

-- 1.12
xor :: Bool -> Bool -> Bool
xor a b 
    | (a == b) = False
    | otherwise = True
-- ou
-- xor a b = a /= b

-- 1.13

safetail :: [a] -> [a]
safetail [] = []
safetail lst = drop 1 lst

-- 1.14

curta :: [a] -> Bool 
curta lst = (length lst <= 2)

curta' :: [a] -> Bool
curta' [] = True
curta' [_] = True
curta' [_, _] = True
curta' _ = False

-- 1.15

-- (a)
mediana :: Ord a => a -> a -> a -> a
mediana a b c
    | a <= b && b <= c = b
    | c <= b && b <= a = b
    | b <= a && a <= c = a
    | c <= a && a <= b = a
    | a <= c && c <= b = c
    | b <= c && c <= a = c
    | otherwise = error "mediana"

-- (b) 
-- Oneliner um bocado nasty mas funfa
mediana' :: (Num a, Ord a) => a -> a -> a -> a
mediana' a b c = (a + b + c) - (max a (max b c)) - (min a (min b c))

-- 1.16


main :: IO()
main = do
{-     print(metade 12.3)
    print(minuscula 'e')
    print(intervalo 13 3 12)
    print(palindromo "abba") -}

    print(mediana 2 3 (-1))
    print(mediana' 2 3 (-1))


