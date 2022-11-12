-- 1.1
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = (a < b + c) && (b < a + c) && (c < a + b)

-- 1.2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c 
    | (testaTriangulo a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
    | otherwise = error "Values entered do not form a Triangle"
    where s = (a + b + c) / 2

-- 1.3

metades :: [a] -> ([a], [a])
metades l = (take h l, drop h l)
    where h = length l `div` 2

-- 1.4

last' :: [a] -> a
last' l = head (reverse l)

last'' :: [a] -> a
last'' l = head (drop (length l - 1) l)

init' :: [a] -> [a]
init' l = reverse (tail (reverse l))

init'' :: [a] -> [a]
init'' l = take (length l - 1) l 

-- 1.6
raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c 
    | b_desc >= 0 = ((-b + sqrt(b_desc)) / (2 * a), (-b - sqrt(b_desc)) / (2 * a)) 
    | otherwise = error "negative delta"
    where b_desc = (b^2 - 4*a*c)

-- 1.8
segundo :: [a] -> a
segundo xs = head (tail xs)

trocar :: (a,a) -> (a,a)
trocar (x,y) = (y,x)

par :: a -> a -> (a,a)
par a b = (a,b)

dobro :: (Num a) => a -> a
dobro x = 2 * x

metade :: (Fractional a) => a -> a
metade x = x / 2

minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

intervalo :: (Ord a) => a -> a -> a -> Bool
intervalo x a b = x >= a && x <= b

palindromo :: String -> Bool
palindromo xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 1.10

classifica :: Float -> Float -> String
classifica p h
    | imc < 18.5 = "baixo peso"
    | imc < 25 = "peso normal"
    | imc < 30 = "excesso de peso"
    | otherwise = "obesidade"
    where imc = p / (h^2)


-- 1.12

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (b && not a)

-- 1.13

safetail :: [a] -> [a]
safetail [] = []
safetail l = tail l

-- 1.14

curta :: [a] -> Bool
curta l = length l <= 3

curta' :: [a] -> Bool
curta' [] = True
curta' [_] = True
curta' [_,_] = True
curta' [_,_,_] = True
curta' _ = False

main :: IO()
main = do
    -- 1.12
    print(xor False True)
    print(xor False False)
    print(xor True True)
    
    -- 1.13
    print(safetail [1,2,3])

    -- 1.14
    print(curta [1,2,3])
        





    -- 1.1
    -- print(testaTriangulo 1.0 3.0  3.0)
    -- 1.2
    -- print(areaTriangulo 1.0 3.0 3.0)
    -- print(areaTriangulo 1.0 3.0 1.0)
    -- 1.3
    -- print(metades [1,2,3,4,5,6,7,8])
    -- print(metades [1,2,3,4,5,6,7])
    -- 1.4
    -- print(last' [1,2,3,4])
    -- print(last'' [1,2,3,4])
    -- print(init' [1,2,3,4])
    -- print(init'' [1,2,3,4])
    -- 1.6
    -- print(raizes 1.0 2.0 3.0)
    -- 1.8
    -- print(segundo [1,2,3])
    -- print(trocar (1,2))
    -- print(par 1 1)
    -- print(dobro 2.0)
    -- print(dobro 2)
    -- print(metade 4.0)
    -- print(metade 4)

    -- print(minuscula 'b')
    -- print(intervalo 2 1 3)

    -- -- 1.10
    -- print(classifica 70 1.70)


