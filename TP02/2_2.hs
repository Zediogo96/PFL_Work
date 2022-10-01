import Data.Char

-- Exercício 2.2

interperse :: a -> [a] -> [a]
interperse _ [] = []
interperse _ [x] = [x]
interperse a (x:xs) = x : a : interperse a xs -- usa-se : para concatenar elementos sem ser de listas
                                                -- em vez de interperse a (x:xs) = [x] ++ [a] ++ interperse a xs


-- Exercício 2.3

mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a`mod`b)

-- Exercício 2.4

-- a 
myInsert :: Ord a => a -> [a] -> [a]
myInsert y [] = [y]
myInsert y (x:xs) 
    | x < y = x : myInsert y xs
    | otherwise = y : x : xs

-- b
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = myInsert x (iSort xs)

-- Exercício 2.5

-- a
myMinimum :: Ord a => [a] -> a
myMinimum [x] = x
myMinimum (x:xs) 
    | z < x = z
    | otherwise = x
    where z = myMinimum xs

-- b 
myDelete :: Eq a => a -> [a] -> [a]
myDelete n [] = []
myDelete n (x:xs)
    | (x == n) = xs 
    | otherwise = x : myDelete n xs

-- c
myssort :: Ord a => [a] -> [a]
myssort [] = []
myssort l = z : myssort (myDelete z l)
    where z = myMinimum l

-- Exercício 2.6
somaquadrados :: (Num a, Enum a) => a -> a -> a
somaquadrados a b = sum [ x^2 | x <- [a..b]]

-- Exercício 2.7

-- Exercício 2.8

dotprod :: [Float] -> [Float] -> Float
dotprod [] [] = 0
dotprod (x:xs) (y:ys) 
    | (length (x:xs)) /= (length (y:ys)) = error "Different sizes, can't calculate"
    | otherwise = x*y + (dotprod xs ys)

dotprod' :: [Float] -> [Float] -> Float
dotprod' [] [] = 0
dotprod' l1 l2
    | (length l1) /= (length l2) = error "Different sizes, can't calculate"
    | otherwise = sum [x*y | (x,y) <- (zip l1 l2)]

-- Exercício 2.9

divprop :: Integer -> [Integer]
divprop n = [ i | i <- [1..(n-1)], n `mod` i == 0]

-- Exercício 2.10
perfeito :: Integer -> [Integer]
perfeito n = [i | i <- [1..n], i == sum (divprop i)]

-- Exercício 2.11
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2) + (y^2) == z^2]

-- Exercício 2.12
primo :: Integer -> Bool
primo n = length (divprop n) == 1

-- Exercício 2.13
isMerssene :: Integer -> Bool
isMerssene n = primo n && not (null [i | i <- [0..n], (2^i - 1) == n])

merssene :: [Integer]
merssene = [i | i <- [1..30], isMerssene i]

-- Exercícip 2.14

-- Exercício 2.15


-- Exercício 2.16
-- (a)
concat' :: [[a]] -> [a]
concat' xss = [ x | xs <- xss, x <- xs]
-- (b)
replicate' :: Integer -> a -> [a]
replicate' n x = [x | _ <- [1..n]]
-- (c)

--Exercício 2.17

forte :: String -> Bool
forte xs = (length xs >= 8) && checkUpper xs && checkLower xs && checkDigit xs
    where 
        checkUpper xs = (length [isUpper x | x <- xs ] /= 0)
        checkLower xs = (length [isLower x | x <- xs ] /= 0)
        checkDigit xs = (length [isDigit x | x <- xs ] /= 0)


main :: IO()
main = do

    {- -- 2.2
    print(interperse '-' "banana")
    -- 2.3
    print(mdc 49 7)
    -- 2.4
    print(myInsert 2 [1,3,4,5,6,9,10])
    print(myInsert 7 [1,3,4,5,6,9,10])
    print(iSort [1,3,9,5,4,8])
    -- 2.5 
    print(myMinimum [4,3,5,10])
    print(myDelete 1 [2,1,1,2,3,4])
    print(myssort [2,1,1,2,3,4])
    -- 2.6
    print(somaquadrados 1 100)
    -- 2.7

    -- 2.8
    print(dotprod [2.3, 3.4, 4.3]  [2.4, 3.4, 4.9])
    print(dotprod' [2.3, 3.4, 4.3]  [2.4, 3.4, 4.9])

    -- print(dotprod [2.3, 3.4, 4.3]  [2.4, 3.4, 4.9,5.5]) -- gives error on purpose

    -- 2.9
    print(divprop 20)
    print(divprop 500)
    -- 2.10
    print(perfeito 500) -}
    -- 2.11
    print(pitagoricos 10)
    -- 2.12 
    print(primo 43)
    print(primo 45)
    -- 2.13
    print(merssene)

    -- 2.16
    print(concat' [[1], [2,3], [5,6]])
    print(replicate' 10 2)
    -- 2.17
    print(forte "Pwds1234")


    
    



