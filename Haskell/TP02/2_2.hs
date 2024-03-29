import Data.Char
import Data.List

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

-- Exercício 2.7 PERGUNTAR O PORQUÊ DA NECESSIDADE DO fromIntegral, stackOverflow não explicou um crl

aprox :: Int -> Double
aprox n = sum [(-1)^k / fromIntegral (2*k +1) | k <- [0..n]]

aprox' :: Int -> Double
aprox' n = sum [((-1) ^ k) / fromIntegral((k + 1) ^ 2) | k <- [0..n]]

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

dotprod'' :: [Float] -> [Float] -> Float
dotprod'' [] [] = 0
dotprod'' [a] [] = 
dotprod'' [] [a] =
dotprod'' l1 l2 = sum [x*y | (x,y) <- (zip l1 l2)]
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
{- cifraLetra :: Int -> Char -> Int

-}


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

-- Exercício 2.21
revertList :: [Int] -> [Int]
revertList [] = []
revertList (x:xs) = (revertList xs) ++ [x]

algarismos :: Int -> [Int]
algarismos n 
    | n < 10 = [n]
    | otherwise = algarismos (div n 10) ++ algarismos (mod n 10)

algarismosRev :: Int -> [Int]
algarismosRev n = revertList (algarismos n)

-- Exercício 2.22
toBits :: Int -> [Int] 
toBits 0 = [0]
toBits 1 = [1]
toBits n = toBits(div n 2) ++ [mod n 2]

-- Exercício 2.23
fromBits :: [Int] -> Int
{- fromBits l = sum (map (2^) (findIndices (==1) (reverse l))) -}
-- ou nouta syntax => $ funciona como substitudo de parentesis, e significa "apply"
fromBits l = sum $ map (2^) $ findIndices (==1) $ reverse l 

-- Exercício 2.24

-- (a)
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x < y = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

-- (b)

divideList :: [a] -> ([a],[a])
divideList [] = ([],[])
divideList [x] = ([x],[])
divideList l
    | (mod size 2 == 0) = ((take halfSize l ), drop halfSize l)
    | otherwise = ((take (halfSize+1) l ), (drop (halfSize + 1) l))
    where 
        size = length l
        halfSize = div size 2
          
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
    where  (left,right) = divideList xs

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
{-     print(pitagoricos 10)
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
    -- 2.18
    -- 2.19
    -- 2.20
    -- 2.21
    print(algarismos 12345)
    print(algarismosRev 12345)
    -- 2.22
    print(toBits 29)
    -- 2.23
    print(fromBits[1,1,1,0,1]) -}
    -- 2.24
    {- 
    print(merge [3,5,7] [1,2,4,6])
    
    print(divideList [1,2,3,4,5,6,7])
    print(divideList [1,2,3,4,5,6])
    print(divideList [1])
     -}
    print(divideList [3,5,7,1,2,4,6])
    print(mergeSort [3,5,7,1,2,4,6])

    -- print (aprox 10000000)
    print (aprox' 10000000)


    
    



