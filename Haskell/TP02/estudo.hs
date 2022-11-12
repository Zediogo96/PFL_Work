import Data.Char
import Data.List

-- 2.1


and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | (x == False) = False
    | otherwise = and' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs)
    | (x == True) = True
    | otherwise = or' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = [a] ++ (replicate' (n-1) a)

select' :: [a] -> Int -> a
select [] n = []
select' l 0 = head l
select' l n = select' (drop 1 l) (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) 
    | x == a = True
    | otherwise = elem' a xs

-- 2.2
intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' a (x:xs) = x : a : intersperse' a xs

-- 2.3

mdc :: (Integral a) => a -> a -> a
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

-- 2.4

insert' :: (Ord a) => a -> [a] -> [a]
insert' a [] = [a]
insert' y (x:xs)
    | (y > x) = x : insert' y xs
    | otherwise = y:x:xs

isort' :: (Ord a) => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)    

-- 2.5

min' :: (Ord a) => [a] -> a
min' l = head (isort' l)

max' :: (Ord a) => [a] -> a
max' l = last (isort' l)

delete' :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete' k (x:xs)
    | (k == x) = xs
    | otherwise = x : (delete' k xs)

ssort' :: Ord a => [a] -> [a]
ssort' [] = []
ssort' l = m : ssort' (delete' m l) 
    where m = min' l

-- 2.6
sum100 :: Int
sum100 = sum [x | x <- [1..100]]

-- 2.8
dotprod :: [Float] -> [Float] -> Float
dotprod l1 l2 = sum [x*y | (x,y) <- zip l1 l2]

-- 2.9
divprop :: Integer -> [Integer]
divprop n = [x | x <- [1..n-1], n `mod` x == 0]

-- 2.10
perfeitos :: Integer -> [Integer]
perfeitos n = [x | x <- [1..n-1], x == sum (divprop x)]

-- 2.11
pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], (x^2) + (y^2) == z^2] 

-- 2.12
primo :: Integer -> Bool
primo n = length l == 2
    where l = [x | x <- [1..n] , n `mod` x == 0]

-- 2.15
cifrar :: Int -> String -> String
cifrar x l = [chr ((ord a) + x) | a <- l]

-- 2.16
concat'' :: [[a]] -> [a]
concat'' l = [x | xs <- l, x <- xs]


replicate'' :: Integer -> a -> [a]
replicate'' n x = [x | _ <- [1..n]]

select'' :: Int -> [a] -> a
select'' n l = head [x | (x,y) <- zip l [0..(length l - 1)], y == n]

forte :: String -> Bool
forte pw = upper && lower && digit && (length pw) >= 8
    where 
        upper = (length [x | x <- pw, isUpper x]) >= 1
        lower = (length [x | x <- pw, isLower x]) >= 1
        digit = (length [x | x <- pw, isLower x]) >= 1


forte' :: String -> Bool
forte' pw = (any isUpper pw) && (any isLower pw) && (any isDigit pw) && (length pw) >= 8
    where 
        upper = any isUpper pw
        lower = any isLower pw
        digit = any isDigit pw

-- 2.19
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' [y | y <- xs, y /= x]

-- 2.20

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:_) = []
transpose' l = (map head l) : transpose' (map tail l)

-- 2.21 
algarismos :: Int -> [Int]
algarismos a = [read [x] :: Int | x <- show a]

-- 2.22
toBits :: Int -> [Int]
toBits 0 = []
toBits n = (n `mod` 2) : toBits (n `div` 2)

-- 2.23
fromBits :: [Int] -> Int
fromBits l = sum $ map (2^) $ findIndices (==1) $ reverse l

-- 2.24
-- merge two sorted lists
merge' :: Ord a => [a] -> [a] -> [a]
merge' [] l = l
merge' l [] = l
merge' (x:xs) (y:ys)
    | x < y = x : merge' xs (y:ys)
    | otherwise = y : merge' (x:xs) ys

mergeSort' :: Ord a => [a] -> [a]
mergeSort' [] = []
mergeSort' [a] = [a]
mergeSort' l = merge' (isort' h1) (isort' h2) 
    where 
        h1 = take len l
        h2 = drop len l
        len = (length l) `div` 2

main :: IO()
main = do

    -- print(and' [True, True, True])
    -- print(or' [False, True, False])
    -- print(concat' [[1,2,3], [4,5]])
    -- print(replicate' 3 False)
    -- print(select' [1,2,3,4,5] 1)
    -- print(elem' 2 [1,3,4,2])
    -- -- 2.2
    -- print(intersperse' '-' "banana")
    -- -- 2.3
    -- print(mdc 15 20)
    -- -- 2.4
    -- print(insert' 2 [0,1,3,5])
    -- print(isort' [1,0,3,2])
    -- -- 2.5
    -- print(min' [4,5,2,16])
    -- print(delete' 1 [2,2,2,1,2,1])
    -- print(ssort' [2,3,4,5,1,7,3,2])
    -- -- 2.6
    -- print(sum100)
    -- -- 2.8
    -- print(dotprod [2.0, 3.0] [2.0, 3.0])
    -- -- 2.9
    -- print(divprop 10)
    -- -- 2.10
    -- print(perfeitos 500)
    -- -- 2.11
    -- print(pitagoricos 10)
    -- -- 2.12
    -- print(primo 103)
    -- print(primo 112)
    -- -- 2.15
    -- print(cifrar 3 "ATAQUE DE MADRUGADA")
    -- -- 2.16
    -- print(concat'' [[1,2,3], [4,5]])
    -- print(replicate'' 3 "SKRR")
    -- print(select'' 2 [3,4,5])
    -- -- 2.17
    -- print(forte "abcdeAf3")
    -- print(forte' "abceAf3")
    -- -- 2.19
    -- print(nub' [1,1,2,2])
    -- -- 2.20
    -- print(transpose' [[1,2,3], [4,5,6], [7,8,9]])
    -- -- 2.21
    -- print(algarismos 1234)
    -- -- 2.22
    -- print(toBits 4)
    -- -- 2.23
    print(fromBits [1,1,1])
    -- 2.24
    print(merge' [3, 5, 7] [1, 2, 4, 6])
    print(mergeSort' [4,3,1,2,7,6])




    