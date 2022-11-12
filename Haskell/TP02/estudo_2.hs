and' :: [Bool] -> Bool
and' [] = False
and' (x:xs)
    | (x == True) = True
    | otherwise = and' xs
------------------------------------
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) 
    | (x == True) = True
    | otherwise = or' xs
------------------------------------
concat' :: [[a]] -> [a]
concat' [] = []
concat' xs = [y | x <- xs, y <- x]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs
------------------------------------
replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <- [1..n]]

replicate'' :: Int -> a -> [a]
replicate'' 0 _ = []
replicate'' n a = a : replicate'' (n-1) a
-----------------------------------

myselect' :: [a] -> Int -> a 
myselect' l n = head (drop n l)

myselect'' :: [a] -> Int -> a
myselect'' [] _ = error "empty list"
myselect'' l 0 = head l
myselect'' l n = myselect'' (drop 1 l) (n-1)

-----------------------------------

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) 
    | (x==n) = True
    | otherwise = elem' n xs

-----------------------------------

intersperse' :: a -> [a] -> [a]
intersperse' a l = drop 1 (foldl (\acc x -> acc ++ [a] ++ [x]) [] l)

-- interperse with list comprehension
intersperse'' :: a -> [a] -> [a]
intersperse'' a l = [x | (i,x) <- zip [0..] l, i /= 0]

-----------------------------------

mdc' :: Integer -> Integer -> Integer
mdc' a 0 = a
mdc' a b = mdc' b (a`mod`b)

-----------------------------------

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
    | (x <= y) = [x] ++ (y:ys)
    | (x > y) = [y] ++ insert' x (ys)

isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)

-----------------------------------

min':: Ord a => [a] -> a
min' l = head (isort' l)

min'' :: Ord a => [a] -> a
min'' [x] = x
min'' (x:xs) 
    | z < x = z
    | otherwise = x 
    where z = min'' xs

delete' :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete' k (x:xs)
    | (k == x) = xs
    | otherwise = x : delete' k xs

ssort' :: Ord a => [a] -> [a]
ssort' [] = []
ssort' l = k : ssort' (delete' k l) 
    where k = min' l

-----------------------------------

sum_ :: (Num a, Enum a) => a
sum_ = sum [x | x <- [1..100]]

dotprod' :: [Float] -> [Float] -> Float
dotprod' l1 l2 = sum [x * y | x <- l1, y <- l2]

divprop' :: Integer -> [Integer]
divprop' k = [x | x <- [1..(k-1)], (k `mod` x) == 0]

perfeitos :: Integer -> [Integer]
perfeitos k = [x | x <- [1..(k-1)], x == sum (divprop' x)]

pitagoricos :: Integer -> [(Integer, Integer, Integer)]
pitagoricos k = [(x,y,z) | x <- [1..k], y <- [1..k], z <- [1..k], ((x^2) + (y^2) == (z)^2)]

primo :: Integer -> Bool
primo k = (length (divprop' k)) == 1

concat''' :: [[a]] -> [a]
concat''' l = [x | y <- l, x <- y]

replicate''' :: Int -> a -> [a]
replicate''' n a = [a | _ <- [1..n]]

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' [y | y <- xs, x /= y]

transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' ([]:_) = []
transpose' l = [head x | x <- l] : transpose' [tail x | x <- l]

algarismos :: Int -> [Int]
algarismos k = [read [a] :: Int| a <- show k]

toBits :: Int -> [Int]
toBits 0 = []
toBits n = toBits (n `div` 2) ++ [n `mod` 2]

fromBits :: [Int] -> Int
fromBits l = sum (map (\(x,y) -> 2^y) [(x,y) | (x,y) <- zip l (reverse [0..((length l)-1)]), x /= 0])

main :: IO()
main = do
    -- print(and' [True, True, True])

    -- print(concat' [[1,2,3], [4,5]])
    -- print(concat'' [[1,2,3], [4,5]])

    -- print(replicate' 3 False)
    -- print(replicate'' 3 False)

    -- print(myselect' [1,2,3,4,5] 2)
    -- print(myselect'' [1,2,3,4,5] 2)

    -- print(elem' 2 [1,3,4,2])

    -- -- 2.2
    -- print(intersperse' '-' "banana")

    -- -- 2.3
    -- print(mdc' 15 20)
    -- -- 2.4
    -- print(insert' 2 [0,1,3,5])
    -- print(isort' [1,0,3,2])
    -- -- 2.5
    -- print(min' [4,5,2,16])
    -- print(min'' [4,5,2,16])
    -- print(delete' 1 [2,2,2,1,2,1])
    -- print(ssort' [2,3,4,5,1,7,3,2])
    -- -- 2.6
    -- print(sum_)
    -- -- 2.8
    -- print(dotprod' [2.0, 3.0] [2.0, 3.0])
    -- -- 2.9
    -- print(divprop' 10)
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
    -- print(merge' [3, 5, 7] [1, 2, 4, 6])
    -- print(mergeSort' [4,3,1,2,7,6])
    