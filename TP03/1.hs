-- 3.1
-- primeiro :: (a->b) -> (a->Bool) -> [a] -> [a]
primeiro f p l = map f (filter p l)

{- primeiro'' f p l = map f $ filter p l 
 -}
-- 3.2

-- exemplo livro prof
sum2 l = foldl (*) 1

-- what is the meaning of \? => fazer uma função on the fly
dec2int :: [Int] -> Int
dec2int l =  foldl (\x y -> (x * 10) + y) 0 l

-- 3.3

zipWith f xs ys = [f x y | (x, y) <- zip xs ys]

{- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : f xs ys -}

-- 3.4

insert' :: Ord a => a -> [a] -> [a]
insert' y [] = [y]
insert' y (x:xs)
    | (x <= y) = x : insert' y xs
    | otherwise = y : x : xs

isort' :: Ord a => [a] -> [a]
isort' = foldr insert' []

-- 3.5
-- (a)
maximum' :: Ord a => [a] -> a
maximum' l = foldr1 max l

minimum' :: Ord a => [a] -> a
minimum' l = foldr1 min l
-- (b)

-- tail => list without it's first element
-- foldl => computers from left (1st element) to last
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' f l = foldl f (head l) (tail l) 

-- init => list w/o it's last element
-- foldr => computers from right (last element) to first
foldr1' :: (a -> a -> a) -> [a] -> a
foldr1' f l = foldr f (last l) (init l)

-- 3.6 

-- mdc a b = if b == 0 then a else mdc b (a ‘mod‘ b)
mdc :: Integer -> Integer -> Integer
mdc a b = fst (until (\(a,b) -> b == 0) (\(a,b) -> (b, a `mod` b)) (a,b))

-- 3.7

-- (a)
mypp :: [a] -> [a] -> [a]
mypp a b = foldr (\cur new -> (cur : new)) b a
-- (b)
myconcat' :: [[a]] -> [a]
myconcat' l = foldr (\x y -> mypp x y) [] l 
-- (c)
myreverse' :: [a] -> [a]
myreverse' l = foldr (\cur new -> new ++ [cur]) [] l
-- (d)
myreverse'' :: [a] -> [a]
myreverse'' l = foldl (\cur new -> new : cur) [] l
-- (e) 
elem' :: Eq a => a -> [a] -> Bool
elem' x l = any (x==) l

-- 3.8
-- (a)
palavras' :: String -> [String]
palavras' [] = []
palavras' (' ' : l) = palavras' l
palavras' l = [y | y <- (takeWhile (' ' /=) l)] : palavras' (dropWhile (' ' /=) l)

-- (b)
-- inicialmente estava a utilizar foldr, o correcto é foldr1, pq obriga a ter 2 elem presentes
despalavras' :: [String] -> String
-- despalavras' s = foldr (\x y -> x ++ " " ++ y) [] s
despalavras' s = foldr1 (\x y -> x ++ " " ++ y) s

-- 3.9

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ z [] = [z]
scanl' f accum (x:xs) = accum : scanl' f (f accum x) xs


main = do

    -- 3.1
    -- print(primeiro (+2) (odd) [1..10])
    -- 3.2
    -- print(dec2int [2,3,4,5]) 
    -- 3.3
    -- print(zipWith' [1..5] [1..5])
    -- 3.4
    -- print(isort' [1,5,3,4,7,6])
    -- 3.5
    -- (a)
    -- print(maximum' [1,2,5,15,3,9])
    -- print(minimum' [15,12,9,5,7])
    -- (b)

    -- 3.6
    -- print(mdc 133 399)
    -- 3.7
    -- (a) 
    -- print(mypp [1,3] [4,5])
    -- (b)
    -- print(myconcat' [[1,2,3],[4,5,6]])
    -- (c)
    -- print(myreverse' [6,5,4,3,2,1])
    -- print(myreverse'' [6,5,4,3,2,1])
    -- (e)
    -- print(elem' 5 [6..10])
    -- 3.8
    print(palavras' "Filho da Puta do Verstappen")
    print(despalavras' ["Filho","da","Puta","do","Verstappen"])
    -- 3.9
    print(scanl' (+) 0 [1..3])
    





    
