import Data.List

-- 3.1


primeiro' :: (a->a) -> (a->Bool) -> [a] -> [a]
primeiro' f p l = map (f) (filter (p) l)

-- 3.2

-- So if you seed the foldl with 0, and had [1,2,3], your function would multiply current (0) by 10 (also 0), then add 1. 
-- Moving on, multiply current (1) by 10 (to get 10) and add 2 (12). Then finally for 3, 12 * 10 = 120, 120 + 3 = 123.

dec2Int:: [Int] -> Int
dec2Int l = foldl (\x y -> (x*10) + y) 0 l

-- 3.3

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3.4

insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys)
    | x < y = x : y : ys
    | otherwise = y : insert' x ys

isort' :: Ord a => [a] -> [a]
isort' l = foldr insert' [] l

-- 3.5

maximum':: Ord a => [a] -> a
maximum' = foldr1 max

minimum' :: Ord a => [a] -> a
minimum' = foldr1 min

----------------------------

foldl1' :: (a->a->a) -> [a] -> a
foldl1' f xs = foldl f (head xs) (tail xs)

-- 3.6
mdc' :: Integral a =>  a -> a -> a
mdc' x y = fst (until (\(x,y) -> y == 0) (\(x,y) -> (y, x`mod`y)) (x,y))

-- 3.7
-- (a)
(+++) :: [a] -> [a] -> [a]
l1 +++ l2 = foldr (:) l2 l1 
-- (b)
concat' :: [[a]] -> [a]
concat' xs = foldl (++) [] xs 
-- (c)
reverse' :: [a] -> [a]
reverse' xs = foldr (\acc elem -> elem ++ [acc]) [] xs
-- (d)
reverse'' :: [a] -> [a]
reverse'' xs = foldl (\acc elem -> elem : acc) [] xs
-- (e)
elem' :: Eq a => a -> [a] -> Bool
elem' a xs = foldr (\x y -> if x == a then True else y) False xs

-- 3.8
palavras' :: String -> [String]
palavras' [] = []
palavras' (' ':xs) = palavras' xs
palavras' s = [x | x <- word] : palavras' rest
    where 
        word = takeWhile (/= ' ') s
        rest = dropWhile (/= ' ') s

-- 3.9 

despalavras' :: [String] -> String
despalavras' xs = foldl1 (\x y -> x ++ " " ++ y) xs

myscanl :: (a->a->a) -> a -> [a] -> [a]
myscanl _ z [] = [z]
myscanl f z (x:xs) = z : myscanl f (f z x) xs

main :: IO() 
main = do
    -- 3.2
    print(dec2Int [1,2,3,4])
    -- 3.4
    print(insert' 3 [1,2,4,5])
    print(isort' [3,4,1,5,3])
    -- 3.5
    print(maximum' [1,33,4,5])
    print(minimum' [4,5,0,33])
    -- 3.7
    print(mdc' 8 160)
    print([1,2,3] +++ [4,5,6])
    print(concat' [[1], [2], [3]])
    print(reverse' ['b','a','n','a','n','a'])

    print(elem' 6 [1,2,3,4,5])

    print(palavras' "ba na na")
    print(despalavras' (palavras' "ba na na"))


   
