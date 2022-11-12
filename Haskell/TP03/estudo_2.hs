-- 3.2
dec2Int :: [Int] -> Int
dec2Int l = foldl (\x y -> (x*10) + y) 0 l
-- 3.3
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x, y) <- zip xs ys]

zipWith'' :: (Num a) => (a->b->c) -> [a] -> [b] -> [c]
zipWith'' f (x:xs) (y:ys) = (f x y) : (zipWith'' f xs ys)

-- 3.4
insert' :: Ord a =>  a -> [a] -> [a]
insert' k [] = [k]
insert' k (l:ls) 
    | (k <= l) = k : (l:ls)
    | otherwise = l : (insert' k ls)

isort' :: Ord a => [a] -> [a]
isort' l = foldr insert' [] l

-- 3.5
maximum' :: Ord a => [a] -> a
maximum' l = foldr1 (\x y -> if x > y then x else y) l

minimum' :: Ord a => [a] -> a
minimum' l = foldr1 (\x y -> if x < y then x else y) l

foldr1' :: (a->a->a) -> [a] -> a
foldr1' f l = foldr f (last l) (init l)

foldl1' :: (a->a->a) -> [a] -> a
foldl1' f l = foldl f (head l) (tail l)

-- 3.6

mdc :: Integral a => a -> a -> a
mdc a b = fst (until (\(x,y) -> y == 0) (\(x,y) -> (y, x `mod`y)) (a,b))

-- 3.7
(+++) :: [a] -> [a] -> [a]
(+++) l1 l2 = foldr (\x y -> x : y) l2 l1 

concat' :: [[a]] -> [a]
concat' = foldr (+++) []

reverse' :: [a] -> [a]
reverse' = foldr (\x y -> y ++ [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldl (\x y -> y : x) []

elem' :: Eq a => a -> [a] -> Bool
elem' k l = any (==k) l

-- 3.8

palavras :: String -> [String]
palavras [] = []
palavras l = word : palavras rest
    where 
        word = takeWhile (/= ' ') l
        rest = (drop 1 (dropWhile (/= ' ') l))

despalavras :: [String] -> String
despalavras l = foldr1 (\curr new -> curr ++ new) l 

main :: IO() 
main = do
    -- 3.2
    -- print(dec2Int [1,2,3,4])
    -- print(dec2Int [1,2])
    -- -- 3.4
    -- print(insert' 3 [1,2,4,5])
    -- print(isort' [3,4,1,5,3])
    -- -- 3.5
    -- print(maximum' [1,33,4,5])
    -- print(minimum' [4,5,0,33])
    -- -- 3.7
    -- print(mdc 325 202020)
    -- print([1,2,3] +++ [4,5,6])
    -- print(concat' [[1], [2], [3]])
    -- print(reverse' ['b','a','n','a','n','a'])
    -- print(reverse'' ['b','a','n','a','n','a'])

    -- print(elem' 6 [1,2,3,4,5])

    print(palavras "ba na na")
    print(despalavras (palavras "ba na na"))