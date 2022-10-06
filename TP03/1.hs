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
isort' = foldl insert' []

main :: IO() 
main = do

    -- 3.1
    -- print(primeiro (+2) (odd) [1..10])
    -- 3.2
    -- print(dec2int [2,3,4,5]) 
    -- 3.3
    -- print(zipWith' [1..5] [1..5])
    -- 3.4
    print(isort' [1,5,3,4,7,6])


    
