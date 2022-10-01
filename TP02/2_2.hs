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



main :: IO()
main = do

    -- 2.2
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
    



