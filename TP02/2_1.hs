-- Exercício 2.1

-- (a)

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

-- (b)

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

-- (c)
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs 

myconcat' :: [[a]] -> [a] 
myconcat' l = [x | xs <- l, x <- xs]

-- (d)
myreplicate :: Int -> a -> [a]
myreplicate 0 y = []
myreplicate x y = y : myreplicate (x-1) y

myreplicate' :: Int -> a -> [a]
myreplicate' x y = [x | _ <- [1..n]]

-- (e)
myListSelect' :: [a] -> Int -> a
myListSelect' a 0 = head a
myListSelect' a b = myListSelect' (drop 1 a) (b-1)

myListSelect'' a b = head [x | (x,i) <- zip l [0..n]]

-- (f)
{-The Eq typeclass provides an interface for testing for equality. 
Any type where it makes sense to test for equality between two values of that type should be a member of the Eq class. 
All standard Haskell types except for IO (the type for dealing with input and output) and functions are a part of the Eq typeclass.-}
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = x == a || elem' a xs

main :: IO()
main = do
    {- 
    -- (a)
    print(myand [True, True, True, True])
    print(myand [False, True, False]) 
    -}

    {- 
    -- (b)
    print(myor [True, False, False])
    print(myor [False, False, False])
    -}

    {- 
    -- (c)
    print(myconcat [[1,2,3], [4,5,6]])
    -}

    
    {- 
    -- (d)
    print(myreplicate 20 5) 
    -}

    {- 
    --(e)
    print(myListSelect' [1..20] 11) 
    -}

    -- (f)
    {-
    print(elem' 15 [1, 10, 11, 5]) -- False
    print(elem' 10 [1,10,11,5]) -- True
    -}


