import Data.List

teste = [if even x then x else -x | x <- [0..10]]

h :: (Ord a) => a -> a -> a -> Bool
h x y z = x >= z && y <= z

f :: [a] -> a
f xs = xs !! 2;

numEqual :: (Eq a) => a -> a -> a -> Int
numEqual x y z = if x == y && y == z then 3 else if x == y || y == z || x == z then 2 else 0

pow :: Floating a => a -> a
pow x = x ^ 2

area :: (Floating a) => a -> a -> a -> a -> a -> a -> a
area a b c d p q = 0.25 * sqrt((4 * (pow p) * (pow q)) - parent)
    where parent = pow ((pow b) + (pow d) - (pow a) - (pow c))


enquantoPar :: [Int] -> [Int]
enquantoPar [] = []
enquantoPar (x:xs) 
    | ((not . odd) x) = x : enquantoPar xs
    | otherwise = []

nat_zip ::[a] -> [(Int, a)]
nat_zip l = [(y,x) | (x,y) <- zip l [1..]]

quadrados xs = [x ^ 2 | x <- xs]

quadrados_rec [] = []
quadrados_rec (x:xs) = (x ^ 2) : quadrados_rec xs

-- get all combinations of digits list from 1 to n
combinations :: Int -> [[Int]]
combinations n = [x | x <- subsequences [1..n]]

digits n = [x | x <- combinations n, sum x == n]












main :: IO() 
main = do  
    
    -- print(combinations 5)
    print(digits 5)
    -- print(quadrados_rec [2,10,1])
    -- print(quadrados [2,10,1])
    
    -- print(h 10 1 5)
    -- print(f [1,2,3,4,5])
    -- print(numEqual 1 2 2)
    -- print(area 1 2 3 4 5 6)
    -- print(enquantoPar [2,4,8,3,4,8,6])
    -- print(nat_zip "zip")



    
