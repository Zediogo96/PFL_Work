transforma :: String -> String
transforma [] = []
transforma (x:xs) 
    | (elem x vogais) = x : 'p' : x : transforma xs
    | otherwise = x : transforma xs 
    where vogais = "aeiou";


type Vector = [Int]
type Matriz = [[Int]]


max_pos :: [Int] -> Int
max_pos l = foldr1 (max) [x | x <- l, x >= 0] 

transposta :: Matriz -> Matriz
transposta [] = []
transposta m = [head x | x <- m] : transposta [tail x | x <- m, tail x /= []]

prodInterno :: Vector -> Vector -> Int
prodInterno l1 l2 = sum (map (\(x,y) -> (x*y))(zip l1 l2))

prodMat :: Matriz -> Matriz -> Matriz
prodMat m1 m2 = [[sum (map (\(x,y) -> (x*y))(zip l1 l2)) | l2 <- transposta m2] | l1 <- m1]


data Arv a = F | N a (Arv a) (Arv a)
    deriving(Show)

-- alturas (N "joão" (N "abel" F F) (N "pedro" F F)) = N 2 (N 1 F F) (N 1 F F)


max_alturaArv :: Arv a -> Int
max_alturaArv F = 0
max_alturaArv (N a left right) = 1 + max (max_alturaArv left) (max_alturaArv right)

alturas :: Arv a -> Arv Int
alturas F = F
alturas (N x left right) = N (max_alturaArv (N x left right)) (alturas left) (alturas right)

equilibrada :: Arv a -> Bool
equilibrada (N _ F F) = True
equilibrada (N x left right) = (diff == 1 || diff == -1 || diff == 0    )
    where  
        max_left = max_alturaArv left
        max_right = max_alturaArv right
        diff = max_left - max_right

auxiliar ::(Num b, Eq b, Integral b) => (a,b) -> [a]
auxiliar (a,b) = if ((b `mod` 2) == 0) then [a,a] else [a]

dups :: [a] -> [a]
dups l = foldr (++) [] (map (auxiliar) [(x,y) | (x,y) <- zip l [0..]])


niguais :: Int -> a -> [a]
niguais 0 _ = []
niguais n a = a : niguais (n-1) a 

length_zip xs = [x | x <- zip (reverse [1..length xs]) xs ]

decompor :: Int -> [Int] -> [[Int]]
decompor 0 _ = [[]]
decompor _ [] = []
decompor v (c:cs)
    | (v >= c) = (map (c:) (decompor (v - c) (c:cs))) ++ decompor v (cs)
    | otherwise = decompor v cs


main :: IO() 
main = do

    -- print(length_zip "zip")
    print(decompor 25 [2,5,10])

    -- print(niguais 5 'a')
    -- print(niguais' 5 'a')

    -- let l1 = [1,2,3,4]
    -- print(map (auxiliar) [(x,y) | (x,y) <- zip l1 [1..]])
    -- dups

    -- print(foldl (\x y -> y:x) [] "abcd")
    -- print(auxiliar ("a", 0))
    -- print(dups [0,1,2,3,4])
    -- -- Arvores
    -- print(max_alturaArv  (N "joão" (N "abel" F F) (N "pedro" (N "José" (N "Maria" F F) F) F)))
    -- print(alturas (N "joão" (N "abel" F F) (N "pedro" (N "José" (N "Maria" F F) F) F)))
    -- print(equilibrada (N "joão" (N "abel" F F) (N "pedro" (N "José" F F) F)))
    -- print(max_pos [-1,2,3,-4])
    -- print(transforma "ola, mundo!")
    -- print(transposta [[1,2], [3,4]]) 
    -- print([(2^x) - 1 | x <- [1..10]])
    -- print(prodInterno [1,2,3] [4,3,2])
    -- print(prodMat [[1,2], [3,4]] [[2,3], [2,4]])


    -- print([[(x,y) | x <- "abc"] | y <- [1,2,3]])
    -- print([(x,y) | x <- "abc", y <- [1,2,3]])
