data Arv a = Vazia | No a (Arv a) (Arv a) deriving (Show)

-- 4.1
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (No n left right) = n + sumArv left + sumArv right
-- 4.2
listar  :: Arv a -> [a]
listar Vazia = []
listar (No n left right) = listar right ++ [n] ++ listar left
-- 4.3
nivel :: Int -> Arv a -> [a]
nivel _ Vazia = []
nivel 0 (No x _ _) = [x]
nivel n (No x left right) = (nivel (n-1) left) ++ (nivel (n-1) right)
-- 4.4
construir :: Ord a => [a] -> Arv a
construir [] = Vazia
construir [x] = No x Vazia Vazia
construir l = No fst_node (construir left) (construir right)
    where
        half = length l `div` 2
        fst_node = l !! half
        left = take half l
        right = drop half l

inserir :: Ord a =>  a -> Arv a -> Arv a
inserir x Vazia = No x Vazia Vazia
inserir x (No y left right) 
    | x == y = No y left right
    | x < y = No y (inserir x left) right
    | otherwise = No y left (inserir x right)
teste_4_4 l = foldr inserir Vazia l

-- 4.5 
mapArv :: (a->b) -> Arv a -> Arv b
mapArv f Vazia = Vazia
mapArv f (No x left right) = (No (f x) (mapArv f left) (mapArv f right))

-- 4.6
mais_esq :: Arv a -> a
mais_esq Vazia = error "Árvore vazia"
mais_esq (No x Vazia _) = x
mais_esq (No x left _) = mais_esq left

mais_dir :: Arv a -> a
mais_dir Vazia = error "Árvore vazia"
mais_dir (No x _ Vazia) = x
mais_dir (No x _ right) = mais_dir right

-- remover
remover :: Ord a => a -> Arv a -> Arv a 
remover _ Vazia = Vazia
remover x (No y left right)
    | (x < y) = No y (remover x left) right
    | (x > y) = No y left (remover x right)
    | (x == y) = Vazia

-- remover o valor mais à direita da árvore da esquerda
remover_dir :: Ord a =>  Arv a -> Arv a
remover_dir Vazia = error "Não têm nós para remover"
remover_dir (No _ Vazia right) = right
remover_dir (No _ left Vazia) = left
remover_dir (No _ left right) = No n (remover n left) right
    where n = mais_dir left

main :: IO() 
main = do
    -- 4.1
    print(sumArv (No 1 (No 2 Vazia Vazia) (No 3 Vazia Vazia)))
    -- 4.2
    print(listar (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)))
    -- 4.3
    print(nivel 2 (No 5 (No 3 (No 2 Vazia Vazia) (No 4 Vazia Vazia)) (No 9 (No 8 Vazia Vazia) (No 10 Vazia Vazia))))
    -- 4.4
    print(inserir 4 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)))
    print(teste_4_4 [1..5])
    -- 4.5
    print(mais_esq (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)))
    print(mais_dir (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)))
    -- 4.6
    print(remover_dir (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)))
