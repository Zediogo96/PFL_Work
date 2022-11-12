{- primo :: Integer -> Bool
primo n = length [x | x <- [1..floor $ sqrt $ fromIntegral n], n `mod` x == 0] == 1  

gémeos :: Integer -> (Integer, Integer)
gémeos n = head [(x, x+2) | x <- [n..], primo x && primo (x+2)]

-}

type Point = (Double, Double)

cantoInfEsq :: [Point] -> [Point]
cantoInfEsq l = [(x,y) | (x,y) <- list_filtY, y == minX]
    where 
        list_filtY = [(x,y) | (x,y) <- l, y == maxY]
        minX = minimum (map (fst) list_filtY)
        maxY = maximum (map (snd) l)

-- boundingBox :: [Point] -> (Point, Point)
-- boundingBox 


main :: IO()
main = do

    -- print(maximum (map (snd) [(1.0, 3.0), (4.0, 2.0)]))
    print(cantoInfEsq [(1.0, 3.0), (4.0, 2.0)])

    -- print(primo 7)
    -- print(gémeos 10)
    -- print(tail (reverse [1..5]))
    -- print(map (\x -> 3*x+1) [1,2,3])
    -- print([(x,y) | x<-[1,2], y<-[2,4], x*y==4])
    -- print(foldr (-) 0 [1,2,3])
    -- -- [(Int, [Int]), (Int, [Int])]

    -- print(filter (/= 0) [1,2,0,0,3,0,0])



    
