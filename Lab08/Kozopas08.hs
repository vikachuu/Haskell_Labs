{-# OPTIONS_GHC -Wall #-}
module Kozopas08 where

import Data.Array

type Graph = Array Int [Int]

nodes :: Graph -> [Int]
nodes g = indices g

edges :: Graph -> [(Int, Int)]
edges g = [(v1, v2) | v1 <- nodes g, v2 <- g ! v1]

adjacent :: Graph -> Int -> [Int]
adjacent g v = g ! v

-- Задача 1 ------------------------------------------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr start end
    | start == end = Nothing
    | ifNullSublist allPathsAB = Nothing
    | otherwise = Just (snd $ maximum $ map (\x -> (length x, x)) allPathsAB)
                  where allPathsAB = allPaths start end (edges gr)

allPaths :: Int -> Int -> [(Int, Int)] -> [[Int]] 
allPaths start end es 
    | start == end = [[start]]
    | otherwise = [ start:path | edge <- es, start == (fst edge),
                                 path <- (allPaths (snd edge) end [e | e <- es, e /= edge])]



-- Задача 2 -----------------------------------------  
isNoCycle :: Graph -> Bool
isNoCycle gr = and [ifNullSublist (allPathsAndLoop neighb ver (edges gr)) | ver <- indices gr, 
                                                                            neighb <- adjacent gr ver]

ifNullSublist :: [[Int]] -> Bool
ifNullSublist [] = True
ifNullSublist _ = False

allPathsAndLoop :: Int -> Int -> [(Int, Int)] -> [[Int]] 
allPathsAndLoop start end es 
    | start == end = if ((start, end) `elem` es) then [[start,end]] else [[start]]
    | otherwise = [start:path | edge <- es, start == (fst edge),
                                path <- (allPathsAndLoop (snd edge) end [e | e <- es, e /= edge])]
                                    
  
-- Задача 3 -----------------------------------------
isTransitive :: Graph -> Bool
isTransitive gr
    | length (indices gr) == 1 = True
    | length (indices gr) == 2 = True
    | otherwise = isTransitive' (edges gr)

isTransitive' :: [(Int, Int)] -> Bool
isTransitive' xs = and [(fst x, snd y) `elem` xs | x <- xs, y <- xs, snd x == fst y]
   
-- Задача 4 -----------------------------------------
isGraph :: Graph -> Bool
isGraph gr = and [(snd x, fst x) `elem` xs| x <- xs]
             where xs = edges gr

-- Задача 5 -----------------------------------------
shortWay :: Graph -> Int -> Int -> Maybe [Int]
shortWay gr start end
    | start == end = Nothing
    | ifNullSublist allPathsAB = Nothing
    | otherwise = Just (snd $ minimum $ map (\x -> (length x, x)) allPathsAB)
                  where allPathsAB = allPaths start end (edges gr)

-- Задача 6 -----------------------------------------
isConnecting :: Graph -> Bool
isConnecting gr
    | length (components gr) == 1 = True
    | otherwise = False


-- Задача 7 -----------------------------------------
components :: Graph -> [[Int]]
components gr = components' (indices gr) (edges gr) []

components' :: [Int] -> [(Int,Int)] -> [[Int]] -> [[Int]]
components' [] _ comp = comp
components' vert edg comp = comp ++ components' leftVertexes edg [newComponent]
                            where leftVertexes = subtractLists vert newComponent
                                  newComponent = createComp vert edg

createComp :: [Int] -> [(Int,Int)] -> [Int]
createComp vert edg = createComp' vert edg [head vert]

createComp' :: [Int] -> [(Int,Int)] -> [Int] -> [Int]
createComp' [] _ _ = []
createComp' _ _ [] = []
createComp' vert edg (v:vs)
    | [x | x <- vert, x == v] == [] = createComp' newVert edg vs
    | otherwise = v : createComp' newVert edg (adjac ++ vs)
    where newVert = [x | x <- vert, x /= v]
          adjac = [x | (x, y) <- edg, y == v] ++ [x | (y, x) <- edg, y == v]

subtractLists :: [Int] -> [Int] -> [Int]
subtractLists xs ys = filter (not . (`elem` ys)) xs


-- Задача 8 -----------------------------------------
topolSorting :: Graph -> Maybe[Int]
topolSorting gr
    | not (isNoCycle gr) = Nothing
    | not (isConnecting gr) = Nothing
    | otherwise = Just (topolSorting' (indices gr) (edges gr))

-- array (1,5) [(1,[3,2,4]), (3,[2]), (4,[2,5]), (2,[5]),(5,[])]
-- [1,2,3,4,5] [(1,3),(1,4),(1,2),(1,5),(3,2),(4,2),(4,5),(2,5)] 
--
-- [(1,[]),(2,[1,3,4]),(3,[1]),(4,[1]),(5,[1,4,2])]
--
-- [1,3,4,2,5]

topolSorting' :: [Int] -> [(Int,Int)] -> [Int]
topolSorting' vert [] = vert
topolSorting' vert edg = [vertInTopList] ++ topolSorting' newVert newEdg
                         where newVert = subtractLists vert [vertInTopList]
                               newEdg = delEdges vertInTopList edg
                               vertInTopList = getVertex vert edg

vertexesIn :: [Int] -> [(Int,Int)] -> [(Int, [Int])]
vertexesIn vert edg = [(v, [fst x | x <- edg, snd x == v]) | v <- vert]

getVertex :: [Int] -> [(Int,Int)] -> Int
getVertex vert edg = fst $ head (filter (null . snd) (vertexesIn vert edg))

delEdges :: Int -> [(Int,Int)] -> [(Int,Int)]
delEdges v edg = filter ((/= v) . fst) edg


gr1, gr2, gr3, gr5, gr6, gr4, gr7, gr9 :: Graph
gr1 = array (1,9) [(1,[2,4]), (2,[3,5]), (3, [7]), (4, [7]), (5, [8,6]), (6, [9]), (7, [8]), (8, [3]), (9, [])]

gr2 = array (1,4) [(1,[2]),(2,[3]),(3,[4]),(4,[1])]

gr3 = array (1,7) [(1, [2,5]), (2, [1,3,5,6]), (3, [2,4,6]), (4, [3,6]), (5, [1,2,6]), (6, [2,3,4,5]), (7, [])]

gr4 = array (1,2) [(1, [2]), (2, [])]

gr6 = array (1,9) [(1,[2,4]), (2,[5]), (3, [7]), (4, [7]), (5, [8,6]), (6, [9]), (7, [8]), (8, []), (9, [])]

gr7 = array (1,3) [(1,[2,3]), (2,[3]), (3,[])]

gr5 = array (1,2) [(1,[2]), (2,[1])]

gr9 = array (1,4) [(1,[2]), (2,[3]), (3,[4]), (4,[])]