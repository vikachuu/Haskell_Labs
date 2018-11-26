{-# OPTIONS_GHC -Wall #-}
module Kozopas07 where

type Index = Int
data BExp = Prim Bool | IdRef Index | Not BExp | And BExp BExp | Or BExp BExp
            deriving (Eq, Ord, Show)
type Env = [(Index, Bool)]

type NodeId = Int
type BDDNode =  (NodeId, (Index, NodeId, NodeId))
type BDD = (NodeId, [BDDNode])

-- Help functions -----------------------------------
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, y, _) = y

thd3 :: (a,b,c) -> c
thd3 (_, _, z) = z

-- Задача 1 -----------------------------------------
checkSat :: BDD -> Env -> Bool
checkSat bdd env =  findLeaf (fst bdd) bdd env

findLeaf :: NodeId -> BDD -> Env -> Bool
findLeaf 1 _ _ = True
findLeaf 0 _ _ = False
findLeaf n bdd env = findLeaf (findNextNodeId n bdd env) bdd env

findNextNodeId :: NodeId -> BDD -> Env -> NodeId
findNextNodeId n bdd env = if (getBoolValue (fst3 (snd nextNode)) env)
                           then thd3 (snd nextNode)
                           else snd3 (snd nextNode)
                           where nextNode = findNodeById n (snd bdd)

getBoolValue :: Index -> Env -> Bool
getBoolValue _ [] = error "index error"
getBoolValue ind (x:xs) = if ind == fst x then snd x else getBoolValue ind xs

findNodeById :: NodeId -> [BDDNode] -> BDDNode
findNodeById _ [] = error "index error"
findNodeById nId (x:xs) = if nId == fst x then x else findNodeById nId xs

-- Задача 2 -----------------------------------------
sat :: BDD -> [[(Index, Bool)]]
sat (_, []) = []
sat bdd@(_, nodes) = filter (checkSat bdd) (getAllEnvs nodes)

getAllEnvs :: [BDDNode] -> [Env]
getAllEnvs nodes = allEnvs (getListsOfPairs (getListOfIndexes nodes))
                   where allEnvs = foldr (\xs yss -> [x:ys | x <- xs, ys <- yss]) [[]]

getListOfIndexes :: [BDDNode] -> [Index]
getListOfIndexes nodes = removeDuplicate [fst3 (snd x)| x <- nodes]
                         where removeDuplicate [] = []
                               removeDuplicate (x:xs) 
                                   | x `elem` xs = removeDuplicate xs
                                   | otherwise = x : removeDuplicate xs

getListsOfPairs :: [Index] -> [[(Index, Bool)]]
getListsOfPairs xs = [[(x, False), (x, True)] | x <- xs]

-- Задача 3 -----------------------------------------
simplify :: BExp -> BExp
simplify (Not (Prim True)) = Prim False
simplify (Not (Prim False)) = Prim True
simplify (Not (IdRef r)) = Not (IdRef r)

simplify (Or (Prim True) (Prim True)) = Prim True
simplify (Or (Prim True) (Prim False)) = Prim True
simplify (Or (Prim False) (Prim True)) = Prim True
simplify (Or (Prim False) (Prim False)) = Prim False
simplify (Or (Prim True) (IdRef r)) = Or (Prim True) (IdRef r)
simplify (Or (Prim False) (IdRef r)) = Or (Prim False) (IdRef r)
simplify (Or (IdRef r) (Prim True)) = Or (IdRef r) (Prim True)
simplify (Or (IdRef r) (Prim False)) = Or (IdRef r) (Prim False)

simplify (And (Prim True) (Prim True)) = Prim True
simplify (And (Prim True) (Prim False)) = Prim False
simplify (And (Prim False) (Prim True)) = Prim False
simplify (And (Prim False) (Prim False)) = Prim False
simplify (And (Prim True) (IdRef r)) = And (Prim True) (IdRef r)
simplify (And (Prim False) (IdRef r)) = And (Prim False) (IdRef r)
simplify (And (IdRef r) (Prim True)) = And (IdRef r) (Prim True)
simplify (And (IdRef r) (Prim False)) = And (IdRef r) (Prim False)

simplify (Prim b) = Prim b
simplify (IdRef r) = IdRef r

simplify expr = expr


-- Задача 4 -----------------------------------------
restrict :: BExp -> Index -> Bool -> BExp
restrict (Prim x) _ _ = Prim x
restrict (IdRef r) ind bl 
    | r == ind = Prim bl
    | otherwise = IdRef r

restrict (Not (IdRef r)) ind bl
    | r == ind = simplify (Not (Prim bl))
    | otherwise = Not (IdRef r)
restrict (Not x) ind bl = simplify (Not (restrict x ind bl))

restrict (Or (IdRef r1) r2) ind bl
    | r1 == ind = simplify (Or (Prim bl) (restrict r2 ind bl))
    | otherwise = simplify (Or (IdRef r1) (restrict r2 ind bl))  
restrict (Or r1 (IdRef r2)) ind bl
    | r2 == ind = simplify (Or (restrict r1 ind bl) (Prim bl))
    | otherwise = simplify (Or (restrict r1 ind bl) (IdRef r2))
restrict (Or r1 r2) ind bl = simplify (Or (restrict r1 ind bl) (restrict r2 ind bl)) 

restrict (And (IdRef r1) r2) ind bl
    | r1 == ind = simplify (And (Prim bl) (restrict r2 ind bl))
    | otherwise = simplify (And (IdRef r1) (restrict r2 ind bl))
restrict (And r1 (IdRef r2)) ind bl
    | r2 == ind = simplify (And (restrict r1 ind bl) (Prim bl))
    | otherwise = simplify (And (restrict r1 ind bl) (IdRef r2))
restrict (And r1 r2) ind bl = simplify (And (restrict r1 ind bl) (restrict r2 ind bl))



-- Задача 5 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildBDD :: BExp -> [Index] -> BDD
buildBDD e xs = buildBDD' e 2 xs 

buildBDD' :: BExp -> NodeId -> [Index] -> BDD
buildBDD' expr _ [] = (getPrim expr, [])
buildBDD' expr nId xs = (nId, buildNodes expr nId xs)

buildNodes :: BExp -> NodeId -> [Index] -> [BDDNode]
buildNodes _ _ [] = error "empty list"
buildNodes expr nId [x] = [(nId, (x, getPrim (restrict expr x False), getPrim (restrict expr x True)))]
buildNodes expr nId (x:xs) =  (buildNodes (restrict expr x False) (nId*2) xs) ++ 
                              (buildNodes (restrict expr x True) (nId*2 + 1) xs) ++
                              [(nId, (x, nId*2, nId*2 + 1))]


getPrim :: (BExp) -> Int
getPrim x
    | x == Prim True = 1
    | otherwise = 0

-- Задача 6 -----------------------------------------
-- Передумова: Кожна змінна (індекс) в бульовому виразі (BExp) з"являється 
--    точно один раз в списку індексів (Index); немає інших елементів
buildROBDD :: BExp -> [Index] -> BDD
buildROBDD expr indexes = (fst bdd, deleteChecks (deleteDuplicateNodes (snd bdd) []) [])
             where bdd = buildBDD expr indexes


deleteDuplicateNodes :: [BDDNode] -> [BDDNode] -> [BDDNode]
deleteDuplicateNodes (x:xs) [] = deleteDuplicateNodes xs [x]
deleteDuplicateNodes (x:xs) ys 
    | checkIfNodePresent x ys = deleteDuplicateNodes (changeTuple x (getNode x ys) xs) 
                                                     (changeTuple x (getNode x ys) ys)
    | otherwise = deleteDuplicateNodes xs (ys ++ [x])
deleteDuplicateNodes [] ys = ys

checkIfNodePresent :: BDDNode -> [BDDNode] -> Bool
checkIfNodePresent node nodes = (length (filter ((snd node) `elem`) nodes)) /= 0

getNode :: BDDNode -> [BDDNode] -> BDDNode
getNode (_, (_, _, _)) [] = error "empty list"
getNode n (x:xs) = if (snd n) `elem` x then x else getNode n xs

changeTuple :: BDDNode -> BDDNode -> [BDDNode] -> [BDDNode]
changeTuple _ _ [] = []
changeTuple x y ((nz, (indz, lz, rz)):zs)
    | (fst x) == lz = [(nz, (indz, fst y, rz))] ++ changeTuple x y zs
    | (fst x) == rz = [(nz, (indz, lz,  fst y))] ++ changeTuple x y zs
    | otherwise = [(nz, (indz, lz, rz))] ++ changeTuple x y zs


deleteChecks :: [BDDNode] -> [BDDNode] -> [BDDNode]
deleteChecks (x:xs) [] = deleteChecks xs [x]
deleteChecks (x:xs) ys
 | ifDelete x = deleteChecks (changeTupleCheck x xs) (changeTupleCheck x ys)
 | otherwise = deleteChecks xs (ys ++ [x])
deleteChecks [] ys = ys

changeTupleCheck :: BDDNode -> [BDDNode] -> [BDDNode]
changeTupleCheck _ [] = []
changeTupleCheck x@(nx, (_, lx, rx)) ((ny, (indy, ly, ry)):ys) 
    | nx == ly = [(ny, (indy, lx, ry))] ++ changeTupleCheck x ys
    | nx == ry = [(ny, (indy, ly, rx))] ++ changeTupleCheck x ys
    | otherwise = [(ny, (indy, ly, ry))] ++ changeTupleCheck x ys

 
ifDelete :: BDDNode -> Bool
ifDelete (_, (_, l, r)) = l == r
 

------------------------------------------------------
-- Приклади для тестування..

b1, b2, b3, b4, b5, b6, b7, b8, b9 :: BExp
b1 = Prim False
b2 = Not (And (IdRef 1) (Or (Prim False) (IdRef 2)))
b3 = And (IdRef 1) (Prim True)
b4 = And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3)))
b5 = Not (And (IdRef 7) (Or (IdRef 2) (Not (IdRef 3))))
b6 = Or (And (IdRef 1) (IdRef 2)) (And (IdRef 3) (IdRef 4))
b7 = Or (Not (IdRef 3)) (Or (IdRef 2) (Not (IdRef 9)))
b8 = Or (IdRef 1) (Not (IdRef 1))
b9 = And (IdRef 3) (Or (IdRef 2) (And (Not (IdRef 2)) (IdRef 1)))

bdd1, bdd2, bdd3, bdd4, bdd5, bdd6, bdd7, bdd8, bdd9 :: BDD
bdd1 = (0,[])
bdd2 = (2,[(4,(2,1,1)),(5,(2,1,0)),(2,(1,4,5))])
bdd3 = (5,[(5,(1,0,1))])
bdd4 = (2,[(2,(2,4,5)),(4,(3,8,9)),(8,(7,0,1)),(9,(7,0,0)),
           (5,(3,10,11)),(10,(7,0,1)),(11,(7,0,1))])
bdd5 = (3,[(4,(3,8,9)),(3,(2,4,5)),(8,(7,1,0)),(9,(7,1,1)),
           (5,(3,10,11)),(10,(7,1,0)),(11,(7,1,0))])
bdd6 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,16,17)),(16,(4,0,0)),
           (17,(4,0,1)),(9,(3,18,19)),(18,(4,0,0)),(19,(4,0,1)),
           (5,(2,10,11)),(10,(3,20,21)),(20,(4,0,0)),(21,(4,0,1)),
           (11,(3,22,23)),(22,(4,1,1)),(23,(4,1,1))])
bdd7 = (6,[(6,(2,4,5)),(4,(3,8,9)),(8,(9,1,1)),(9,(9,1,0)), (5,(3,10,11)),(10,(9,1,1)),(11,(9,1,1))])
bdd8 = (2,[(2,(1,1,1))])
bdd9 = (2,[(2,(1,4,5)),(4,(2,8,9)),(8,(3,0,0)),(9,(3,0,1)),(5,(2,10,11)),(10,(3,0,1)),(11,(3,0,1))])



