{-# OPTIONS_GHC -Wall #-}
module Kozopas03 where

data BinTree a = EmptyB 
                | Node a (BinTree a) (BinTree a)
                   deriving (Show, Eq)
data Tree23 a  = Leaf a   
               | Node2 (Tree23 a) a (Tree23 a) 
               | Node3 (Tree23 a) a (Tree23 a) a (Tree23 a)
               | Empty23     -- empty 2-3-tree!!!
                   deriving (Eq, Show)

isEmpty :: (Ord a) => BinTree a -> Bool
isEmpty EmptyB = True
isEmpty _ = False

-- Task 1 -----------------------------------------		
isSortedList :: (Ord a) => [a] -> Bool
isSortedList xs = and $ zipWith (<=) xs (tail xs)

hasDuplicates :: (Ord a) => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs)
    | elem x xs = True
    | otherwise = hasDuplicates xs

isSearch :: (Ord a) => BinTree a -> Bool
isSearch tree = isSortedList(treeList) && not (hasDuplicates treeList)
    where treeList = treeToList tree

-- Task 2-----------------------------------------
elemSearch :: (Ord a) => BinTree a -> a -> Bool
elemSearch EmptyB _ = False
elemSearch (Node a lt rt) x
    | x == a = True
    | x < a = elemSearch lt x
    | otherwise = elemSearch rt x

-- Task 3 -----------------------------------------
insSearch :: (Ord a) => BinTree a -> a -> BinTree a 
insSearch EmptyB a = Node a EmptyB EmptyB
insSearch (Node a lt rt) x
    | x == a = Node a lt rt
    | x < a = Node a (insSearch lt x) rt
    | otherwise = Node a lt (insSearch rt x)

-- Task 4 -----------------------------------------
delSearch :: (Ord a) => BinTree a -> a -> BinTree a 
delSearch EmptyB _ = EmptyB
delSearch (Node a lt rt) x
    | x == a = deleteRoot (Node a lt rt)
    | x < a = Node a (delSearch lt x) rt
    | otherwise = Node a lt (delSearch rt x)

deleteRoot :: (Ord a) => BinTree a -> BinTree a
deleteRoot EmptyB = error "Empty tree in deleteRoot"
deleteRoot (Node _ lt rt)
    | isEmpty lt = rt
    | isEmpty rt = lt
    | otherwise = Node new_root lt (delSearch rt new_root) where new_root = rightTreeLeftElement rt

rightTreeLeftElement :: (Ord a) => BinTree a -> a
rightTreeLeftElement EmptyB = error "Empty tree in rightTreeLeftElement"
rightTreeLeftElement (Node v EmptyB _) = v
rightTreeLeftElement (Node _ lt _) = rightTreeLeftElement lt

-- Task 5 -----------------------------------------
treeToList :: (Ord a) => BinTree a -> [a]
treeToList EmptyB = []
treeToList (Node v lt rt) = treeToList(lt) ++ [v] ++ treeToList(rt)

sortList :: (Ord a) => [a] -> [a]
sortList a = treeToList (foldl (insSearch) EmptyB a)

-- Task 6-----------------------------------------
isLeaf :: (Ord a) => Tree23 a -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

isBalancedTree23 :: (Ord a) => Tree23 a -> Bool
isBalancedTree23 (Leaf _) = True
isBalancedTree23 Empty23 = True
isBalancedTree23 (Node2 lt _ rt)
    | (isLeaf lt) && not (isLeaf rt) = False
    | not (isLeaf lt) && (isLeaf rt) = False
    | (isLeaf lt) && (isLeaf rt) = True
    | otherwise = (isBalancedTree23 lt) && (isBalancedTree23 rt)
isBalancedTree23 (Node3 lt _ mt _ rt)
    | not (isLeaf lt) && (isLeaf mt) && (isLeaf rt) = False
    | (isLeaf lt) && not (isLeaf mt) && (isLeaf rt) = False
    | (isLeaf lt) && (isLeaf mt) && not (isLeaf rt) = False
    | (isLeaf lt) && (isLeaf mt) && (isLeaf rt) = True
    | otherwise = (isBalancedTree23 lt) && (isBalancedTree23 mt) && (isBalancedTree23 rt)

minInRightTree :: (Ord a) => Tree23 a -> a
minInRightTree (Leaf v) = v
minInRightTree (Empty23) = error "Empty tree in minInRightTree"
minInRightTree (Node2 lt _ _) = minInRightTree lt
minInRightTree (Node3 lt _ _ _ _) = minInRightTree lt

isTree23  :: (Ord a) => Tree23 a -> Bool 
isTree23 (Empty23) = True
isTree23 (Leaf _) = True
isTree23 (Node2 lt k rt) = isBalancedTree23 (Node2 lt k rt) && (k == (minInRightTree rt))
isTree23 (Node3 lt k1 mt k2 rt) = isBalancedTree23 (Node3 lt k1 mt k2 rt) && (k1 == (minInRightTree mt)) &&(k2 == (minInRightTree rt))


-- Task 7-----------------------------------------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 Empty23 _ = False
elemTree23 (Leaf k) a = k == a
elemTree23 (Node2 lt k rt) a
    | a == k = True
    | a < k = elemTree23 lt a
    | otherwise = elemTree23 rt a
elemTree23 (Node3 lt k1 mt k2 rt) a
    | a == k1 = True
    | a == k2 = True
    | a < k1 = elemTree23 lt a
    | a > k1 && a < k2 = elemTree23 mt a
    | otherwise = elemTree23 rt a

-- Task 8-----------------------------------------
tree23ToList :: (Ord a) => Tree23 a -> [a]
tree23ToList Empty23 = []
tree23ToList (Leaf a) = [a]
tree23ToList (Node2 lt a rt) = tree23ToList(lt) ++ [a] ++ tree23ToList(rt)
tree23ToList (Node3 lt a mt b rt) = tree23ToList(lt) ++ [a] ++ tree23ToList(mt)  ++ [b] ++ tree23ToList(rt)

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = (tree23ToList a) == (tree23ToList b)

-- Task 9-----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 (Empty23) a = (Leaf a)
insTree23 (Leaf v) a
 | a == v = (Leaf v)
 | a < v = Node2 (Leaf a) v (Leaf v)
 | otherwise = Node2 (Leaf v) a (Leaf a)
insTree23 (Node2 lt v rt) a
 | a == v = (Node2 lt v rt)
 | a < v = balanceNode2 (insTree23 lt a) v rt
 | otherwise = balanceNode2 lt v (insTree23 rt a)
insTree23 (Node3 lt v1 mt v2 rt) a
 | a == v1 = Node3 lt v1 mt v2 rt
 | a == v2 = Node3 lt v1 mt v2 rt
 | a < v1 = balanceNode3 (insTree23 lt a) v1 mt v2 rt
 | a > v2 = balanceNode3 lt v1 mt v2 (insTree23 rt a)
 | otherwise = balanceNode3 lt v1 (insTree23 mt a) v2 rt

balanceNode2 :: (Ord a) => Tree23 a -> a -> Tree23 a -> Tree23 a
balanceNode2 (Node2 t1 v1 t2) v2 t3 = Node3 t1 v1 t2 v2 t3
balanceNode2 t1 v1 (Node2 t2 v2 t3) = Node3 t1 v1 t2 v2 t3

balanceNode2 (Leaf l1) v (Leaf l2) = Node2 (Leaf l1) v (Leaf l2)
balanceNode2 lt v rt = Node2 lt v rt

balanceNode3 :: (Ord a) => Tree23 a -> a -> Tree23 a -> a -> Tree23 a -> Tree23 a
balanceNode3 t1 v1 t2 v2 (Node2 t3 v3 t4) = Node2 (Node2 t1 v1 t2) v2 (Node2 t3 v3 t4)
balanceNode3 t1 v1 (Node2 t2 v2 t3) v3 t4 = Node2 (Node2 t1 v1 t2) v2 (Node2 t3 v3 t4)
balanceNode3 (Node2 t1 v1 t2) v2 t3 v3 t4 = Node2 (Node2 t1 v1 t2) v2 (Node2 t3 v3 t4)

balanceNode3 (Leaf l1) v1 (Leaf l2) v2 (Leaf l3) = Node3 (Leaf l1) v1 (Leaf l2) v2 (Leaf l3)
balanceNode3 lt v1 mt v2 rt = Node3 lt v1 mt v2 rt


{-
-- isTerminal tr = True <=> if son of tr - leaves !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Node2 (Leaf _) _ _)     = True 
isTerminal (Node3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False


-- insTerm v tr - додається значення v в дерево tr з коРнем - термінальний вузол 
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm a (Node2 (Leaf l) _ (Leaf r))
    | a < l = (Node3 (Leaf a) l (Leaf l) r (Leaf r), Nothing)
    | a > r = (Node3 (Leaf l) r (Leaf r) a (Leaf a), Nothing)
    | otherwise = (Node3 (Leaf l) a (Leaf a) r (Leaf r), Nothing)
insTerm a (Node3 (Leaf l) _ (Leaf m) _ (Leaf r))
    | a < l = ((Node2 (Leaf a) l (Leaf l)), Just (m, (Node2 (Leaf m) r (Leaf r))))
    | a > l && a < m = ((Node2 (Leaf l) a (Leaf a)), Just (m, (Node2 (Leaf m) r (Leaf r))))
    | a > m && a < r = ((Node2 (Leaf l) m (Leaf m)), Just (a, (Node2 (Leaf a) r (Leaf r))))
    | otherwise = ((Node2 (Leaf l) m (Leaf m)), Just (r, (Node2 (Leaf r) a (Leaf a))))

-- insNode v tr - додає значення v в дерево tr з корнем - нетермінальний вузол 
insNode :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insNode  = undefined


--   Результат вставки вузла в 2-3-дерево, 
--   корінь якого - вузол вида Node2 або Node3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a 
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довілье дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr | isTerminal tr = insTerm v tr
            | otherwise     = insNode v tr
-}


---  Бінарні дерева 
bt1, bt2, bt3 ::  BinTree Int
bt1 = Node 9 (Node 4 EmptyB 
                     (Node 8 EmptyB EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                      EmptyB)
bt2 = Node 9 (Node 4 EmptyB 
                     (Node 8 (Node 6 EmptyB EmptyB)
                             EmptyB))
             (Node 20 (Node 10 EmptyB EmptyB) 
                       EmptyB)
bt3 = EmptyB

---- 2-3-tree -----
tr1, tr2, tr3, tr4,tr5 :: Tree23 Int
tr1 =  Node2 (Node2 (Node2 (Leaf 0) 1 (Leaf 1)) 
                     2
                    (Node2 (Leaf 2) 3 (Leaf 3)))
              4
             (Node2 (Node2 (Leaf 4) 5 (Leaf 5)) 
                     6
                    (Node2 (Leaf 6) 7 (Leaf 7)))
tr2 =  Node3 (Node2 (Leaf 0) 1 (Leaf 1))
              2
             (Node3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4))
              5
             (Node3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node2 (Leaf 16) 19 (Leaf 19))

tr4 = Node3 (Node2 (Leaf 2) 5 (Leaf 5))
            7
            (Node3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
            16
            (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 = Node2 (Node2 (Node2 (Leaf 2) 5 (Leaf 5))
                    7
                   (Node2 (Leaf 7) 8 (Leaf 8)) 
            )
            10
            (Node2 (Node2 (Leaf 10) 12 (Leaf 12))
                   16
                   (Node3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))
            )