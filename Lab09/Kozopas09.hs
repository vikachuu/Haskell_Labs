{-# OPTIONS_GHC -Wall #-}
module Kozopas09 where

import Data.List

data RE = Null   |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)

-- Задача 1 -----------------------------------------
simplify :: RE -> RE
simplify Null = Null
simplify (Term a) = Term a
simplify (Seq a b) = Seq (simplify a) (simplify b)
simplify (Alt a b) = Alt (simplify a) (simplify b)
simplify (Rep a) = Rep (simplify a)
simplify (Plus a) = Seq (simplify a) (Rep (simplify a))
simplify (Opt a) = Alt (simplify a) Null

-- Задача 2 -----------------------------------------
startState     :: Automation -> State
terminalStates :: Automation -> [State]
transitions    :: Automation -> [Transition] 

startState (st, _, _) = st
terminalStates (_, tst, _) = tst
transitions (_, _, tr) = tr

-- Задача 3 -----------------------------------------
isTerminal :: State -> Automation -> Bool 
isTerminal st (_, tst, _) = st `elem` tst

-- Задача 4 -----------------------------------------
transitionsFrom :: State -> Automation -> [Transition]
transitionsFrom st (_, _, tr) = [(fst3 x, snd3 x, thd3 x) | x <- tr, fst3 x == st]

-- Задача 5 -----------------------------------------
labels :: [Transition] -> [Label]
labels tr = nub [thd3 x | x <- tr, thd3 x /= Eps]

-- Задача 6 -----------------------------------------
stStep  :: Automation -> State -> Label -> [State]
setStep :: Automation -> [State] -> Label -> [State]
closure :: Automation -> [State] -> [State]

stStep (_, _, tr) state lb = [snd3 x | x <- tr, thd3 x == lb, fst3 x == state]

setStep (_, _, tr) stArr lb = [snd3 x | x <- tr, st <- stArr, thd3 x == lb, fst3 x == st]

closure (_, _, tr) stArr = [snd3 x | x <- tr, st <- stArr, thd3 x == Eps, fst3 x == st]

-- Задача 7 -----------------------------------------
accepts :: Automation -> String -> Bool
accepts naut@(st, tst, _) str = or [comp x tst | x <- (accepts' naut str (closure naut [st]))]
                                 where comp xs ys = or [x `elem` ys | x <- xs]

accepts' :: Automation -> String -> [State] -> [[State]]
accepts' _ [] st = [st]
accepts' naut (x:xs) startStates  =    if null (closure naut startStates)
                                       then if null tempStates
                                            then []
                                            else [startStates] ++ accepts' naut xs tempStates
                                       else [startStates] ++ accepts' naut (x:xs) (closure naut startStates)
                                       where tempStates = (setStep naut startStates (C x)) 

-- Задача 8 -----------------------------------------
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make Null start end next = ([(start, end, Eps)],  next)
make (Term ch) start end next = ([(start, end, C ch)],  next)
make (Seq r1 r2) start end next = let recur1 = make r1 start next (next+2)
                                      recur2 = make r2 (next+1) end (snd recur1)
                                  in (fst recur1 ++ [(next, next+1, Eps)] ++ fst recur2, snd recur2)

make (Rep r1) start end next = let recur = make r1 next (next + 1) (next+2)
                               in ([(start, end, Eps)] ++ [(start, next, Eps)] 
                                ++ fst recur ++ [((next + 1), next, Eps)] 
                                ++ [(next + 1, end, Eps)], (snd recur))

                                
make (Alt r1 r2) start end next = let recur1 = make r1 next (next + 1) (next+4)
                                      recur2 = make r2 (next + 2) (next + 3) (snd recur1)
                                  in ([(start, next, Eps)] 
                                    ++ fst recur1 ++ [(next+1, end, Eps)] 
                                    ++ [(start, next+2, Eps)] 
                                    ++ fst recur2 ++ [(next+3, end, Eps)], snd recur2)

make (Plus _) _ _ _ = error "simplify error"
make (Opt _) _ _ _ = error "simplify error"

-- Задача 9 -----------------------------------------
getFrontier :: State -> Automation -> [Transition]
getFrontier st aut = getFrontier' st aut (transitionsFrom st aut)
  
getFrontier' :: State -> Automation -> [Transition] -> [Transition]
getFrontier' st aut []
     | isTerminal st aut = [(st, st, Eps)]
     | otherwise      = []
getFrontier' st aut ((f, t, l) : ts)
     | isTerminal t aut && l == Eps = (t, t, Eps) : getFrontier' st aut ts
     | l == Eps = getFrontier' st aut (transitionsFrom t aut ++ ts)
     | otherwise = (f, t, l) : getFrontier' st aut ts      

createMetaState :: [Transition] -> MetaState
createMetaState tr = sort . nub $ [s | (s, _, _) <- tr]

groupTransitions :: [Transition] -> [(Label, [State])]
groupTransitions tr = [(l, [st | (_, st, lbl) <- tr, l == lbl]) | l <- labels tr]


extendDA' :: Automation -> MetaState -> [(Label, [State])] -> [MetaState] -> [MetaTransition] -> (MetaState, [MetaState], [MetaTransition])
extendDA' aut ms gts mss mts = foldl extend (ms, ms : mss, mts) gts
          where extend (_, mss1, mts1) (label, ss) = (ms, mss', (ms, ms', label) : mts')
                 where (ms', mss', mts') = makeDA' aut ss mss1 mts1


makeDA' :: Automation -> [State] -> [MetaState] -> [MetaTransition] -> (MetaState, [MetaState], [MetaTransition])
makeDA' aut st mst mtr
    | ms `elem` mst = (ms, mst, mtr)
    | otherwise = extendDA' aut ms groupTr mst mtr
      where ts = concatMap (flip getFrontier aut) st
            ms = createMetaState ts
            groupTr = groupTransitions ts

makeDA :: Automation -> Automation
makeDA aut@(st, tst, _) = (1, termSt, trans)
                           where (_, metaSts, metaTr) = makeDA' aut [st] [] []
                                 trans = convertTransitions metaTr temp
                                 termSt = nub (map (flip lookUp temp) (finalMetaStates (reverse metaSts) tst))
                                 temp = (zip (reverse metaSts) [1..])

convertTransitions :: [MetaTransition] -> [(MetaState, State)] -> [Transition]
convertTransitions metaTr mstst = map (\(ms1, ms2, l) -> (lookUp ms1 mstst, lookUp ms2 mstst, l)) metaTr

finalMetaStates :: [MetaState] -> [State] -> [MetaState]
finalMetaStates metaSts = foldr (\sf -> (++) (filter (\ ms -> sf `elem` ms) metaSts)) []

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xs = fromJust (lookup x xs)

fromJust :: Maybe a -> a
fromJust Nothing  = error "Nothing"
fromJust (Just x) = x

-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigure, re1, re2, re3, re4, re5 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Alt (Term 'a') Null) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])

daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )

-- Help functions -----------------------------------
fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

snd3 :: (a,b,c) -> b
snd3 (_, y, _) = y

thd3 :: (a,b,c) -> c
thd3 (_, _, z) = z
