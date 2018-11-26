{-# OPTIONS_GHC -Wall #-}
module Kozopas06 where

import Data.Maybe
import qualified Data.Map as M

-- Всі програми і їх елементи являються вірними (well-formed) в наступному значенні:
-- Всі оператори, функції і процедури застосовуються  
-- до вірної кількості аргументів, кожний з яких має відповідний тип.
-- Вирази, які повинні обчислювати логічні значення, будуть завжди обчислюватися
-- до 0 (false) або 1 (true).
-- В присвоєнні масиву  a[i] = e масив завжди визначений (в області дії).
-- Процедура, що викликається як x := p(e1, …, en), завжди буде повертати значення 
-- (закінчує своє обчислення оператором return e) 
-- Оператор return завжди останній оператор для виконання в блоку процедури 
-- (тобто немає "мертвого коду")

--------------------------------------------------------------------
type Id = String
data Value = I Int | A [(Int, Int)]
           deriving (Eq, Show)
data Op = Add | Minus | Mul | Less | Equal | Index
          deriving (Eq, Show)
data Exp = Const Value | 
           Var Id | 
           OpApp Op Exp Exp |
           Cond Exp Exp Exp |
           FunApp Id [Exp] 
         deriving (Eq, Show)

data VarDef = Arr Id | Int Id   deriving (Eq, Show)
type FunDef = (Id, ([VarDef], Exp))

type Binding = M.Map Id Value
type StateP = ([Binding],Binding)
-- st = ([locn,.. loc1], glob)  стек локальних записів активацій + глобальний запис активації

data Statement = Assign Id Exp |
                 AssignA Id Exp Exp |
                 If Exp Block Block |
                 While Exp Block |
                 Call Id Id [Exp] |
                 Return Exp 
               deriving (Eq, Show)

type Block     = [Statement]
type ProcDef   = (Id, ([VarDef], Block))
type Program   = ([VarDef], [FunDef], [ProcDef])

-- Задача 1 -----------------------------------------
getValue ::  Id -> StateP -> Value
-- Передумова: Значення змінної Id є в стані StateP
getValue v (local, global) = case inLocal v local of
                              Nothing -> fromJust(M.lookup v global)
                              Just m -> m

inLocal :: Id -> [Binding] -> Maybe Value
inLocal _ [] = Nothing
inLocal v locals = case M.lookup v (head locals) of
                        Nothing -> inLocal v (tail locals)
                        Just m -> Just m

-- Задача 2 -----------------------------------------
getLocals :: StateP -> Binding
getLocals (local, _) = head local

getGlobals :: StateP -> Binding
getGlobals (_, global) = global 

-- Задача 3 -----------------------------------------
assignArray :: Value -> Value -> Value -> Value
-- Аргументи - масив, індекс і (нове) значення відповідно
-- Передумова: Три аргумента (Value)  мають значення відповідного типу  
--     (масив (A),  ціле (I) і ціле (I)) відповідно.
assignArray arr i v = A (assignArray2 arr i v)

assignArray2 :: Value -> Value -> Value -> [(Int,Int)]
assignArray2 (I _) _ _ = error "not array"
assignArray2 (A _) (I _) (A _) = error "not value"
assignArray2 (A _) (A _) _ = error "not index"
assignArray2 (A arr) (I i) (I v) 
    | null arr = [(i,v)]
    | otherwise =  if fst (head arr) == i then [(i, v)] ++ (tail arr)
                   else [head arr] ++ (assignArray2 (A (tail arr)) (I i) (I v))


-- Задача 4 -----------------------------------------
updateVar :: (Id, Value) -> StateP -> StateP
updateVar (var, value) st = if (elemState var st) then changeVar (var, value) st
                            else addNewLocal (var, value) st

elemState :: Id -> StateP -> Bool
elemState varName (local, global) = varName `elem` (concat [M.keys x | x <- local] ++ M.keys global)

changeVar :: (Id, Value) -> StateP -> StateP
changeVar (name, value) (local, global)
    | isJust (inLocal name local) = (addToLocal (name, value) local, global)
    | otherwise = (local, M.insert name value global)

addToLocal :: (Id, Value) -> [Binding] -> [Binding]
addToLocal (name, value) local = if name `elem` M.keys (head local) 
                                 then [M.insert name value (head local)] ++ (tail local)
                                 else [head local] ++ addToLocal (name, value) (tail local)

addNewLocal :: (Id, Value) -> StateP -> StateP
addNewLocal (var, val) (local, global) = ([M.insert var val (head local)] ++ (tail local), global)


-- Задача 5 -----------------------------------------
applyOp :: Op -> Value -> Value -> Value
-- Передумова: Значення мають відповідні типи (I або A) для кожної операції
applyOp Add (I x) (I y) = I (x + y)
applyOp Minus (I x) (I y) = I (x - y)
applyOp Mul (I x) (I y) = I (x * y)
applyOp Less  (I x) (I y) = if x < y then (I 1) else (I 0)
applyOp Equal (I x) (I y) = if x == y then (I 1) else (I 0)
applyOp Index (A x) (I y)
    | null x = (I 0)
    | otherwise =  if fst (head x) == y then (I (snd (head x)))
                   else applyOp Index (A (tail x)) (I y)

applyOp Add (I _) (A _) = error "error"
applyOp Add (A _) (_) = error "error"
applyOp Minus (I _) (A _) = error "error"
applyOp Minus (A _) (_) = error "error"
applyOp Mul (I _) (A _) = error "error"
applyOp Mul (A _) (_) = error "error"
applyOp Less (I _) (A _) = error "error"
applyOp Less (A _) (_) = error "error"
applyOp Equal (I _) (A _) = error "error"
applyOp Equal (A _) (_) = error "error"
applyOp Index (I _) (_) = error "error"
applyOp Index (A _) (A _) = error "error"


-- Задача 6 -----------------------------------------
bindArgs :: [Id] -> [Value] -> Binding
-- Передумова: списки мають однакову довжину
bindArgs idlist vallist  = M.fromList (zip idlist vallist)

-- Задача 7 -----------------------------------------
eval :: Exp -> [FunDef] -> StateP -> Value
eval (Const cst) _ _ = cst
eval (Var v) _ st = getValue v st
eval (Cond cnd r1 r2) dfx st
    |eval cnd dfx st == (I 1) = eval r1 dfx st
    |eval cnd dfx st == (I 0) = eval r2 dfx st
    |otherwise = error "not Bool"
eval (OpApp op x y) dfx st = applyOp op (eval x dfx st) (eval y dfx st)

eval (FunApp fname exprSeq) dfx st = eval expression dfx newState
                                     where functionR = getFunction fname dfx
                                           expression = getExpr functionR
                                           newState = createNewState (getArgs functionR) (evalArgs exprSeq dfx st) st

getFunction :: Id -> [FunDef] -> FunDef
getFunction _ [] = error "function not found"
getFunction fname dfx = if fst (head dfx) == fname then head dfx else getFunction fname (tail dfx)

getArgs :: FunDef -> [Id]
getArgs (_, (vardf, _)) = map getId vardf 

getId :: VarDef -> Id
getId (Arr x) = x
getId (Int x) = x

getExpr :: FunDef -> Exp
getExpr (_, (_, expr)) = expr

createNewState :: [Id] -> [Value] -> StateP -> StateP
createNewState ids vals (local, global) = ([bindArgs ids vals] ++ local, global)


evalArgs :: [Exp] -> [FunDef] -> StateP -> [Value]
evalArgs ex dfx st = [eval x dfx st | x <- ex]

-- Задача 8 -----------------------------------------
-- data Statement = Assign Id Exp |
--                  AssignA Id Exp Exp |
--                  If Exp Block Block |
--                  While Exp Block |
--                  Call Id Id [Exp] |
--                  Return Exp 
--                deriving (Eq, Show)

-- data VarDef = Arr Id | Int Id   deriving (Eq, Show)
-- type FunDef = (Id, ([VarDef], Exp))

-- type Block     = [Statement]
-- type ProcDef   = (Id, ([VarDef], Block))

executeStatement :: Statement -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeStatement (Assign x y) dfx _ st = updateVar (x, eval y dfx st) st
executeStatement (AssignA x i y) dfx _ st = updateVar (x, assignArray arrName index arrVal) st
                                              where arrName = getValue x st
                                                    index = eval i dfx st
                                                    arrVal = eval y dfx st

executeStatement (If expr b1 b2) dfx dpx st = if (eval expr dfx st) == (I 1)
                                              then executeBlock b1 dfx dpx st
                                              else executeBlock b2 dfx dpx st

executeStatement (While expr b1) dfx dpx st = if (eval expr dfx st) == (I 1)
                                              then executeStatement (While expr b1) dfx dpx (executeBlock b1 dfx dpx st)
                                              else st

executeStatement (Return expr) dfx _ st = updateVar ("$res", eval expr dfx st) st

executeStatement (Call id1 id2 arrExp) dfx dpx st = if id1 == ""
                                                    then cutLocalState (executeBlock block dfx dpx newSt)
                                                    else cutLocalStateAndAdd id1 (executeBlock block dfx dpx newSt)
                                                        where block = getBlock (getProcedure id2 dpx)
                                                              ids = getArgsProcNames (getProcedure id2 dpx)
                                                              vals = evalArgs arrExp dfx st
                                                              newSt = createNewPState ids vals st

getProcedure :: Id -> [ProcDef] -> ProcDef
getProcedure _ [] = error "procedure not found"
getProcedure pname dpx = if fst (head dpx) == pname then head dpx else getProcedure pname (tail dpx)

getArgsProcNames :: ProcDef -> [Id]
getArgsProcNames (_, (vardf, _)) = map getId vardf

getBlock :: ProcDef -> Block
getBlock (_, (_, block)) = block

createNewPState :: [Id] -> [Value] -> StateP -> StateP
createNewPState ids vals (local, global) = ([bindArgs ids vals] ++ local, global)

cutLocalState :: StateP -> StateP
cutLocalState (local, global) = (tail local, global)

cutLocalStateAndAdd :: Id -> StateP -> StateP
cutLocalStateAndAdd name (local, global) = if name `elem` (concat [M.keys x | x <- (tail local)] ++ M.keys global)
 -- if name `elem` M.keys global
                                           then (tail local, global)
                                           else ([M.insert name (getValue "$res" (local, global)) M.empty] ++ (tail local), global)
                                               

executeBlock :: Block -> [FunDef] -> [ProcDef] -> StateP -> StateP
executeBlock [] _ _ st = st
executeBlock block dfx dpx st = executeBlock (tail block) dfx dpx (executeStatement (head block) dfx dpx st)

---------------------------------------------------------------------
-- Допоміжні функції і дані для тестування...
-- Функція пошуку...
lookUp :: (Eq a, Show a) => a -> [(a, b)] -> b
lookUp x t = fromMaybe (error ("\nНе знайдено  " ++ show x )) 
              (lookup x t)

-- Стан для тестування
sampleState :: StateP
sampleState = ([M.fromList [("x",I 5)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

sampleState1 :: StateP
sampleState1 = ([M.fromList [("x",I 1)]], M.fromList [("y",I 2),("a", listToVal [4,2,7])])

-- Перетворює список цілих в масив Value...
listToVal :: [Int] -> Value
listToVal xs = A (zip [0..] xs)

 -- Перетворює ціле в Exp...
intToExp :: Int -> Exp
intToExp n = Const (I n)

-- Реалізація виконання програми 
program :: Program -> StateP 
program (dvx, dfx, dpx) = 
   let initv :: VarDef -> (Id, Value)
       initv (Arr v) = (v, A [])
       initv (Int v) = (v, I 0) 
       gl = M.fromList (map initv dvx) 
   in executeStatement (Call "" "main" []) dfx dpx ([],gl)

-- fib чиста функція
-- Функція fib, що обчислює число Фібоначчі
-- func  fib(n) =
--     (n < 3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
     ([Int "n"], Cond (OpApp Less (Var "n") (Const (I 3)))
                  (Const (I 1))
                  (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const (I 1))])
                             (FunApp "fib" [OpApp Minus (Var "n") (Const (I 2))]))
     )
    )

-- Масив
sampleArray :: Exp
sampleArray = Const (listToVal [9,5,7,1])

-- Сума елементів масиву 0..n ...
sumA1 :: ProcDef
sumA1 = ("sumA1",
     ([Arr "a", Int "n"], 
                  [Assign "s" (Const (I 0)),
                   Assign "i" (Const (I 0)),
                   Assign "limit" (OpApp Add (Var "n") (Const (I 1))),
                   While (OpApp Less (Var "i") (Var "limit"))
                         [Assign "s" (OpApp Add (Var "s") 
                                                (OpApp Index (Var "a") (Var "i"))),
                          Assign "i" (OpApp Add (Var "i") (Const (I 1)))
                         ],
                   Return (Var "s")]
     )
    )

-- Додавання двох чисел...
gAdd :: ProcDef
gAdd = ("gAdd", 
     ([Int "x", Int "y"], [Assign "gSum" (OpApp Add (Var "x") (Var "y"))])
    )

-- Повна програма
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],[Call "" "gAdd" [intToExp 5, intToExp 10] ]))])




-- executeBlock  [Call "s" "sumA1" [sampleArray, intToExp 3]]   [] [sumA1] ([M.fromList [("s", I 0)]], M.fromList [("x", I 0)])

-- executeBlock  [Call "s" "sumA1" [sampleArray, intToExp 3]]   [] [sumA1] ([M.fromList [(“s”, I 0)]], M.fromList [(“s”, I 0)])   