{-# OPTIONS_GHC -Wall #-}
module Kozopas05 where

data AbstractInteger = Zero
                     | Succ AbstractInteger
                     | Pred AbstractInteger
                     deriving (Show, Eq)

-- for testing
ai1, ai2 ::AbstractInteger
ai1 = Pred (Pred (Pred Zero))
ai2 = Succ (Succ Zero)

isNormalized :: AbstractInteger -> Bool
isNormalized (Succ (Pred _)) = False
isNormalized (Pred (Succ _)) = False
isNormalized (Zero) = True
isNormalized (Succ Zero) = True
isNormalized (Pred Zero) = True
isNormalized (Succ x) = isNormalized x
isNormalized (Pred x) = isNormalized x

normalize :: AbstractInteger -> AbstractInteger
normalize ai
    | isNormalized ai = ai
    | otherwise = normalize (norm ai) where
                  norm (Zero) = Zero
                  norm (Pred (Succ x)) = norm x
                  norm (Succ (Pred x)) = norm x
                  norm (Succ x) = (Succ (norm x))
                  norm (Pred x) = (Pred (norm x))

-- Задача 1 -----------------------------------------
instance Ord AbstractInteger where
    (<=) (Zero) (Zero) = True
    (<=) (Zero) (Pred _) = False
    (<=) (Zero) (Succ _) = True
    (<=) (Pred _) (Zero)  = True
    (<=) (Succ _) (Zero) = False
    (<=) (Succ x) (Succ y) = (<=) x y
    (<=) (Pred x) (Pred y) = (<=) x y
    (<=) (Succ _) (Pred _) = False
    (<=) (Pred _) (Succ _) = True

   
-- Задача 2 ----------------------------------------
aiToInteger :: AbstractInteger -> Integer
aiToInteger (Zero) = 0
aiToInteger (Succ a) = 1 + aiToInteger a
aiToInteger (Pred a) = -1 + aiToInteger a
 
-- Задача 3 -----------------------------------------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs a1 a2 = normalize (plus a1 a2) where
                  plus Zero Zero = 0
                  plus Zero x = x
                  plus x Zero = x
                  plus x (Succ y) = plusAbs (Succ x) y
                  plus x (Pred y) = plusAbs (Pred x) y

-- Задача 4 -----------------------------------------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs Zero _ = Zero
timesAbs _ Zero = Zero
timesAbs (Succ x) (Succ y) = plusAbs (Succ x) (timesAbs (Succ x) y)
timesAbs (Pred x) (Pred y) = timesAbs (negate (Pred x)) (negate (Pred y))
timesAbs (Pred x) (Succ y) = plusAbs (Pred x) (timesAbs (Pred x) y)
timesAbs (Succ x) (Pred y) = plusAbs (Pred y) (timesAbs (Pred y) x)

-- Задача 5 -----------------------------------------
instance Num AbstractInteger where
    (+)   = plusAbs
    (*)   = timesAbs
    negate = negateImpl
    fromInteger n
        | n == 0 = Zero
        | n < 0 = (Pred (fromInteger (n+1)))
        | otherwise = (Succ (fromInteger (n-1)))
    abs         = absImpl
    signum      = signumImpl

negateImpl :: AbstractInteger -> AbstractInteger
negateImpl Zero = Zero
negateImpl (Succ x) = Pred (negateImpl x)
negateImpl (Pred x) = Succ (negateImpl x)

absImpl :: AbstractInteger -> AbstractInteger
absImpl Zero = Zero
absImpl (Succ x) = Succ x
absImpl (Pred x) = negate (Pred x)

signumImpl :: AbstractInteger -> AbstractInteger
signumImpl x
    | x < Zero = (Pred Zero)
    | x == Zero = Zero
    |otherwise = (Succ Zero)


-- Задача 6 -----------------------------------------
factorial :: (Eq a, Num a) => a -> a
factorial x
    | x == 0 = 1
    | otherwise = x * factorial (x-1)

-- Задача  7 -----------------------------------------
data Quaternion = Quaternion Double Double Double Double deriving (Eq)

instance Show Quaternion where
    show (Quaternion a b c d) = show a ++ getSign b ++ show b ++ "i" ++ getSign c ++ show c ++ "j" ++ getSign d ++ show d ++ "k" where
                                getSign x
                                   | x >= 0 = "+"
                                   | otherwise = ""

-- Задача 8 -----------------------------------------
plusQuaternion :: Quaternion -> Quaternion -> Quaternion
plusQuaternion (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = (Quaternion (a1+a2) (b1+b2) (c1+c2) (d1+d2))

-- Задача 9 -----------------------------------------
timesQuaternion :: Quaternion -> Quaternion -> Quaternion
timesQuaternion (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = (Quaternion
    (a1*a2 - b1*b2 - c1*c2 - d1*d2)
    (a1*b2 + b1*a2 + c1*d2 - d1*c2)
    (a1*c2 - b1*d2 + c1*a2 + d1*b2)
    (a1*d2 + b1*c2 - c1*b2 + d1*a2))

--- Задача 10 ----------------------------------------
instance Num Quaternion  where
    (+)   = plusQuaternion
    (*)   = timesQuaternion
    negate (Quaternion a b c d) = (Quaternion (negate a) (negate b) (negate c) (negate d))
    fromInteger n = (Quaternion (fromInteger n) 0 0 0)
    abs (Quaternion a b c d) = (Quaternion (sqrt(a*a + b*b + c*c + d*d)) 0 0 0)
    signum (Quaternion a b c d) = (Quaternion (a/m) (b/m) (c/m) (d/m)) where
                                   m = sqrt(a*a + b*b + c*c + d*d)
