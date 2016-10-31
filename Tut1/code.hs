

-- f muss monoton wachsend sein.
-- Es wird nach der größten Zahl x aus [lowerBound, upperBound] gesucht, sodass f x <= y
invert :: (Integer -> Integer) -> Integer -> Integer -> Integer-> Integer
invert f lowerBound upperBound y
    | lowerBound > upperBound = error "Interval is empty"
    | lowerBound == upperBound = if f lowerBound <= y then lowerBound else error "Interval too small"
    | f mid <= y = invert f mid upperBound y
    | f mid > y = invert f lowerBound (mid-1) y
        -- Annahme lowerBound < mid <= upperBound wenn lowerBound != upperBound 
        where mid = (1 + lowerBound + upperBound) `div` 2

root x = invert (^2) 1 x x


insert :: [Int] -> Int -> [Int]
insert (x:xs) e 
    | e < x = e:x:xs
    | otherwise = x:insert xs e

insertSort :: [Int] -> [Int]
insertSort = foldl insert []


data Term = Add Term Term | Mult Term Term 
    | Var String | Const Integer | Minus Term Term

-- showsPrec arbeitet mit einem Akkumulator
instance Show Term where
    showsPrec i t str = showTerm i t ++ str

showTerm :: Int -> Term -> String
showTerm i (Add a b)
    | i <= 1 = showTerm 1 a ++ " + " ++ showTerm 1 b
    | otherwise = "(" ++ showTerm 1 (Add a b) ++ ")"
showTerm i (Mult a b) = showTerm 2 a ++ " * " ++ showTerm 2 b
showTerm i (Const a) = show a
showTerm i (Var name) = name

instance Num Term where
   (+) = Add
   (-) = Minus
   (*) = Mult
   fromInteger = Const
   abs = error "not implemented"
   signum = error "not implemented"

derive :: String -> Term -> Term
derive name (Var varName)
    | name == varName = Const 1
    | otherwise = Const 0
derive name (a `Add` b) = derive name a `Add` derive name b
derive name (a `Mult` b) = (a' `Mult` b) `Add` (b' `Mult` a)
    where a' = derive name a; b' = derive name b
derive name (Const i) = Const 0

x :: Term
x = Var "x"
y :: Term
y = Var "y"

termMap :: (Term -> Term) -> Term -> Term
termMap f (a `Add` b) = f a `Add` f b
termMap f (a `Mult` b) = f a `Mult` f b
termMap f t = t

simplify :: Term -> Term
simplify t = simplify' $ termMap simplify t

simplify' :: Term -> Term
simplify' ((Const 0) `Add` b) = b
simplify' (b `Add` (Const 0)) = b
simplify' ((Const 1) `Mult` b) = b
simplify' (b `Mult` (Const 1)) = b
simplify' ((Const 0) `Mult` b) = 0
simplify' (b `Mult` (Const 0)) = 0
simplify' ((Const a) `Add` (Const b)) = Const (a + b)
simplify' ((Const a) `Mult` (Const b)) = Const (a * b)
simplify' t = t