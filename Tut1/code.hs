

-- f muss monoton wachsend sein.
-- Es wird nach der größten Zahl x aus [lowerBound, upperBound] gesucht, sodass f x <= y
invert :: (Integer -> Integer) -> Integer -> Integer -> Integer-> Integer
invert f lowerBound upperBound y
    | lowerBound >= upperBound = upperBound
    | f mid <= y = invert f mid upperBound y
    | f mid > y = invert f lowerBound (mid-1) y
        -- Annahme lowerBound < mid <= upperBound wenn lowerBound != upperBound 
        where mid = (1 + lowerBound + upperBound) `div` 2

root x = invert (^2) 1 x x


data Term = Add Term Term | Mult Term Term 
    | Var String | Const Integer | Minus Term Term


instance Show Term where
    showsPrec i (Add a b) str
        | i <= 1 = str ++ (showsPrec 1 a "") ++ " + " ++ (showsPrec 1 b "")
        | otherwise = "(" ++ (showsPrec 1 (Add a b) "") ++ ")"
    showsPrec i (Mult a b) str = str ++ (showsPrec 2 a "") ++ " * " ++ (showsPrec 2 b "")
    showsPrec i (Const a) str = str ++ show a
    showsPrec i (Var name) str = str ++ name


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
derive name (Add a b) = (derive name a) `Add` (derive name b)
derive name (Mult a b) = (a' `Mult` b) `Add` (b' `Mult` a)
    where a' = derive name a; b' = derive name b
derive name (Const i) = Const 0