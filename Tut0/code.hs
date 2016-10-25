
len :: [a] -> Int
len [] = 0
len (x:xs) = len xs + 1

len' :: [a] -> Int -> Int
len' [] acc = acc
len' (x:xs) acc = len' xs $! (1 + acc)

len'' = (foldl $ flip $ const (+1)) 0


isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

primes1 = [ x | x <- 2:[3,5..], null [ divsr | divsr <- [2..isqrt x], x `divisibleBy` divsr ]]
    where x `divisibleBy` y = x `mod` y == 0


{-
   Für jede Primzahl p != 2 gibt es eine Primzahl p' mit (isqrt p) < p' < p.
   Beweis: Ziemlich schwierig!
-}

primes2 = [ x | x <- 2:[3,5..], not $ (x `divisibleBy`) `any` possibleDivisorOf x]
    where x `divisibleBy` y = x `mod` y == 0
          possibleDivisorOf x = [2..isqrt x]

primes3 = 2:[ x | x <- [3,5..], not $ (x `divisibleBy`) `any` possibleDivisorOf x]
    where x `divisibleBy` y = x `mod` y == 0
          possibleDivisorOf x = takeWhile (<=isqrt x) primes3