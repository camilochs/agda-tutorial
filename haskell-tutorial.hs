{-# LANGUAGE TemplateHaskell #-}

-- Funciones puras

sumTwoNum :: Int -> Int -> Int
sumTwoNum x y = x + y

cList :: [Int] -> [Int]
cList [] = []
cList [x] = [x]
cList (x:xs) = (cList [x]) ++ (cList xs)

-- Funciones de primer orden

mapCustom :: (a -> b) -> [a] -> [b]
mapCustom f [] = []
mapCustom f (x:xs) = (f x): mapCustom f xs

mapCustom2 :: (a -> b) -> Int -> [a] -> [b]
mapCustom2 f _ [] = []
mapCustom2 f r (x:xs) = (take r (repeat (f x))) ++ mapCustom2 f r xs


-- Búsqueda de patrones (pattern matching)

dumb1 :: Int -> [Char]
dumb1 100 = "Limite del número par"
dumb1 101 = "Limite del número impar"
dumb1 x
        | x `mod` 2 == 0 = dumb1 (x + 2)
        | otherwise = dumb1 (x * x)



type CharInt = Either Char Int
onlyCharOrInt :: CharInt -> Bool
onlyCharOrInt (Left _) = True
onlyCharOrInt (Right _) = True




-- Transparencia referencial

bSort :: (Ord a) => [a] -> [a]
bSort [] = []
bSort [x] = [x]
bSort (x:xs)
  | x > (head xs) = head xs : bSort (x : tail xs)
  | otherwise = x : bSort xs

bubbleSort xs = foldl (\acc _ ->  bSort acc) xs xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (p:xs) = quickSort lesser ++ [p] ++ quickSort greater
                   where
                     lesser  = filter (< p) xs
                     greater = filter (>= p) xs

sort f xs = f xs
