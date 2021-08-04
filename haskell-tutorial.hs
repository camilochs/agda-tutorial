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

removeFirstLastElement :: [a] -> [a]
removeFirstLastElement []     = []
removeFirstLastElement [_]    = []
removeFirstLastElement [_,_]  = []
removeFirstLastElement (x:xs) = [head xs] ++ removeFirstLastElement xs

type CharInt = Either Char Int
onlyCharOrInt :: CharInt -> Bool
onlyCharOrInt (Left _) = True
onlyCharOrInt (Right _) = True


data MultiItem = Item1 Int | Item2 Int Int | ItemN [MultiItem]

sumItems :: MultiItem -> Int
sumItems (Item1 x) = x
sumItems (Item2 x y) = x + y
sumItems (ItemN (x:xs)) = sumItems x + sum [sumItems e | e <- xs]


numberToString:: Int -> [Char]
numberToString 0 = "cero"
numberToString 1 = "uno"
numberToString 2 = "dos"
numberToString 3 = "tres"
numberToString x
  | x < 0 = numberToString (x + 1)
  | otherwise = numberToString (x - 1)


-- Evaluación perezosa

r = (repeat 1)

numberNotEvaluated x y z = x + z

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
