module Part5.Tasks where

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ x [] = x
myFoldl f x (h : t) = myFoldl f (f x h) t

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (h : t) = f h (myFoldr f x t)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr ((:) . f) []

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((<>) . f) []

myConcat :: [[a]] -> [a]
myConcat = myFoldr (<>) []

myReverse :: [a] -> [a]
myReverse = myFoldl (flip (:)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr f []
    where f h t = if p h then h:t
                         else t

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr f ([], [])
    where f x (fit, unfit) = if p x then (x:fit, unfit)
                                    else (fit, x:unfit)
