module Part3.Tasks where

import Control.Arrow
import Data.Function (on)
import Data.List (group, sort)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = f n : finc f (n + 1)

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : f `ff` f x

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = snd . maximum . map (length &&& head) . group . sort $ concatMap toDigits numbers
  where
    toDigits 0 = []
    toDigits n = toDigits n' ++ [n `mod` 10]
      where
        n' = n `div` 10

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (h : t) = h : uniq'
  where
    uniq' = uniq . filter (h /=) $ t

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy _ [] = []
grokBy f (h : t) = (f h, h : h') : grokBy f t'
  where
    eq = (==) `on` f
    eq' = eq h
    (h', t') = span eq' t
