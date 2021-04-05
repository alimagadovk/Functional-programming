{-# OPTIONS_GHC -Wall #-}

module Luhn where

-- Проверка корректности списка цифр (чисел от 0 до 9) алгоритмом Луна. 
-- Алгоритм:
-- 1. Все числа, стоящие на чётных местах (считая с конца), удваиваются. Если при этом получается число, большее 9, то из него вычитается 9. Числа, стояшие на нечётных местах, не изменяются.
-- То есть: последнее число не меняется; предпоследнее удваивается; 3-е с конца (предпредпоследнее) не меняется; 4-е с конца удваивается и т.д.
-- 2. Все полученные числа складываются.
-- 3. Если полученная сумма кратна 10, то исходный список корректен.

-- Не пытайтесь собрать всё в одну функцию, используйте вспомогательные.
-- Не забудьте добавить тесты (в отдельном модуле или здесь же), в том числе для вспомогательных функций!
rev :: [Int] -> [Int]
rev [] = []
rev xs = (last xs) : rev (init xs) 

len :: [Int] -> Int
len [] = 0
len xs = 1 + (len (tail xs))

create_seq :: Int -> Int -> [Int]
create_seq 0 _ = []
create_seq 1 0 = [1]
create_seq 1 1 = [0]
create_seq n seed = (1 - seed) : (create_seq (n - 1) (1 - seed))


mult :: [Int] -> [Int] -> [Int]
mult [] [] = []
mult xs ys = ((head (xs)) * (head (ys))) : mult (tail xs) (tail ys)

mult2 :: [Int] -> [Int]
mult2 xs = mult (rev xs) [(2 ^ p) | p <- (create_seq (len xs) 1)]


calc_sum :: [Int] -> Int
calc_sum [] = 0
calc_sum xs = head xs + calc_sum (tail xs)

isLuhnValid :: [Int] -> Bool
isLuhnValid [] = False
isLuhnValid xs = res where
    temp1 = mult2 xs
    temp2 = map (\x -> if (x > 9) then (x - 9) else x) temp1
    res = if (mod (calc_sum temp2) 10 == 0)
    then True
    else False
