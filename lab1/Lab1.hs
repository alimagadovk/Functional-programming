-- В сдаваемой версии не должно быть предупреждений
-- (в исходной они есть, поскольку аргументы функций не используются).
{-# OPTIONS_GHC -Wall #-}

-- Для первых упражнений есть тесты в Lab1_Test.hs. Добавьте свои!

-- Не забывайте, что можно добавлять вспомогательные
-- функции и переменные. Старайтесь, чтобы код был
-- читаемым. Рекомендации по оформлению кода: 
-- https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
-- Как компилятор понимает отступы: https://en.wikibooks.org/wiki/Haskell/Indentation

-- Одно из заданий вынесено в файл Luhn.hs для отделения его вспомогательных
-- функций. Рекомендуемая очерёдность выполнения: после distance или после intersect.
module Lab1 where

-- xor x y находит "исключающее или" x и y
-- xor True False == True
-- xor True True == False

-- используйте сопоставление с образцом
xor :: Bool -> Bool -> Bool
xor x y = x /= y

-- max3 x y z находит максимум из x, y и z
-- max3 1 3 2 == 3
-- max3 5 2 5 == 5
-- median3 x y z находит второе по величине число (медиану)
-- median3 1 3 2 == 2
-- median3 5 2 5 == 5
max3, median3 :: Integer -> Integer -> Integer -> Integer
max3 x y z = (max (max x y) z)


minI3 :: Integer -> Integer -> Integer -> Integer
minI3 x y z = (min (min x y) z)

min3 :: Double -> Double -> Double -> Double
min3 x y z = (min (min x y) z)


median3 x y z = r where
    r1 = max3 x y z
    r2 = minI3  x y z
    t = x + y + z
    r = t - r1 - r2

-- Типы данных, описывающие цвета в моделях 
-- RGB (https://ru.wikipedia.org/wiki/RGB), компоненты от 0 до 255
-- и CMYK (https://ru.wikipedia.org/wiki/CMYK), компоненты от 0.0 до 1.0
data RGB = RGB { red :: Int, green :: Int, blue :: Int } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)
-- Задайте функцию для их преобразования
-- (формулы из http://www.codeproject.com/Articles/4488/XCmyk-CMYK-to-RGB-Calculator-with-source-code):
-- Black   = min(1-Red, 1-Green, 1-Blue)
-- Cyan    = (1-Red-Black) / (1-Black)
-- Magenta = (1-Green-Black) / (1-Black)
-- Yellow  = (1-Blue-Black) / (1-Black) 
-- где значения Red, Green и Blue нормализованы от 0 до 1).

-- Заметьте, что (/) для Int не работает, и неявного преобразования Int в Double нет.
-- Это преобразование производится с помощью функции fromIntegral.
rbgToCmyk :: RGB -> CMYK
rbgToCmyk color = cmyk_color where
    r = fromIntegral(red color) / 255.0
    g = fromIntegral(green color) / 255.0
    b = fromIntegral(blue color) / 255.0
    k = min3 (1.0 - r) (1.0 - g) (1.0 - b)
    c = (1.0 - r - k) / (1.0 - k)
    m = (1.0 - g - k) / (1.0 - k)
    y = (1.0 - b - k) / (1.0 - k)
    cmyk_color = CMYK c m y k

-- geomProgression b q n находит n-й (считая с 0) член 
-- геометрической прогрессии, нулевой член которой -- b, 
-- а знаменатель -- q.
-- geomProgression 3.0 2.0 2 == 12.0

-- используйте рекурсию
-- не забудьте случаи n < 0 и n == 0.
geomProgression :: Double -> Double -> Integer -> Double
geomProgression b q n = b * (q^^(n - 1) - 1) / (q - 1)

-- coprime a b определяет, являются ли a и b взаимно простыми
-- (определение: Целые числа называются взаимно простыми, 
-- если они не имеют никаких общих делителей, кроме +/-1)
-- coprime 10 15 == False
-- coprime 12 35 == True

-- используйте рекурсию
-- есть ли важные пограничные случаи?

-- полезные функции в Prelude (автоматически загруженной
-- части стандартной библиотеки): quot, rem, quotRem 
-- (или div, mod, divMod в зависимости от того, как 
-- обрабатываете отрицательные числа)
-- https://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html


myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd x y = myGcd y (x `mod` y)


coprime :: Integer -> Integer -> Bool
coprime a b
    | myGcd a b == 1 = True
    | myGcd a b /= 1 = False


-- вектор задаётся списком координат
data Point = Point [Double] deriving (Eq, Show, Read)

-- distance x y находит расстояние между двумя точками в n-мерном
-- пространстве. Если число координат точек разное, сообщите об ошибке.
-- distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0
-- distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0

-- используйте рекурсию и сопоставление с образцом

calc_sum_sq :: [Double] -> [Double] -> Double
calc_sum_sq v1 v2
    | ((null (tail v1)) && (not (null (tail v2)))) || ((null (tail v2)) && (not (null (tail v1)))) = error "Amounts of coordinates are different"
    | (not (null (tail v1))) && (not (null (tail v2))) = ((head v1) - (head v2))^^2 + calc_sum_sq (tail v1) (tail v2)
    | (null (tail v1)) && (null (tail v2)) = ((head v1) - (head v2))^^2

pointCoords :: Point -> [Double]
pointCoords (Point vect_coords) = vect_coords

distance :: Point -> Point -> Double
distance x y = d where
    d = (calc_sum_sq (pointCoords x) (pointCoords y)) ** 0.5

-- intersect xs ys возвращает список, содержащий общие элементы двух списков.
-- intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4] (или [4, 2]!)
-- intersect [1, 2, 4, 6] [3, 5, 7] == []

-- используйте рекурсию и сопоставление с образцом
intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ =  []
intersect _  [] =  []
intersect xs ys = [x | x <- xs, any ((==) x) ys]

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]

zipN1 :: [[a]] -> [a]
zipN1 [x:xs] = [x]
zipN1 ((x:xs):xss) = x: zipN1 xss

zipN :: [[a]] -> [[a]]
zipN xss 
    |   any (null) xss = []
    |   not (any (null) xss) = (zipN1 xss) : (zipN [(tail xs)| xs <- xss])

-- Нижеперечисленные функции можно реализовать или рекурсивно, или с помощью 
-- стандартных функций для работы со списками (map, filter и т.д.)
-- Попробуйте оба подхода! Хотя бы одну функцию реализуйте обоими способами.

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, findLast :: (a -> Bool) -> [a] -> Maybe a
find f [] = Nothing
find f xs
    | (f (head xs)) = Just (head xs)
    | not (f (head xs)) && (not (null (tail xs))) = find f (tail xs)
    | null (tail xs) = Nothing

-- Реализуем find с помощью функции filter:
{-find f [] = Nothing
find f xs = Just (head (filter f xs))-}

findLast f [] = Nothing
findLast f xs
    | (f (last xs)) = Just (last xs)
    | not (f (last xs)) && (not (null (init xs))) = find f (init xs)
    | null (init xs) = Nothing

-- mapFuncs принимает список функций fs и возвращает список результатов 
-- применения всех функций из fs к x.
-- mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0]
mapFuncs :: [a -> b] -> a -> [b]
mapFuncs [] _ = []
mapFuncs fs x = [(f x) | f <- fs]

-- satisfiesAll принимает список предикатов (функций, возвращающих Bool) preds 
-- и возвращает True, если все они выполняются 
-- (т.е. возвращают True) для x. Полезные стандартные функции: and, all.
-- satisfiesAll [even, \x -> x rem 5 == 0] 10 == True
-- satisfiesAll [] 4 == True (кстати, почему?). Потому что в пустом списке нет предикатов, возвращающих True, а значит ни один предикат не будет применён к аргументам
satisfiesAll :: [a -> Bool] -> a -> Bool
satisfiesAll [] _ = True
satisfiesAll preds x
    | all (True ==) (mapFuncs preds x) = True
    | any (False ==) (mapFuncs preds x) = False

-- непустой список состоит из первого элемента (головы)
-- и обычного списка остальных элементов
data NEL a = NEL a [a]

-- запишите правильный тип (т.е. такой, чтобы функция имела результат для любых аргументов)
-- и реализуйте функции на NEL, аналогичные tail, last и zip
-- tailNel :: NEL a -> ???
-- lastNel :: NEL a -> ???
-- zipNel :: NEL a -> NEL b -> ???
-- listToNel :: [a] -> ???
-- nelToList :: NEL a -> ???

tailNel :: NEL a -> NEL a
tailNel (NEL _ xs) = NEL (head xs) (tail xs)


lastNel :: NEL a -> a
lastNel (NEL x xs)
    | null xs = x
    | (not (null xs)) = last xs


zipNel :: NEL a -> NEL b -> NEL (a,b)
zipNel a b = res where
    xs1 = nelToList a
    xs2 = nelToList b
    res = listToNel (zip xs1 xs2)


listToNel :: [a] -> NEL a
listToNel x = NEL (head x) (tail x)

nelToList :: NEL a -> [a]
nelToList (NEL x xs) = x : xs