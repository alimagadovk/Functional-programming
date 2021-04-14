max3 x y z = (max (max x y) z)

min3 x y z = (min (min x y) z)


geomProgression b q 0 = 1
geomProgression b q n = b * (q^^n - 1) / (q - 1)



median3 x y z = r where
    r1 = max3 x y z
    r2 = min3 x y z
    t = x + y + z
    r = t - r1 - r2

data RGB = RGB { red :: Int, green :: Int, blue :: Int } deriving (Eq, Show, Read)
data CMYK = CMYK { cyan :: Double, magenta :: Double, yellow :: Double, black :: Double } deriving (Eq, Show, Read)


rbgToCmyk color = cmyk_color where
    r = fromIntegral(red color) / 255.0
    g = fromIntegral(green color) / 255.0
    b = fromIntegral(blue color) / 255.0
    k = min3 (1.0 - r) (1.0 - g) (1.0 - b)
    c = (1.0 - r - k) / (1.0 - k)
    m = (1.0 - g - k) / (1.0 - k)
    y = (1.0 - b - k) / (1.0 - k)
    cmyk_color = CMYK c m y k

myGcd :: Integer -> Integer -> Integer
myGcd x 0 = x
myGcd x y = myGcd y (mod x y)

coprime a b
    | myGcd a b == 1 = True
    | myGcd a b /= 1 = False

-- вектор задаётся списком координат
data Point = Point [Double] deriving (Eq, Show, Read)

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


intersect :: [Integer] -> [Integer] -> [Integer]
intersect [] _ =  []
intersect _  [] =  []
intersect xs ys = [x | x <- xs, any ((==) x) ys]

-- zipN принимает список списков и возвращает список, который состоит из
-- списка их первых элементов, списка их вторых элементов, и так далее.
-- zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
-- res = (zipN1 [(tail xs) | xs <- xss]) : res

zipN1 :: [[a]] -> [a]
zipN1 [x:xs] = [x]
zipN1 ((x:xs):xss) = x: zipN1 xss

zipN :: [[a]] -> [[a]]
zipN xss 
    |   any (null) xss = []
    |   not (any (null) xss) = (zipN1 xss) : (zipN [(tail xs)| xs <- xss])

-- Если в списке xs есть такие элементы x, для которых f x == True, то
-- find f xs возвращает Just (первый x), а findLast f xs -- Just (последний x).
-- Если таких нет, то обе функции возвращают Nothing
-- find (> 0) [-1, 2, -3, 4] == Just 2
-- findLast (> 0) [-1, 2, -3, 4] == Just 4
-- find (> 0) [-1, -2, -3] == Nothing
find, findLast :: (a -> Bool) -> [a] -> Maybe a
--find f [] = Nothing
--find f xs
--    | (f (head xs)) = Just (head xs)
--    | not (f (head xs)) && (not (null (tail xs))) = find f (tail xs)
--    | null (tail xs) = Nothing

find f [] = Nothing
find f xs = Just (head (filter f xs))

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
-- satisfiesAll [] 4 == True (кстати, почему?)
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




main = do
    let a = [1, 2, 3, 4]
    let b = [4, 5, 6]
    let c = [7, 8, 9]
    let d = [a, b, c]
    let x1 = NEL 1 [2, 3]
    let x2 = NEL 4 [5, 6]
    print(head ( tailNel x2) )