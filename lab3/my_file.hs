
import Data.List(intercalate)

data Stream a = a :> Stream a

instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ sTake 10 s)
             ++ ", ..."

-- Реализуйте функцию, превращающую поток в (бесконечный) список
streamToList :: Stream a -> [a]
streamToList (a :> s) = a : streamToList s

-- функция, возвращающая n первых элементов потока
-- удобна для написания тестов следующих функций
sTake :: Int -> Stream a -> [a]
sTake n s = take n (streamToList s)

sRepeat :: a -> Stream a
sRepeat a = a :> sRepeat a

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = x :> sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (a :> s1) s2 = a :> sInterleave s2 s1

nats :: Stream Integer
nats = sIterate (\x -> x + 1) 0

ruler :: Stream Integer
ruler = let ruler' x = sInterleave (sRepeat x) (ruler' (x + 1))
        in ruler' 0

main = do
    let a = sRepeat 1
    let x = ruler
    print(ruler)