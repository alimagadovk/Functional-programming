{-# LANGUAGE OverloadedStrings #-}

module Lab1_Test where

import EasyTest
import Control.Applicative

import Lab1
import Luhn


-- Для запуска сделайте двойной щелчок по этому файлу или зайдите в директорию и запустите 
-- ghci Lab1_Test из командной строки.
-- Для перезагрузки после изменений (в Lab1 или Lab1_Test) используйте команду :reload (сокращённо :r) внутри GHCi.
-- Не забудьте попробовать :help (:h)!

-- документация EasyTest в EasyTest.html (http://hackage.haskell.org/package/easytest-0.2.1/docs/EasyTest.html)
-- там используются понятия и синтаксис, которые нам пока незнакомы, не беспокойтесь об этом
-- к концу курса вы должны быть способны понять большую часть EasyTest :)
-- <|> комбинирует тесты

-- обратите внимание на оформление списка: так удобнее добавлять, удалять и комментировать элементы в конце
allTests = tests
  -- scope даёт название тесту (и может быть вложено)
  [ scope "xor" $ tests 
      -- expect проверяет условие (безусловный успех: ok, безусловная ошибка: crash "причина")
      [ expect $ xor True True == False
      , expect $ xor True False == True
      , expect $ xor False True == True
      , expect $ xor False False == False
      ]
    , scope "max3" $ -- или tests [ expect (max3 1 3 2 == 3), expect (max3 5 2 5 == 5) ]
      expect (max3 1 3 2 == 3) <|>
      expect (max3 5 2 5 == 5)

    , scope "median3" $
      expect (median3 1 3 2 == 2) <|>
      expect (median3 5 2 5 == 5)


    , scope "coprime" $
      expect (coprime 10 15 == False) <|>
      expect (coprime 12 35 == True)

    , scope "distance" $
      expect (distance (Point [1.0, 0.0]) (Point [0.0, 1.0]) == sqrt 2.0) <|>
      expect (distance (Point [0.0, 0.0]) (Point [0.0, 1.0]) == 1.0)


    , scope "intersect" $
      expect (intersect [1, 2, 4, 6] [5, 4, 2, 5, 7] == [2, 4]) <|>
      expect (intersect [1, 2, 4, 6] [3, 5, 7] == [])

    , scope "zipN" $
      expect (zipN [[1, 2, 3], [4, 5, 6], [7, 8, 9]] == [[1, 4, 7], [2, 5, 8], [3, 6, 9]])

    , scope "find" $
      expect (find (> 0) [-1, 2, -3, 4] == Just 2) <|>
      expect (find (> 0) [-1, -2, -3] == Nothing)


    , scope "mapFuncs" $
      expect (mapFuncs [\x -> x*x, (1 +), \x -> if even x then 1 else 0] 3 == [9, 4, 0])

    , scope "satisfiesAll" $
      expect (satisfiesAll [even, \x -> (rem x 5) == 0] 10 == True) <|>
      expect (satisfiesAll [] 4 == True)


    , scope "isLuhnValid" $
      expect (isLuhnValid [4,  5,  6,  1,     2, 6,  1,  2,     1,  2,  3,  4,     5,  4,  6,  7] == True)



      
    -- добавьте тесты для остальных функций!
  ]

main = run allTests -- runOnly "xor" allTests
                    -- rerun XXXX (или rerunOnly XXXX "xor") для повтора случайных тестов