module Chapter_2_1 where

import Data.Function (on)

-- |
-- Напишите функцию трех аргументов @getSecondFrom@, полиморфную по каждому из них,
-- которая полностью игнорирует первый и третий аргумент, а возвращает второй. Укажите ее тип.
getSecondFrom :: a -> b -> c -> b
getSecondFrom _ y _ = y

-- |
-- Функция @multSecond@, перемножающая вторые элементы пар, реализована следующим образом
--
-- @
-- multSecond = g /`on/` h
-- g = undefined
-- h = undefined
-- @
--
-- Напишите реализацию функций @g@ и @h@.
--
-- >>> multSecond ('A',2) ('E',7)
-- 14
multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on` h
  where
    g = (*)
    h = snd

-- |
-- Реализуйте функцию @on3@, имеющую семантику, схожую с @on@, но принимающую в качестве первого аргумента трехместную функцию.
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)