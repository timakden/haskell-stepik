module Chapter_1_4 where

import Data.Char (digitToInt, isDigit)

-- |
-- Реализуйте функцию @twoDigits2Int@, которая принимает два символа и возвращает число,
-- составленное из этих символов, если оба символа числовые, и 100 в противном случае.
-- (Первый символ рассматривается как количество десятков, второй — единиц.)
--
-- >>> twoDigits2Int '4' '2'
-- 42
--
-- >>> twoDigits2Int '4' 'a'
-- 100
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then 10 * digitToInt x + digitToInt y else 100

-- |
-- Будем задавать точки на плоскости парами типа @(Double, Double)@.
-- Реализуйте функцию @dist@, которая возвращает расстояние между двумя точками, передаваемыми ей в качестве аргументов.
--
-- >>> dist (5, 1) (8, 5)
-- 5.0
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)
