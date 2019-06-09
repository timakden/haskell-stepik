module Module1 where

import Data.Char

-- 1.2 Функции
{-
  Реализуйте функцию трех аргументов lenVec3, которая вычисляет длину трехмерного вектора.
  Аргументы функции задают декартовы координаты конца вектора, его начало подразумевается
  находящимся в начале координат. Для извлечения квадратного корня воспользуйтесь функцией sqrt,
  определенной в стандартной библиотеке.
-}
lenVec3 :: Double -> Double -> Double -> Double
lenVec3 x y z = sqrt (x^2 + y^2 + z^2)

{-
  Напишите реализацию функции sign, которая возвращает 1, если ей передано положительное число,
  (-1), если отрицательное, и 0 в случае, когда передан 0.
-}
sign :: (Ord a, Num a, Num p) => a -> p
sign x | x > 0 = 1
       | x < 0 = -1
       | otherwise = 0

-- 1.3 Операторы           
-- Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов.
x |-| y = abs (x - y)

-- 1.4 Базовые типы
-- Запишите тип функции standardDiscount, определенной как частичное применение функции discount.
discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

{-
  Реализуйте функцию twoDigits2Int, которая принимает два символа и возвращает число,
  составленное из этих символов, если оба символа числовые, и 100 в противном случае.
-}
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y | isDigit x && isDigit y = 10 * digitToInt x + digitToInt y
                  | otherwise = 100

{-
  Будем задавать точки на плоскости парами типа (Double, Double).
  Реализуйте функцию dist, которая возвращает расстояние между двумя точками, передаваемыми ей в качестве аргументов.
-}
dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)

-- 1.5 Рекурсия
{-
  Определите функцию, вычисляющую двойной факториал, то есть произведение натуральных чисел,
  не превосходящих заданного числа и имеющих ту же четность. Например: 7!!=7⋅5⋅3⋅1,  8!!=8⋅6⋅4⋅2.
  Предполагается, что аргумент функции может принимать только неотрицательные значения.
-}
doubleFact :: Integer -> Integer
doubleFact n = if (n <= 0) then 1 else n * doubleFact (n - 2)

{-
  Последовательность чисел Фибоначчи 0,1,1,2,3,5,8,13,21,… легко определить рекурсивно,
  задав два первых терминирующих значения и определив любое последующее как сумму двух непосредственно предыдущих.
  Измените определение функции fibonacci так, чтобы она была определена для всех целых чисел и
  порождала при отрицательных аргументах указанную последовательность.
-}
fibonacci :: Integer -> Integer
fibonacci n | n > 1     = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0     = fibonacci (n + 2) - fibonacci (n + 1)
            | otherwise = n

{-
  Реализация функции для вычисления числа Фибоначчи, основанная на прямом рекурсивном определении,
  крайне неэффективна - количество вызовов функции растет экспоненциально с ростом значения аргумента.
  С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию,
  имеющую линейную сложность (по числу рекурсивных вызовов). Как и в предыдущем задании,
  функция должна быть определена для всех целых чисел.
-}
fibonacci2 :: Integer -> Integer
fibonacci2 n | n < 0 && odd n = -helper n 1 0
             | otherwise      = helper n 1 0
             where
               helper index previous current | index > 0 = helper (index - 1) (previous + current) previous
                                             | index < 0 = helper (index + 1) (previous - current) (-previous)
                                             | otherwise = current

-- 1.6 Локальные связывания и правила отступов
{-
  Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности:
  a0 = 1; a1 = 2; a2 = 3; ak+3 = ak+2 + ak+1 − 2ak.
  Попытайтесь найти эффективное решение.
-}
seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n > 2  = helper 1 2 3 n
       where
         helper acc1 acc2 acc3 n | n == 0    = acc1
                                 | otherwise = helper acc2 acc3 ((acc3 + acc2) - 2 * acc1) (n - 1)

-- Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x | x < 0 =  sum'n'count(abs x)
              | otherwise = (sum x, count x) where
                sum x | x < 10    = x
                      | otherwise = (x `mod` 10) + sum (x `div` 10)
                count x | x < 10    = 1
                        | otherwise = 1 + count (x `div` 10)

{-
  Реализуйте функцию, находящую значение определённого интеграла от заданной функции f на заданном
  интервале [a,b] методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
-}
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + helper 0 f a h 1) where
                      h = (b - a) / 1000
                      helper s f a h n | n == 1000 = s
                                       | otherwise = helper (s + f (a + n * h)) f a h (n + 1)
