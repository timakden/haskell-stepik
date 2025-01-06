module Chapter_1_6 where

-- |
-- Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
-- \[
-- a_0 = 1; a_1 = 2; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2a_k
-- \]
-- Попытайтесь найти эффективное решение.
seqA :: Integer -> Integer
seqA n =
  let helper 0 a b c = a
      helper n a b c = helper (n - 1) b c (c + b - 2 * a)
   in helper n 1 2 3

-- |
-- Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.
--
-- >>> sum'n'count (-39)
-- (12,2)
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x
  | x < 0 = sum'n'count (abs x)
  | otherwise = (sum x, count x)
  where
    sum x
      | x < 10 = x
      | otherwise = (x `mod` 10) + sum (x `div` 10)
    count x
      | x < 10 = 1
      | otherwise = 1 + count (x `div` 10)

-- |
-- Реализуйте функцию, находящую значение определённого интеграла от заданной функции \(f\) на заданном интервале
-- \([a,b]\) методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)
--  
-- >>> integration sin pi 0
-- -2.0
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = h * ((f a + f b) / 2 + helper 0 f a h 1)
  where
    h = (b - a) / 1000
    helper s f a h n
      | n == 1000 = s
      | otherwise = helper (s + f (a + n * h)) f a h (n + 1)
