module Chapter_1_2 where

-- |
-- Реализуйте функцию трех аргументов @lenVec3@, которая вычисляет длину трехмерного вектора.
-- Аргументы функции задают декартовы координаты конца вектора, его начало подразумевается
-- находящимся в начале координат. Для извлечения квадратного корня воспользуйтесь функцией
-- @sqrt@, определенной в стандартной библиотеке.
--
-- >>> lenVec3 2 3 6
-- 7.0
lenVec3 :: (Floating a) => a -> a -> a -> a
lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

-- |
-- Напишите реализацию функции @sign@, которая возвращает 1, если ей передано положительное
-- число, (-1), если отрицательное, и 0 в случае, когда передан 0.
--
-- >>> sign 42
-- 1
--
-- >>> sign (-42)
-- -1
sign :: (Ord a, Num a, Num p) => a -> p
sign x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0
