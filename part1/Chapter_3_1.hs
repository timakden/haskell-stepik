module Chapter_3_1 where

-- |
-- Реализуйте функцию @addTwoElements@, которая бы добавляла два переданных ей значения в голову переданного списка.
--
-- >>> addTwoElements 2 12 [85,0,6]
-- [2,12,85,0,6]
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements a b xs = a : b : xs

-- |
-- Реализуйте функцию @nTimes@, которая возвращает список, состоящий из повторяющихся значений ее первого аргумента.
-- Количество повторов определяется значением второго аргумента этой функции.
--
-- >>> nTimes 42 3
-- [42,42,42]
-- >>> nTimes 'z' 5
-- "zzzzz"
nTimes :: a -> Int -> [a]
nTimes a n
  | n <= 0 = []
  | otherwise = a : nTimes a (n - 1)

-- |
-- Сформируйте список целых чисел, содержащий только те элементы исходного списка, значение которых нечетно.
--
-- >>> oddsOnly [2,5,7,10,11,12]
-- [5,7,11]
--
-- Для анализа четности можно использовать функции @odd@ и @even@ стандартной библиотеки.
oddsOnly :: (Integral a) => [a] -> [a]
oddsOnly [] = []
oddsOnly (x : xs) = if odd x then x : oddsOnly xs else oddsOnly xs

-- |
-- Реализуйте функцию @isPalindrome@, которая определяет, является ли переданный ей список палиндромом.
--
-- >>> isPalindrome "saippuakivikauppias"
-- True
-- >>> isPalindrome [1]
-- True
-- >>> isPalindrome [1,2]
-- False
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- |
-- Составьте список сумм соответствующих элементов трех заданных списков.
-- Длина результирующего списка должна быть равна длине самого длинного из заданных списков,
-- при этом «закончившиеся» списки не должны давать вклада в суммы.
--
-- >>> sum3 [1,2,3] [4,5] [6]
-- [11,7,3]
sum3 :: (Num a) => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x : xs) (y : ys) (z : zs) = x + y + z : sum3 xs ys zs

-- |
-- Напишите функцию @groupElems@, которая группирует одинаковые элементы в списке (если они идут подряд)
-- и возвращает список таких групп.
--
-- >>> groupElems []
-- []
-- >>> groupElems [1,2]
-- [[1],[2]]
-- >>> groupElems [1,2,2,2,4]
-- [[1],[2,2,2],[4]]
-- >>> groupElems [1,2,3,2,4]
-- [[1],[2],[3],[2],[4]]
groupElems :: (Eq a) => [a] -> [[a]]
groupElems [] = []
groupElems xs = fst p : groupElems (snd p)
  where
    p = span (== head xs) xs
