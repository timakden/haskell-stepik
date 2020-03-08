module Module2 where

import Data.Function

-- 2.1 Параметрический полиморфизм

{-
  Напишите функцию трех аргументов getSecondFrom, полиморфную по каждому из них,
  которая полностью игнорирует первый и третий аргумент,а возвращает второй. Укажите ее тип.
-}
getSecondFrom :: a -> b -> c -> b
getSecondFrom _ y _ = y

{-
  В модуле Data.Function определена полезная функция высшего порядка

  on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
  on op f x y = f x `op` f y
  Она принимает четыре аргумента: бинарный оператор с однотипными аргументами (типа b), функцию f :: a -> b, возвращающую значение типа b, и два значения типа a. Функция on применяет f дважды к двум значениям типа a и передает результат в бинарный оператор.

  Используя on можно, например, записать функцию суммирования квадратов аргументов так:

  sumSquares = (+) `on` (^2)
  Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом

  multSecond = g `on` h

  g = undefined

  h = undefined
  Напишите реализацию функций g и h.
-}
multSecond = g `on` h

g = (*)

h = snd

{-
  Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую в качестве первого аргумента трехместную функцию.
-}
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

-- 2.2 Параметрический полиморфизм (2)

{-
  Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42, затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:

  doItYourself = f . g . h
  Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.

  f = undefined

  g = undefined

  h = undefined
-}
doItYourself = f . g2 . h2

f = logBase 2

g2 = (^3)

h2 = max 42

-- 2.3 Классы типов

{-
  Реализуйте класс типов Printable, предоставляющий один метод toString — функцию одной переменной,
  которая преобразует значение типа, являющегося представителем Printable, в строковое представление.
-}
class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString p = "(" ++ (toString(fst p)) ++ "," ++ (toString(snd p)) ++ ")"

-- 2.4 Стандартные классы типов

{-
  Пусть существуют два класса типов KnownToGork и KnownToMork, которые предоставляют методы stomp (stab) и
  doesEnrageGork (doesEnrageMork) соответственно:

  class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

  class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
  Класса типов KnownToGorkAndMork является расширением обоих этих классов, предоставляя дополнительно метод stompOrStab:

  class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
  Задайте реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp, если переданное ему
  значение приводит в ярость Морка; вызывает stab, если оно приводит в ярость Горка и вызывает сначала stab, а потом stomp, если оно приводит в ярость их обоих. Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент.
-}
class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x =
        if doesEnrageGork x == True && doesEnrageMork x == True
            then (stomp . stab) x
            else if doesEnrageGork x == True
                then stab x
                else if doesEnrageMork x == True
                    then stomp x
                    else x

{-
  Реализуйте класс типов

  class SafeEnum a where
    ssucc :: a -> a
    spred :: a -> a
  обе функции которого ведут себя как succ и pred стандартного класса Enum, однако являются тотальными,
  то есть не останавливаются с ошибкой на наибольшем и наименьшем значениях типа-перечисления соответственно, а обеспечивают циклическое поведение. Ваш класс должен быть расширением ряда классов типов стандартной библиотеки, так чтобы можно было написать реализацию по умолчанию его методов, позволяющую объявлять его представителей без необходимости писать какой бы то ни было код. Например, для типа Bool должно быть достаточно написать строку

  instance SafeEnum Bool
  и получить возможность вызывать

  GHCi> ssucc False
  True
  GHCi> ssucc True
  False
-}
class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x | x == maxBound = minBound
          | otherwise     = succ x

  spred :: a -> a
  spred x | x == minBound = maxBound
          | otherwise     = pred x

{-
  Напишите функцию с сигнатурой:

  avg :: Int -> Int -> Int -> Double
  вычисляющую среднее значение переданных в нее аргументов:

  GHCi> avg 3 4 8
  5.0
-}
avg :: Int -> Int -> Int -> Double
avg a b c = (/) ((fromIntegral a) + (fromIntegral b) + (fromIntegral c)) 3.0
