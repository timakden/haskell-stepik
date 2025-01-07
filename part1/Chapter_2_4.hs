module Chapter_2_4 where

class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

-- |
-- Пусть существуют два класса типов @KnownToGork@ и @KnownToMork@, которые предоставляют методы @stomp@ (@stab@)
-- и @doesEnrageGork@ (@doesEnrageMork@) соответственно:
--
-- @
-- class KnownToGork a where
--   stomp :: a -> a
--   doesEnrageGork :: a -> Bool
--
-- class KnownToMork a where
--   stab :: a -> a
--   doesEnrageMork :: a -> Bool
-- @
--
-- Класс типов @KnownToGorkAndMork@ является расширением обоих этих классов, предоставляя дополнительно метод @stompOrStab@:
--
-- @
-- class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
--   stompOrStab :: a -> a
-- @
--
-- Задайте реализацию по умолчанию метода @stompOrStab@, которая вызывает метод @stomp@,
-- если переданное ему значение приводит в ярость Морка; вызывает @stab@, если оно приводит в ярость Горка
-- и вызывает сначала @stab@, а потом @stomp@, если оно приводит в ярость их обоих.
-- Если не происходит ничего из вышеперечисленного, метод должен возвращать переданный ему аргумент.
class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x
    | doesEnrageGork x && doesEnrageMork x = (stomp . stab) x
    | doesEnrageGork x = stab x
    | doesEnrageMork x = stomp x
    | otherwise = x

-- |
-- Реализуйте класс типов
--
-- @
-- class SafeEnum a where
--   ssucc :: a -> a
--   spred :: a -> a
-- @
--
-- обе функции которого ведут себя как @succ@ и @pred@ стандартного класса @Enum@,
-- однако являются тотальными, то есть не останавливаются с ошибкой на наибольшем и наименьшем
-- значениях типа-перечисления соответственно, а обеспечивают циклическое поведение.
-- Ваш класс должен быть расширением ряда классов типов стандартной библиотеки,
-- так чтобы можно было написать реализацию по умолчанию его методов, позволяющую объявлять
-- его представителей без необходимости писать какой бы то ни было код.
-- Например, для типа @Bool@ должно быть достаточно написать строку
--
-- @
-- instance SafeEnum Bool
-- @
--
-- и получить возможность вызывать
--
-- >>> ssucc False
-- True
-- >>> ssucc True
-- False
class (Enum a, Eq a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | x == maxBound = minBound
    | otherwise = succ x

  spred :: a -> a
  spred x
    | x == minBound = maxBound
    | otherwise = pred x

-- |
-- Напишите функцию с сигнатурой:
--
-- @
-- avg :: Int -> Int -> Int -> Double
-- @
--
-- вычисляющую среднее значение переданных в нее аргументов:
--
-- >>> avg 3 4 8
-- 5.0
avg :: Int -> Int -> Int -> Double
avg a b c = (/) (fromIntegral a + fromIntegral b + fromIntegral c) 3.0
