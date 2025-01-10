module Chapter_3_3 where

-- |
-- Реализуйте c использованием функции @zipWith@ функцию @fibStream@, возвращающую бесконечный список чисел Фибоначчи.
--
-- >>> take 10 fibStream
-- [0,1,1,2,3,5,8,13,21,34]
fibStream :: [Integer]
fibStream = 0 : zipWith (+) fibStream (1 : fibStream)

-- |
-- Предположим, что функция @repeat@, была бы определена следующим образом:
--
-- @
-- repeat = iterate repeatHelper
-- @
--
-- Определите, как должна выглядеть функция @repeatHelper@.
repeatHelper :: a -> a
repeatHelper = id

-- |
-- Пусть задан тип @Odd@ нечетных чисел следующим образом:
--
-- @
-- data Odd = Odd Integer deriving (Eq, Show)
-- @
--
-- Сделайте этот тип представителем класса типов @Enum@.
--
-- >>> succ $ Odd (-100000000000003)
-- Odd (-100000000000001)
--
-- Конструкции с четным аргументом, типа @Odd 2@, считаются недопустимыми и не тестируются.
--
-- /Примечание./ Мы еще не знакомились с объявлениями пользовательских типов данных,
-- однако, скорее всего, приведенное объявление не вызовет сложностей.
-- Здесь объявляется тип данных @Odd@ с конструктором @Odd@. Фактически это простая упаковка для типа @Integer@.
-- Часть @deriving (Eq, Show)@ указывает компилятору, чтобы он автоматически сгенерировал представителей
-- соответствующих классов типов для нашего типа (такая возможность имеется для ряда стандартных классов типов).
-- Значения типа @Odd@ можно конструировать следующим образом:
--
-- @
-- GHCi> let x = Odd 33
-- GHCi> x
-- Odd 33
-- @
-- и использовать конструктор данных @Odd@ в сопоставлении с образцом:
--
-- @
-- addEven :: Odd -> Integer -> Odd
-- addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
--                   | otherwise      = error "addEven: second parameter cannot be odd"
-- @
newtype Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  succ :: Odd -> Odd
  succ (Odd x) = Odd $ x + 2

  pred :: Odd -> Odd
  pred (Odd x) = Odd $ x - 2

  toEnum :: Int -> Odd
  toEnum x = Odd $ toInteger x * 2 + 1

  fromEnum :: Odd -> Int
  fromEnum (Odd x) = quot (fromInteger x - 1) 2

  enumFrom :: Odd -> [Odd]
  enumFrom = iterate succ

  enumFromThen :: Odd -> Odd -> [Odd]
  enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]

  enumFromTo :: Odd -> Odd -> [Odd]
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]

  enumFromThenTo :: Odd -> Odd -> Odd -> [Odd]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]

-- |
-- Пусть есть список положительных достоинств монет @coins@, отсортированный по возрастанию.
-- Воспользовавшись механизмом генераторов списков, напишите функцию @change@, которая разбивает
-- переданную ей положительную сумму денег на монеты достоинств из списка @coins@ всеми возможными способами.
-- Например, если @coins = [2, 3, 7]@:
--
-- >>> change 7
-- [[2,2,3],[2,3,2],[3,2,2],[7]]
--
-- /Примечание./ Порядок монет в каждом разбиении имеет значение, то есть наборы @[2,2,3]@ и @[2,3,2]@ — различаются.
-- Список @coins@ определять не надо.
change :: (Ord a, Num a) => a -> [[a]]
change n
  | n < 0 = []
  | n == 0 = [[]]
  | otherwise = [x : xs | x <- coins, xs <- change (n - x)]
