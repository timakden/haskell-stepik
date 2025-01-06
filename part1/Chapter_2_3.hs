module Chapter_2_3 where

-- |
-- Реализуйте класс типов @Printable@, предоставляющий один метод @toString@ — функцию одной переменной,
-- которая преобразует значение типа, являющегося представителем @Printable@, в строковое представление.
-- Сделайте типы данных @Bool@ и @()@ представителями этого класса типов, обеспечив следующее поведение:
--
--  >>> toString True
-- "true"
-- >>> toString False
-- "false"
-- >>> toString ()
-- "unit type"
class Printable a where
  toString :: a -> [Char]

instance Printable Bool where
  toString :: Bool -> [Char]
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString :: () -> [Char]
  toString () = "unit type"

-- |
-- Сделайте тип пары представителем класса типов @Printable@, реализованного вами в предыдущей задаче,
-- обеспечив следующее поведение:
--
-- >>> toString (False,())
-- "(false,unit type)"
-- >>> toString (True,False)
-- "(true,false)"
instance (Printable a, Printable b) => Printable (a, b) where
  toString :: (Printable a, Printable b) => (a, b) -> [Char]
  toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"
