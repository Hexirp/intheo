module Parser where
  import Prelude

  newtype Parser a = Parser { runParser :: String -> [(a, String)] }

  instance Functor Parser where
    fmap f x = i0 f x
     where
      i0 f (Parser x) = Parser (i1 f x)
      i1 f x = \x0 -> i2 f (x x0)
      i2 f x = case x of { [] -> []; xv : xs -> i3 f xv : i2 f xs }
      i3 f x = case x of { (x0, x1) -> (f x0, x1) }
