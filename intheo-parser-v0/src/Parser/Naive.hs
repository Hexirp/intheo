module Parser.Naive where
  import Prelude

  import Control.Applicative
  import Control.Monad

  import Data.List

  newtype Parser a = Parser { runParser :: String -> [(a, String)] }

  instance Functor Parser where
    fmap f x = i0 f x
     where
      i0 f (Parser x) = Parser (i1 f x)
      i1 f x = \s -> i2 f (x s)
      i2 f x = case x of { [] -> []; xv : xs -> i3 f xv : i2 f xs; }
      i3 f x = case x of { (v, s) -> (f v, s); }

  instance Applicative Parser where
    pure v = Parser (\s -> [(v, s)])

    x0 <*> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) x1
      i2 x0 x1 = case x0 of { [] -> []; x0v : x0s -> i3 x0v x1 (i2 x0s x1); }
      i3 x0 x1 r = case x0 of { (v0, s0) -> i4 v0 (x1 s0) r; }
      i4 v0 x1 r = case x1 of { [] -> r; x1v : x1s -> i5 v0 x1v : i4 v0 x1s r; }
      i5 v0 x1 = case x1 of { (v1, s1) -> (v0 v1, s1); }

  instance Monad Parser where
    x0 >>= x1 = i0 x0 x1
     where
      i0 (Parser x0) x1 = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) x1
      i2 x0 x1 = case x0 of { [] -> []; x0v : x0s -> i3 x0v x1 (i2 x0s x1); }
      i3 x0 x1 r = case x0 of { (v0, s0) -> i4 s0 (x1 v0) r; }
      i4 s0 (Parser x1) r = i5 (x1 s0) r
      i5 x1 r = case x1 of { [] -> r; x1v : x1s -> x1v : i5 x1s r; }

  instance Alternative Parser where
    empty = Parser (\s -> [])

    x0 <|> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) (x1 s)
      i2 x0 x1 = case x0 of { [] -> x1; x0v : x0s -> x0v : i2 x0s x1; }

  instance MonadPlus Parser where

  token :: Parser Char
  token = Parser (\s -> case s of { [] -> []; sv : ss -> [(sv, ss)]; })

  choice :: Ord t => [(t, a -> Parser a)] -> a -> Parser a
  choice x z = i0 (sortOn (\x -> case x of (x0, x1) -> x0) x) z
   where
    i0 x z = case x of { [] -> pure z; xv : xs -> do { z' <- i0 xs z; i1 xv z' }; }
    i1 x z = case x of { (t, f) -> f z; }
