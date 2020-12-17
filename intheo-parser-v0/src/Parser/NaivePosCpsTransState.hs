{-# LANGUAGE RankNTypes #-}

module Parser.NaivePosCpsTransState where
  import Prelude

  import Control.Applicative
  import Control.Monad

  newtype Parser p s m a = Parser { runParser :: forall r. s -> m r -> (a -> p -> s -> r -> m r) -> m r }

  instance Functor (Parser p s m) where
    fmap f x = i0 f x
     where
      i0 f (Parser x) = Parser (i1 f x)
      i1 f x = \s n c -> x s n (\xvv xvp xvs xs -> c (f xvv) xvp xvs xs)

  instance (Monoid p, Monad m) => Applicative (Parser p s m) where
    pure xv = Parser (\xs xn xc -> join (fmap (xc xv mempty xs) xn))

    x0 <*> x1 = i0 x0 x1
     where
      i0 = undefined

  instance (Monoid p, Monad m) => Monad (Parser p s m) where
    x0 >>= x1 = i0 x0 x1
     where
      i0 = undefined

  instance (Monoid p, Monad m) => Alternative (Parser p s m) where
    empty = Parser (\s n c -> n)

    x0 <|> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (\s n c -> x0 s (x1 s n c) c)

  instance (Monoid p, Monad m) => MonadPlus (Parser p s m) where
