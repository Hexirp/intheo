{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.NoFailures where

  import Control.Applicative
  import Control.Monad

  newtype Parser p s m a = Parser { runParser :: forall r. s -> (a -> p -> s -> m r) -> m r }

  instance Functor (Parser p s m) where
    fmap :: forall p s m a b. (a -> b) -> Parser p s m a -> Parser p s m b
    fmap f x = i0 f x
     where
      i0 :: (a -> b) -> Parser p s m a -> Parser p s m b
      i0 f (Parser x) = Parser (i1 f x)
      i1 :: (a -> b) -> (forall r. s -> (a -> p -> s -> m r) -> m r) -> (forall r. s -> (b -> p -> s -> m r) -> m r)
      i1 f x = \s c -> x s (\xv xp xs -> c (f xv) xp xs)

  instance Monoid p => Applicative (Parser p s m) where
    pure :: forall p s m. Monoid p => forall a. a -> Parser p s m a
    pure x = i0 x
     where
      i0 :: a -> Parser p s m a
      i0 x = Parser (i1 x)
      i1 :: a -> (forall r. s -> (a -> p -> s -> m r) -> m r)
      i1 x = \s c -> c x mempty s

    (<*>) :: forall p s m. Monoid p => forall a b. Parser p s m (a -> b) -> Parser p s m a -> Parser p s m b
    x0 <*> x1 = i0 x0 x1
     where
      i0 :: Parser p s m (a -> b) -> Parser p s m a -> Parser p s m b
      i0 = undefined
