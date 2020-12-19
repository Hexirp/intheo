{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.NoFailureIO where

  import Control.Applicative
  import Control.Monad

  newtype Parser e m a = Parser { runParser :: forall r. e -> (a -> m r) -> m r }

  instance Functor (Parser e m) where
    fmap :: forall e m a b. (a -> b) -> Parser e m a -> Parser e m b
    fmap f x = i0 f x
     where
      i0 :: (a -> b) -> Parser e m a -> Parser e m b
      i0 f (Parser x) = Parser (i1 f x)
      i1 :: (a -> b) -> (forall r. e -> (a -> m r) -> m r) -> (forall r. e -> (b -> m r) -> m r)
      i1 f x = \e c -> x e (\xv -> c (f xv))

  instance Applicative (Parser e m) where
    pure :: forall e m a. a -> Parser e m a
    pure x = i0 x
     where
      i0 :: a -> Parser e m a
      i0 x = Parser (i1 x)
      i1 :: a -> (forall r. e -> (a -> m r) -> m r)
      i1 x = \e c -> c x

    (<*>) :: forall e m a b. Parser e m (a -> b) -> Parser e m a -> Parser e m b
    x0 <*> x1 = i0 x0 x1
     where
      i0 :: Parser e m (a -> b) -> Parser e m a -> Parser e m b
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. e -> ((a -> b) -> m r) -> m r) -> (forall r. e -> (a -> m r) -> m r) -> (forall r. e -> (b -> m r) -> m r)
      i1 x0 x1 = \e c -> x0 e (\x0v -> i2 e x0v x1 c)
      i2 :: forall r. e -> (a -> b) -> (forall r. e -> (a -> m r) -> m r) -> (b -> m r) -> m r
      i2 e x0v x1 c = x1 e (\x1v -> i3 x0v x1v c)
      i3 :: forall r. (a -> b) -> a -> (b -> m r) -> m r
      i3 x0v x1v c = c (x0v x1v)

  instance Monad (Parser e m) where
    (>>=) :: forall e m a b. Parser e m a -> (a -> Parser e m b) -> Parser e m b
    x0 >>= x1 = i0 x0 x1
     where
      i0 :: Parser e m a -> (a -> Parser e m b) -> Parser e m b
      i0 (Parser x0) x1 = Parser (i1 x0 x1)
      i1 :: (forall r. e -> (a -> m r) -> m r) -> (a -> Parser e m b) -> (forall r. e -> (b -> m r) -> m r)
      i1 x0 x1 = \e c -> x0 e (\x0v -> i2 e x0v x1 c)
      i2 :: forall r. e -> a -> (a -> Parser e m b) -> (b -> m r) -> m r
      i2 e x0v x1 c = i3 e (x1 x0v) c
      i3 :: forall r. e -> Parser e m b -> (b -> m r) -> m r
      i3 e (Parser x1) c = i4 e x1 c
      i4 :: forall r. e -> (forall r. e -> (b -> m r) -> m r) -> (b -> m r) -> m r
      i4 e x1 c = x1 e (\x1v -> i5 x1v c)
      i5 :: forall r. b -> (b -> m r) -> m r
      i5 x1v c = c x1v

  instance Alternative m => Alternative (Parser e m) where
    empty :: forall e m a. Alternative m => Parser e m a
    empty = i0
     where
      i0 :: Parser e m a
      i0 = Parser i1
      i1 :: forall r. e -> (a -> m r) -> m r
      i1 = \e c -> empty

    (<|>) :: forall e m a. Alternative m => Parser e m a -> Parser e m a -> Parser e m a
    x0 <|> x1 = i0 x0 x1
     where
      i0 :: Parser e m a -> Parser e m a -> Parser e m a
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. e -> (a -> m r) -> m r) -> (forall r. e -> (a -> m r) -> m r) -> (forall r. e -> (a -> m r) -> m r)
      i1 x0 x1 = \e c -> x0 e c <|> x1 e c

  instance Alternative m => MonadPlus (Parser e m) where

  class Monad m => Forkable env m where
    forkEnv :: env -> m env
    margeEnv :: env -> env -> m ()

  onHold :: forall e m a. Forkable e m => Parser e m a -> Parser e m (a, e)
  onHold x = i0 x
   where
    i0 :: Parser e m a -> Parser e m (a, e)
    i0 (Parser x) = Parser (i1 x)
    i1 :: (forall r. e -> (a -> m r) -> m r) -> (forall r. e -> ((a, e) -> m r) -> m r)
    i1 x = \e c -> forkEnv e >>= \e' -> x e' (\xv -> i2 xv e' c)
    i2 :: forall r. a -> e -> ((a, e) -> m r) -> m r
    i2 xv e' c = c (xv, e')

  accept :: forall e m a. Forkable e m => (a, e) -> Parser e m a
  accept x = i0 x
   where
    i0 :: (a, e) -> Parser e m a
    i0 x = Parser (i1 x)
    i1 :: (a, e) -> (forall r. e -> (a -> m r) -> m r)
    i1 x = case x of { (xv, e') -> i2 xv e'; }
    i2 :: a -> e -> (forall r. e -> (a -> m r) -> m r)
    i2 xv e' = \e c -> margeEnv e e' >> c xv

  refuse :: forall e m a. Forkable e m => (a, e) -> Parser e m ()
  refuse x = pure ()

  backtrack :: forall e m a b. Forkable e m => Parser e m (Either a b) -> (a -> Parser e m b) -> Parser e m b
  backtrack x0 x1 = do
    x0 <- onHold x0
    x2 <- case x0 of
      (x0v, e') -> case x0v of
        Left x0a -> x1 x0a
        Right x0b -> accept (x0b, e')
    return x2
