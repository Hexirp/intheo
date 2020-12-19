{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.NoFailure where

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
    pure :: forall p s m a. Monoid p => a -> Parser p s m a
    pure x = i0 x
     where
      i0 :: a -> Parser p s m a
      i0 x = Parser (i1 x)
      i1 :: a -> (forall r. s -> (a -> p -> s -> m r) -> m r)
      i1 x = \s c -> c x mempty s

    (<*>) :: forall p s m a b. Monoid p => Parser p s m (a -> b) -> Parser p s m a -> Parser p s m b
    x0 <*> x1 = i0 x0 x1
     where
      i0 :: Parser p s m (a -> b) -> Parser p s m a -> Parser p s m b
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. s -> ((a -> b) -> p -> s -> m r) -> m r) -> (forall r. s -> (a -> p -> s -> m r) -> m r) -> (forall r. s -> (b -> p -> s -> m r) -> m r)
      i1 x0 x1 = \s c -> x0 s (\x0v x0p x0s -> i2 x0v x0p x0s x1 c)
      i2 :: forall r. (a -> b) -> p -> s -> (forall r. s -> (a -> p -> s -> m r) -> m r) -> (b -> p -> s -> m r) -> m r
      i2 x0v x0p x0s x1 c = x1 x0s (\x1v x1p x1s -> i3 x0v x0p x1v x1p x1s c)
      i3 :: forall r. (a -> b) -> p -> a -> p -> s -> (b -> p -> s -> m r) -> m r
      i3 x0v x0p x1v x1p x1s c = c (x0v x1v) (x0p <> x1p) x1s

  instance Monoid p => Monad (Parser p s m) where
    (>>=) :: forall p s m a b. Monoid p => Parser p s m a -> (a -> Parser p s m b) -> Parser p s m b
    x0 >>= x1 = i0 x0 x1
     where
      i0 :: Parser p s m a -> (a -> Parser p s m b) -> Parser p s m b
      i0 (Parser x0) x1 = Parser (i1 x0 x1)
      i1 :: (forall r. s -> (a -> p -> s -> m r) -> m r) -> (a -> Parser p s m b) -> (forall r. s -> (b -> p -> s -> m r) -> m r)
      i1 x0 x1 = \s c -> x0 s (\x0v x0p x0s -> i2 x0v x0p x0s x1 c)
      i2 :: forall r. a -> p -> s -> (a -> Parser p s m b) -> (b -> p -> s -> m r) -> m r
      i2 x0v x0p x0s x1 c = i3 x0p x0s (x1 x0v) c
      i3 :: forall r. p -> s -> Parser p s m b -> (b -> p -> s -> m r) -> m r
      i3 x0p x0s (Parser x1) c = i4 x0p x0s x1 c
      i4 :: forall r. p -> s -> (forall r. s -> (b -> p -> s -> m r) -> m r) -> (b -> p -> s -> m r) -> m r
      i4 x0p x0s x1 c = x1 x0s (\x1v x1p x1s -> i5 x0p x1v x1p x1s c)
      i5 :: forall r. p -> b -> p -> s -> (b -> p -> s -> m r) -> m r
      i5 x0p x1v x1p x1s c = c x1v (x0p <> x1p) x1s

  instance (Monoid p, Alternative m) => Alternative (Parser p s m) where
    empty :: forall p s m a. (Monoid p, Alternative m) => Parser p s m a
    empty = i0
     where
      i0 :: Parser p s m a
      i0 = Parser i1
      i1 :: forall r. s -> (a -> p -> s -> m r) -> m r
      i1 = \s c -> empty

    (<|>) :: forall p s m a. (Monoid p, Alternative m) => Parser p s m a -> Parser p s m a -> Parser p s m a
    x0 <|> x1 = i0 x0 x1
     where
      i0 :: Parser p s m a -> Parser p s m a -> Parser p s m a
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. s -> (a -> p -> s -> m r) -> m r) -> (forall r. s -> (a -> p -> s -> m r) -> m r) -> (forall r. s -> (a -> p -> s -> m r) -> m r)
      i1 x0 x1 = \s c -> x0 s c <|> x1 s c

  instance (Monoid p, Alternative m) => MonadPlus (Parser p s m) where

  onHold :: forall p s m a. Monoid p => Parser p s m a -> Parser p s m (a, p, s)
  onHold x = i0 x
   where
    i0 :: Parser p s m a -> Parser p s m (a, p, s)
    i0 (Parser x) = Parser (i1 x)
    i1 :: (forall r. s -> (a -> p -> s -> m r) -> m r) -> (forall r. s -> ((a, p, s) -> p -> s -> m r) -> m r)
    i1 x = \s c -> x s (\xv xp xs -> i2 xv xp xs s c)
    i2 :: forall r. a -> p -> s -> s -> ((a, p, s) -> p -> s -> m r) -> m r
    i2 xv xp xs s c = c (xv, xp, xs) mempty s

  accept :: forall p s m a. (a, p, s) -> Parser p s m a
  accept x = i0 x
   where
    i0 :: (a, p, s) -> Parser p s m a
    i0 x = Parser (i1 x)
    i1 :: (a, p, s) -> (forall r. s -> (a -> p -> s -> m r) -> m r)
    i1 x = case x of { (xv, xp, xs) -> i2 xv xp xs; }
    i2 :: a -> p -> s -> (forall r. s -> (a -> p -> s -> m r) -> m r)
    i2 xv xp xs = \s c -> c xv xp xs

  refuse :: forall p s m a. Monoid p => (a, p, s) -> Parser p s m ()
  refuse x = pure ()

  backtrack :: forall p s m e a. Monoid p => Parser p s m (Either e a) -> (e -> Parser p s m a) -> Parser p s m a
  backtrack x0 x1 = do
    x0 <- onHold x0
    x2 <- case x0 of
      (x0v, x0p, x0s) -> case x0v of
        Left x0e -> x1 x0e
        Right x0g -> accept (x0g, x0p, x0s)
    return x2
