{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- https://fumieval.hatenablog.com/entry/2013/03/09/180504
-- https://github.com/fumieval/mtl-c/blob/2eca50ba70e9919827bfe124e1200cc9c2af0532/Control/Monad/Writer/CPS.hs
--
-- 上記の二つを参考にした。
module Parser.NoFailureReaderStrict where
  import Prelude

  import Control.Applicative
  import Control.Monad

  import Control.DeepSeq

  newtype Parser e p s m a = Parser { runParser :: forall r. e -> s -> (a -> p -> s -> m r) -> m r }

  instance Functor (Parser e p s m) where
    fmap :: forall e p s m a b. (a -> b) -> Parser e p s m a -> Parser e p s m b
    fmap f x = i0 f x
     where
      i0 :: (a -> b) -> Parser e p s m a -> Parser e p s m b
      i0 f (Parser x) = Parser (i1 f x)
      i1 :: (a -> b) -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (forall r. e -> s -> (b -> p -> s -> m r) -> m r)
      i1 f x = \e s c -> x e s (\xv xp xs -> c (f xv) xp xs)

  instance (Monoid p, NFData p) => Applicative (Parser e p s m) where
    pure :: forall e p s m a. (Monoid p, NFData p) => a -> Parser e p s m a
    pure x = i0 x
     where
      i0 :: a -> Parser e p s m a
      i0 x = Parser (i1 x)
      i1 :: a -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r)
      i1 x = \e s c -> let p' = mempty in rnf p' `seq` c x p' s

    (<*>) :: forall e p s m a b. (Monoid p, NFData p) => Parser e p s m (a -> b) -> Parser e p s m a -> Parser e p s m b
    x0 <*> x1 = i0 x0 x1
     where
      i0 :: Parser e p s m (a -> b) -> Parser e p s m a -> Parser e p s m b
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. e -> s -> ((a -> b) -> p -> s -> m r) -> m r) -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (forall r. e -> s -> (b -> p -> s -> m r) -> m r)
      i1 x0 x1 = \e s c -> x0 e s (\x0v x0p x0s -> i2 e x0v x0p x0s x1 c)
      i2 :: forall r. e -> (a -> b) -> p -> s -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (b -> p -> s -> m r) -> m r
      i2 e x0v x0p x0s x1 c = x1 e x0s (\x1v x1p x1s -> i3 x0v x0p x1v x1p x1s c)
      i3 :: forall r. (a -> b) -> p -> a -> p -> s -> (b -> p -> s -> m r) -> m r
      i3 x0v x0p x1v x1p x1s c = let p' = x0p <> x1p in rnf p' `seq` c (x0v x1v) p' x1s

  instance (Monoid p, NFData p) => Monad (Parser e p s m) where
    (>>=) :: forall e p s m a b. (Monoid p, NFData p) => Parser e p s m a -> (a -> Parser e p s m b) -> Parser e p s m b
    x0 >>= x1 = i0 x0 x1
     where
      i0 :: Parser e p s m a -> (a -> Parser e p s m b) -> Parser e p s m b
      i0 (Parser x0) x1 = Parser (i1 x0 x1)
      i1 :: (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (a -> Parser e p s m b) -> (forall r. e -> s -> (b -> p -> s -> m r) -> m r)
      i1 x0 x1 = \e s c -> x0 e s (\x0v x0p x0s -> i2 e x0v x0p x0s x1 c)
      i2 :: forall r. e -> a -> p -> s -> (a -> Parser e p s m b) -> (b -> p -> s -> m r) -> m r
      i2 e x0v x0p x0s x1 c = i3 e x0p x0s (x1 x0v) c
      i3 :: forall r. e -> p -> s -> Parser e p s m b -> (b -> p -> s -> m r) -> m r
      i3 e x0p x0s (Parser x1) c = i4 e x0p x0s x1 c
      i4 :: forall r. e -> p -> s -> (forall r. e -> s -> (b -> p -> s -> m r) -> m r) -> (b -> p -> s -> m r) -> m r
      i4 e x0p x0s x1 c = x1 e x0s (\x1v x1p x1s -> i5 x0p x1v x1p x1s c)
      i5 :: forall r. p -> b -> p -> s -> (b -> p -> s -> m r) -> m r
      i5 x0p x1v x1p x1s c = let p' = x0p <> x1p in rnf p' `seq` c x1v p' x1s

  instance (Monoid p, NFData p, Alternative m) => Alternative (Parser e p s m) where
    empty :: forall e p s m a. (Monoid p, NFData p, Alternative m) => Parser e p s m a
    empty = i0
     where
      i0 :: Parser e p s m a
      i0 = Parser i1
      i1 :: forall r. e -> s -> (a -> p -> s -> m r) -> m r
      i1 = \e s c -> empty

    (<|>) :: forall e p s m a. (Monoid p, Alternative m) => Parser e p s m a -> Parser e p s m a -> Parser e p s m a
    x0 <|> x1 = i0 x0 x1
     where
      i0 :: Parser e p s m a -> Parser e p s m a -> Parser e p s m a
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 :: (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r)
      i1 x0 x1 = \e s c -> x0 e s c <|> x1 e s c

  instance (Monoid p, NFData p, Alternative m) => MonadPlus (Parser e p s m) where

  onHold :: forall e p s m a. (Monoid p, NFData p) => Parser e p s m a -> Parser e p s m (a, p, s)
  onHold x = i0 x
   where
    i0 :: Parser e p s m a -> Parser e p s m (a, p, s)
    i0 (Parser x) = Parser (i1 x)
    i1 :: (forall r. e -> s -> (a -> p -> s -> m r) -> m r) -> (forall r. e -> s -> ((a, p, s) -> p -> s -> m r) -> m r)
    i1 x = \e s c -> x e s (\xv xp xs -> i2 xv xp xs s c)
    i2 :: forall r. a -> p -> s -> s -> ((a, p, s) -> p -> s -> m r) -> m r
    i2 xv xp xs s c = let p' = mempty in rnf p' `seq` c (xv, xp, xs) p' s

  accept :: forall e p s m a. (a, p, s) -> Parser e p s m a
  accept x = i0 x
   where
    i0 :: (a, p, s) -> Parser e p s m a
    i0 x = Parser (i1 x)
    i1 :: (a, p, s) -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r)
    i1 x = case x of { (xv, xp, xs) -> i2 xv xp xs; }
    i2 :: a -> p -> s -> (forall r. e -> s -> (a -> p -> s -> m r) -> m r)
    i2 xv xp xs = \e s c -> c xv xp xs

  refuse :: forall e p s m a. (Monoid p, NFData p) => (a, p, s) -> Parser e p s m ()
  refuse x = pure ()

  backtrack :: forall e p s m a b. (Monoid p, NFData p) => Parser e p s m (Either a b) -> (a -> Parser e p s m b) -> Parser e p s m b
  backtrack x0 x1 = do
    x0 <- onHold x0
    x2 <- case x0 of
      (x0v, x0p, x0s) -> case x0v of
        Left x0e -> x1 x0e
        Right x0g -> accept (x0g, x0p, x0s)
    return x2
