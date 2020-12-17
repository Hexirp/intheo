{-# LANGUAGE RankNTypes #-}

module Parser.NaivePosCps where
  import Prelude

  import Control.Applicative
  import Control.Monad

  newtype Parser p a = Parser { runParser :: forall r. String -> r -> (a -> p -> String -> r -> r) -> r }

  instance Functor (Parser p) where
    fmap f x = i0 f x
     where
      i0 f (Parser x) = Parser (i1 f x)
      i1 f x = \s n c -> x s n (\xvv xvp xvs xs -> c (f xvv) xvp xvs xs)

  instance Monoid p => Applicative (Parser p) where
    pure xv = Parser (\xs xn xc -> xc xv mempty xs xn)

    x0 <*> x1 = i0 x0 x1
     where
      i0 = undefined

  instance Monoid p => Monad (Parser p) where
    x0 >>= x1 = i0 x0 x1
     where
      i0 = undefined

  instance Monoid p => Alternative (Parser p) where
    empty = Parser (\s n c -> n)

    x0 <|> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (\s n c -> x0 s (x1 s n c) c)

  instance Monoid p => MonadPlus (Parser p) where

  tell :: p -> Parser p ()
  tell p = undefined

  token_raw :: Monoid p => Parser p Char
  token_raw = undefined

  data Pos = Pos Word Word

  instance Semigroup Pos where
    Pos x0 x1 <> Pos y0 y1
      | y0 == 0 = Pos x0 (x1 + y1)
      | y0 >  0 = Pos (x0 + y0) 0

  instance Monoid Pos where
    mempty = Pos 0 0

  token :: Parser Pos Char
  token = tell (Pos 0 1) >> token_raw

  match_token :: Char -> Parser Pos Char
  match_token c = do
    t <- token
    if t == c then pure t else empty

  match_tokens :: String -> Parser Pos String
  match_tokens s = case s of { [] -> pure []; sv : ss -> do { t <- match_token sv; r <- match_tokens ss; return (t : r); }; }

  new_line :: Parser Pos String
  new_line =
    (tell (Pos 1 0) >> match_tokens "\r") <|>
    (tell (Pos 1 0) >> match_tokens "\r\n") <|>
    (tell (Pos 1 0) >> match_tokens "\n") <|>
    (tell (Pos 1 0) >> match_tokens "\v") <|>
    (tell (Pos 1 0) >> match_tokens "\f") <|>
    (tell (Pos 1 0) >> match_tokens "\0085") <|>
    (tell (Pos 1 0) >> match_tokens "\2028") <|>
    (tell (Pos 1 0) >> match_tokens "\2029") <|>
    empty 

  choice :: (Ord t, Monoid p) => [(t, a -> Parser p a)] -> a -> Parser p a
  choice x z = undefined

