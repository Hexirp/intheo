module Parser.NaivePos where
  import Prelude

  import Control.Applicative
  import Control.Monad

  newtype Parser p a = Parser { runParser :: String -> [(a, p, String)] }

  instance Functor (Parser p) where
    fmap f x = i0 f x
     where
      i0 f (Parser x) = Parser (i1 f x)
      i1 f x = \s -> i2 f (x s)
      i2 f x = case x of { [] -> []; xv : xs -> i3 f xv : i2 f xs; }
      i3 f x = case x of { (v, p, s) -> (f v, p, s); }

  instance Monoid p => Applicative (Parser p) where
    pure v = Parser (\s -> [(v, mempty, s)])

    x0 <*> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) x1
      i2 x0 x1 = case x0 of { [] -> []; x0v : x0s -> i3 x0v x1 (i2 x0s x1); }
      i3 x0 x1 r = case x0 of { (v0, p0, s0) -> i4 v0 p0 (x1 s0) r; }
      i4 v0 p0 x1 r = case x1 of { [] -> r; x1v : x1s -> i5 v0 p0 x1v : i4 v0 p0 x1s r; }
      i5 v0 p0 x1 = case x1 of { (v1, p1, s1) -> (v0 v1, p0 <> p1, s1); }

  instance Monoid p => Monad (Parser p) where
    x0 >>= x1 = i0 x0 x1
     where
      i0 (Parser x0) x1 = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) x1
      i2 x0 x1 = case x0 of { [] -> []; x0v : x0s -> i3 x0v x1 (i2 x0s x1); }
      i3 x0 x1 r = case x0 of { (v0, p0, s0) -> i4 p0 s0 (x1 v0) r; }
      i4 p0 s0 (Parser x1) r = i5 p0 (x1 s0) r
      i5 p0 x1 r = case x1 of { [] -> r; x1v : x1s -> i6 p0 x1v : i5 p0 x1s r; }
      i6 p0 x1 = case x1 of { (v1, p1, s1) -> (v1, p0 <> p1, s1); }

  instance Monoid p => Alternative (Parser p) where
    empty = Parser (\s -> [])

    x0 <|> x1 = i0 x0 x1
     where
      i0 (Parser x0) (Parser x1) = Parser (i1 x0 x1)
      i1 x0 x1 = \s -> i2 (x0 s) (x1 s)
      i2 x0 x1 = case x0 of { [] -> x1; x0v : x0s -> x0v : i2 x0s x1; }

  instance Monoid p => MonadPlus (Parser p) where

  tell :: p -> Parser p ()
  tell p = Parser (\s -> [((), p, s)])

  token_raw :: Monoid p => Parser p Char
  token_raw = Parser (\s -> case s of { [] -> []; sv : ss -> [(sv, mempty, ss)]; })

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
