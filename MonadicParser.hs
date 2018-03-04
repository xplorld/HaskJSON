{-# LANGUAGE LambdaCase #-}

module MonadicParser where

import Data.Char
import Control.Applicative
import Control.Monad
-- import Control.MonadZero

newtype Parser a = Parser { parse :: String -> [(a, String)] }

instance Monad Parser where
    m >>= k  = Parser $ \s -> [(x, y) | (u, v) <- parse m s, (x, y) <- parse (k u) v]

instance Applicative Parser where
    pure t = Parser $ \s -> [(t, s)]
    pab <*> pa = Parser $ \s -> [(f a, s2) | (f, s1) <- parse pab s, (a, s2) <- parse pa s1]

instance Functor Parser where
    fmap f m = Parser $ \s -> [(f x, y) | (x, y) <- parse m s]
    -- a <$ pb = Parser $ \s -> [(a, s1) | (_, s1) <- parse pb s]

instance Alternative Parser where
    empty = failure
    p1 <|> p2 = Parser $ \s ->
        case parse p1 s of
            [] -> parse p2 s
            res -> res

-- MonadPlus ++ is actually OR in parser
instance MonadPlus Parser where
    mzero = Parser (const [])
    mplus p q = Parser (\cs -> parse p cs ++ parse q cs)


unit :: a -> Parser a
unit = pure

failure :: Parser a
failure = Parser $ const []

anyChar :: Parser Char
anyChar = Parser $ \case
        c:cs -> [(c, cs)]
        [] -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = anyChar >>= \c ->
  if p c
  then unit c
  else failure

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return ""
string (c:cs) = do {
    _ <- char c; 
    _ <- string cs;
    return (c:cs)
}