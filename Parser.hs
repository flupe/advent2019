module Parser where

import Control.Applicative
import Data.Functor
import Data.Bifunctor (first)
import Data.List (uncons, stripPrefix)
import Data.Char (isDigit, isAlpha)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

eat :: Parser a -> String -> a
eat p s =
    case parse p s of
        Just (r, []) -> r
        Just (_, rs) -> error $ "Parser did not consume entire stream, remains: " ++ rs
        _            -> error "Parser error"

instance Functor Parser where
    fmap f p = Parser \s -> fmap (first f) (parse p s)

instance Applicative Parser where
    pure x  = Parser \s -> Just (x, s)
    f <*> x = Parser \s -> case parse f s of
                            Just (f, s) -> parse (f <$> x) s
                            Nothing     -> Nothing

instance Alternative Parser where
    empty   = Parser $ const Nothing
    p <|> q =  Parser \s -> case parse p s of
                                 Nothing -> parse q s
                                 res     -> res

-- | Retrieve first char of input.
item :: Parser Char
item = Parser uncons

-- | Retrieve at most `n` chars of input.
chars :: Int -> Parser String
chars n = Parser $ Just . splitAt n

-- | Retrieve prefix whose chars satisfy predicate `f`.
takeWhile' :: (Char -> Bool) -> Parser String
takeWhile' f = Parser \case x:xs | f x -> (first (x:)) <$> parse (takeWhile' f) xs
                            s          -> Just("", s)

-- | Consume specified prefix of input.
string :: String -> Parser ()
string p = Parser \s -> ((),) <$> stripPrefix p s

-- | Consume integer prefix of input.
integer :: Parser Int
integer = read <$> takeWhile' isDigit

-- | Consume alphabetic prefix of input.
identifier :: Parser String
identifier = takeWhile' isAlpha

-- | Repeat parser zero or more times.
some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> many' p

-- | Repeat parser one or more times.
many' :: Parser a -> Parser [a]
many' p = some' p <|> pure []

sepBy :: String -> Parser a -> Parser [a]
sepBy sep p = (:) <$> p <*> many' (string sep *> p)

space = string " "
