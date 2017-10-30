module H4 where

import           Control.Applicative (Alternative(..), Applicative(..))
import           Control.Monad       ((>=>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace, isUpper)
import           Data.Maybe          (fromJust, fromMaybe, isNothing)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

--inParser f = Parser . f . runParser
--fmap = inParser . fmap . fmap . first
--Parser p1 <|> Parser p2 = Parser $ liftA2 <|> p1 p2
instance Functor Parser
    --fmap :: (a -> b) -> f a -> f b
                                     where
  fmap f p = Parser (fmap (first f) . runParser p)

instance Applicative Parser
    --pure :: a -> f a
                       where
  pure x = Parser $ \s -> Just (x, s)
    --(<*>) :: f (a -> b) -> f a -> f b
  p1 <*> p2 =
    Parser $ \s ->
      case runParser p1 s of
        Nothing -> Nothing
        Just (f, r) -> runParser (f <$> p2) r

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = abParser *> pure ()

intPair :: Parser [Integer]
intPair = (\x y -> [x, y]) <$> posInt <* char ' ' <*> posInt

intOrUppercase :: Parser ()
intOrUppercase = posInt *> pure () <|> satisfy isUpper *> pure ()

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

type Ident = String

data Atom
  = N Integer
  | I Ident
  deriving (Show)

data SExpr
  = A Atom
  | Comb [SExpr]
  deriving (Show)

trimSpaces :: Parser a -> Parser a
trimSpaces p = spaces *> p <* spaces

parenthesis :: Parser a -> Parser a
parenthesis p = char '(' *> p <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = trimSpaces $ atom <|> comb
  where
    atom = A <$> parseAtom
    comb = parenthesis $ Comb <$> oneOrMore parseSExpr

parseAtom :: Parser Atom
parseAtom = N <$> posInt <|> I <$> ident

newtype LVar =
  LVar [Atom]
  deriving (Show)

data LExpr =
  LExpr Ident
        LVar
  deriving (Show)

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  p >>= f = Parser $ runParser p >=> (\(a, cs) -> runParser (f a) cs)

-- let var = [var | int]
symbolSkip :: String -> Parser a -> Parser a
symbolSkip cs p = foldr ((*>) . char) p cs

parseLVar :: Parser LVar
parseLVar =
  LVar <$> oneOrMore (trimSpaces parseAtom <* zeroOrMore (char '+') <* spaces)

parseLExpr :: Parser LExpr
parseLExpr =
  Parser $ runParser (trimSpaces $ symbolSkip "let " ident) >=> \(st, ls) ->
      runParser (symbolSkip "=" parseLVar) ls >>= \(st', ls') ->
        Just (LExpr st st', ls')

getVarValue :: [(String, Integer)] -> Atom -> Integer
getVarValue mp (N n) = n
getVarValue mp (I i) = fromJust (lookup i mp)

getVars :: [(String, Integer)] -> [Atom] -> Integer
getVars mp [] = 0
getVars mp (l:ls) = getVars mp ls + getVarValue mp l

opLExpr :: [(String, Integer)] -> [LExpr] -> [LExpr]
opLExpr mp [] = []
opLExpr mp (LExpr var (LVar lst):ls) =
  LExpr var (LVar [N res]) : opLExpr (mp ++ [(var, res)]) ls
  where
    res = getVars mp lst

maybePrettyShow :: [LExpr] -> String
maybePrettyShow [] = ""
maybePrettyShow (LExpr var (LVar al):ls) =
  "let " ++ var ++ " = " ++ show (getVars [] al) ++ "\n" ++ maybePrettyShow ls

l1 = runParser parseLExpr "let x = 1+2+ 3"

l2 = runParser parseLExpr "let y = 1+ x+3"

l3 = runParser parseLExpr "let z = x + y"

ol = opLExpr [] (map (fst . fromJust) [l1, l2, l3])
