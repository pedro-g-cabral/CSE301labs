module Parser (runParser, parseLExp, parseCmd, parseCustom, readParser) where

import Expr
import Cmd

import Data.Maybe
import Data.Char

import Control.Applicative

-- We begin by defining the type of parsers for a type:
newtype Parser a = Parser { runParser :: String -> Maybe (a,String) }

-- The idea is that a value of type Parser a is something that takes a
-- string as input, and tries to parse a prefix of the input as a value
-- of type a.  If it succeeds, it returns Just a value of type (a,String),
-- and otherwise it returns Nothing.

-- Or in other words, adapting a poem by Graham Hutton, 

  -- A parser for things
  -- Is a function from strings
  -- To maybe a pair
  -- Of a thing and a string!

-- We can define a Monad instance for Parser
instance Monad Parser where
  -- return :: a -> Parser a
  return x = Parser (\s -> Just (x,s))

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f  = Parser (\s -> case runParser p s of
                             Nothing -> Nothing
                             Just (x,s') -> runParser (f x) s')

-- We add some boilerplate code to derive Functor and Applicative
-- instances from the Monad instance
instance Functor Parser where
  fmap f p = p >>= \x -> return (f x)

instance Applicative Parser where
  pure = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

-- Note that the type Parser a is isomorphic to StateT String Maybe a,
-- and we could have defined it that way to automatically derive all
-- these type class instances, but we prefer to do it for ourselves.

-- We also define an Alternative instance, which makes it convenient
-- to write backtracking parsers.
instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (\s -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\s -> case runParser p1 s of
                                 Just (x,s') -> Just (x,s')
                                 Nothing -> runParser p2 s)

-- The idea is that "empty" is a parser that always fails, while
-- p1 <|> p2 is a parser that first tries to parse a string using p1,
-- and if that fails tries parsing the same string using p2.

-- Now we define parsers for various kinds of basic stuff, as well as
-- parser "combinators" which combine one or more parsers to define a
-- new parser.

-- The "item" parser just reads one character of the input and returns it.
-- Note there must be at least one character of input for item to succeed.
item :: Parser Char
item = Parser $ \s -> case s of
                        []     -> Nothing
                        (x:s') -> Just (x,s')

-- dually, the "end" parser detects when we have reached the end of input.
end :: Parser ()
end = Parser $ \s -> if null s then Just ((),"") else Nothing

-- sat p parses a character satisfying the predicate p
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- parse a specific character
char :: Char -> Parser Char
char c = sat (==c)

-- parse a letter of the alphabet
letter :: Parser Char
letter = sat isAlpha

-- parse an alphanumeric character
alphanum :: Parser Char
alphanum = sat isAlphaNum

-- parse a space character
space :: Parser Char
space = sat isSpace

-- parse a specific string of characters
string :: String -> Parser String
string []     = return []
string (c:cs) = char c >> string cs >> return (c:cs)

-- the paren parser combinator turns a parser of something into a
-- parser of something wrapped in parentheses
paren :: Parser a -> Parser a
paren p = do
  char '('
  spaces
  x <- p
  spaces
  char ')'
  return x

-- list p turns a parser of p's into a parser of lists of p's.  It is
-- defined in mutual recursion with listne p, which builds a parser of
-- non-empty lists of p's.
list, listne :: Parser a -> Parser [a]
list p = listne p <|> pure []
listne p = do
  x <- p
  xs <- list p
  return (x:xs)

-- separatedBy p q is similar, parsing a non-empty list of p's separated by q's
separatedBy :: Parser a -> Parser b -> Parser [a]
separatedBy p q = do
  x <- p
  xs <- list (q >> p)
  return (x:xs)

-- spaces parses some spaces
spaces :: Parser [Char]
spaces = list space

-- Using these basic parsers and parser combinators, we now define
-- parsers for expressions and commands.

-- We begin by defining a parser for variable names, which have to
-- begin with a letter, followed by a sequence of alphanumeric
-- characters or the "prime" symbol '.

var :: Parser Var
var = do
  base <- letter
  rest <- list (alphanum <|> char '\'')
  return (base:rest)

-- The expression parser is defined in mutual recursion with parsers
-- for the various kinds of subexpressions.  To implement the standard
-- convention that treats application as a left-associative operator
-- (thereby reducing the number of parentheses needed), we parse an
-- expression by trying to parse it either as a lambda abstraction or
-- as a sequence of applications.

parseLExp :: Parser LExp
parseLExp = parseLam <|> parseApps

-- Parse a lambda abstraction expression.
parseLam :: Parser LExp
parseLam = do
  char '\\' <|> char 'λ'        -- parse backslash or unicode λ for the lambda symbol
  x <- var                      -- parse a variable name
  char '.'                      -- parse a dot
  t <- parseLExp                -- parse an expression
  return (L x t)                -- return the lambda abstraction

-- Parse a left-nested sequence of applications.
parseApps :: Parser LExp
parseApps = do
  ts <- separatedBy (parseVar <|> paren parseLExp) spaces     -- parse a list of variables or parenthesized expressions,
                                                              --   separated by spaces.
  return (foldl A (head ts) (tail ts))                        -- return a left-nested sequence of application

-- Parse a variable expression
parseVar :: Parser LExp
parseVar = var >>= \x -> return (V x)

-- The command parser is a bit simpler, since the language of
-- interpreter commands is pretty simple.  We define parsers
-- for each of the four kinds of commands.

-- Parse a command.
parseCmd :: Parser Cmd
parseCmd = parseEval <|> parseLet <|> parseNoop <|> parseQuit

-- Parse an eval command.
parseEval :: Parser Cmd
parseEval = do
  t <- parseLExp                -- parse an expression
  end                           -- reached end of input
  return (Eval t)               -- return an eval command

-- Parse a let command
parseLet :: Parser Cmd
parseLet = do
  (string "let" >> space >> return ()) <|> return ()    -- allow the optional string "let" at the beginning
  spaces
  x <- var                                              -- parse a variable name
  spaces
  char '='                                              -- equality sign
  spaces
  t <- parseLExp                                        -- parse an expression
  end                                                   -- reached end of input
  return (Let x t)                                      -- return a let command

-- Parse a no-op command
parseNoop :: Parser Cmd
parseNoop = do
  end
  return Noop

-- Parse a quit command
parseQuit :: Parser Cmd
parseQuit = do
  string ":quit" <|> string ":q"
  end
  return Quit

readParser :: MonadFail m => Parser a -> String -> m a
readParser p s = case runParser p s of
                   Just (x,s') -> if null s' then return x else fail "readParser: input left"
                   Nothing -> fail "readParser: failed to parse"

-- Custom Parser (For testing)
parseCustom = parseQuit <|> parseNoop