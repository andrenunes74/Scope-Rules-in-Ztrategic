-- Perfil em Eng. de Sw
-- Aula de 17/2/2023
{-# LANGUAGE DeriveDataTypeable #-}
module Parser where
import Prelude hiding ((<*>),(<$>))
import Data.Char

infixl 2 <|>
infixl 3 <*>

type Parser r = String -> [(r,String)]

symbola :: String -> [(Char,String)]
symbola []    = []
symbola (h:t) | h == 'a'  = [(h , t)]
              | otherwise = []

symbol :: Char -> Parser Char
symbol c [] = []
symbol c (h:t) | h == c    = [(h,t)]
               | otherwise = []

symbol' c = f <$> symbol c <*> spaces
    where f r1 r2 = r1

satisfy :: (Char -> Bool) -> Parser Char
satisfy p [] = []
satisfy p (h:t) | p h        = [ (h,t)]
                | otherwise  = []

satisfy' p = (\r1 _ -> r1) <$> satisfy p <*> spaces

token :: String -> Parser String
token t [] = []
token t inp = if take (length t) inp == t
              then [(t,drop (length t) inp )]
              else []

token' t = (\r1 _ -> r1) <$> token t <*> spaces

succeed :: a -> Parser a 
succeed r inp = [( r , inp )] 

(<|>) :: Parser a -> Parser a -> Parser a
(p <|> q) inp = p inp ++ q inp

loops  =  token "for"
      <|> token "while"

(<*>) :: Parser (a -> r) -> Parser a -> Parser r
(p <*> q) inp = [ ( f v ,inp'')
                | ( f   ,inp' ) <- p inp
                , (   v ,inp'') <- q inp'
                ]

(<$>)  :: (a -> r) -> Parser a -> Parser r
(f <$> p) inp =  [ ( f v , inp') 
                 | (   v , inp') <- p inp
                 ]

-- a+
--  A -> a
--    |  a A
-- qts a existem!
pA =  f <$> symbol 'a' 
  <|> g <$> symbol 'a' <*> pA
   where f x = 1
         g x y = 1 + y

-- Gramática do quadro
pX =  f <$>  pB <*> pA
   where f b a = (b,a)

pB =   f <$> symbol 'b' <*> pB
  <|>        succeed 0 
  where f  a b = 1 + b
       

-- um ou mais símbolos i
--   i+

pI :: Parser [Char]
pI  =  f <$>  symbol 'i'
   <|> g <$>  symbol 'i' <*> pI
  where f r1    = [r1]
        g r1 r2 = r1:r2

oneOrMore :: Parser a -> Parser [a]
oneOrMore p  =    f <$> p
            <|>   g <$> p <*> oneOrMore p
            where f r1 = [r1]
                  g r1 r2 = r1:r2
-- a*

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = succeed []
            <|> g <$> p <*> zeroOrMore p
            where g r1 r2 = r1:r2
  

spaces = zeroOrMore (satisfy isSpace)

ident = (\a _ -> a) <$> oneOrMore (satisfy isAlpha) <*> spaces
number = (\a _ -> a) <$> oneOrMore (satisfy isDigit) <*> spaces


--
-- Nao funciona com producoes recursivas a esquerda!
-- unica desvantagem dos combinadores de parsing
--
pIErr :: Parser [Char]
pIErr  =   f <$>  symbol 'i'
       <|> g <$>  pIErr <*> symbol 'i' 
  where f r1    = [r1]
        g r1 r2 = r1++[r2]


enclosedBy a pl f = fs <$> a <*> pl <*> f
   where fs r1 r2 r3 = r2


--
--   I -> 
--     | i ',' I
--

followedBy p s =        succeed []
              <|> f <$> p <*> s <*> followedBy p s
              where f r1 _ r3 = r1 : r3

--
-- I -> i
--   |  i ',' I

separatedBy p s = f <$> p <*> s <*> separatedBy p s
               <|> g <$> p
               where f r1 _ r3 = r1 : r3
                     g k1      = [k1]

-- parser para bloco a la C

block a i s f = enclosedBy a (followedBy i s) f

blockC = block (symbol' '{')
               (symbol' 'i')   -- parser de statments
               (symbol' ';')
               (symbol' '}')
