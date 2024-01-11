
module Parser
  (
    Parser(..)
  , term
  , statement
  , statements
  )
where

import Control.Monad
import Control.Applicative
import Prelude hiding (repeat)
import Data.Char (isLower, isAlphaNum)
import Data.List (elemIndex)
import Data.Tuple (swap)

import Lambda (Term(..), Type(..))
import Statements (Statement(..))
import State (State(..), get, put)




newtype Parser a = Parser {parse :: String -> Maybe (String, a)}




instance Functor Parser where
  fmap f p = Parser $ \s -> do {(s', a) <- parse p s; return (s', f a)}

instance Applicative Parser where
  pure a   = Parser $ \s -> Just (s, a)
  mf <*> m = mf >>= (\f -> m >>= (\a -> return $ f a))

instance Monad Parser where
  return = pure
  p >>= f  = Parser $ \s -> 
    case parse (fmap f p) s of
      Just (s', p') -> parse p' s'
      Nothing       -> Nothing

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
  
instance MonadPlus Parser where
  mzero       = Parser $ \s -> Nothing
  mplus p1 p2 = Parser $ \s -> parse p1 s `mplus` parse p2 s




item :: Parser Char
item = Parser $ \s -> case s of [] -> Nothing; (c:cs) -> Just (cs, c)

sat :: (Char -> Bool) -> Parser Char
sat pred = do {c <- item; guard (pred c); return c}

char :: Char -> Parser Char
char c = sat ((==) c)

string :: String -> Parser String
string s = traverse sat (map (==) s)

between :: Parser x -> Parser y -> Parser a -> Parser a
between x y p = do {x; v <- p; y; return v}

parens :: Parser a -> Parser a
parens = between (token $ char '(') (token $ char ')')

repeat1 :: Parser a -> Parser [a]
repeat1 p = (:) <$> p <*> repeat p

repeat :: Parser a -> Parser [a]
repeat p = repeat1 p <|> return []

attemptRecurse :: a -> Parser a -> (a -> a -> a) -> Parser a
attemptRecurse a p f = Parser $
  \s -> case parse p s of
          Nothing      -> Just (s,a)
          Just (s',a') -> parse (attemptRecurse (f a a') p f) s'

attempt :: a -> Parser a -> (a -> a -> a) -> Parser a
attempt a p f = Parser $
  \s -> case parse p s of
          Nothing       -> Just (s,a)
          Just (s',a')  -> Just (s',f a a')



whitespace :: Parser String
whitespace = repeat $ sat $ \c -> c `elem` " \n\t"

lineComment :: Parser ()
lineComment = string "--" >> repeat (do {c <- item; guard (c /= '\n')}) >> char '\n' >> return ()

identifier :: Parser String
identifier = (:) <$> sat isLower <*> repeat (sat isAlphaNum <|> char '\'')

token :: Parser a -> Parser a
token p = do {v <- p; whitespace; return v}

begin :: Parser a -> Parser a
begin p = whitespace >> p

index :: [String] -> Parser String -> Parser Int
index ctx p = do {str <- p; case elemIndex str ctx of Nothing -> mzero; Just i -> return i}

term0 :: Bool -> [String] -> Parser Term
term0 typed ctx = var <|> lambda <|> token (parens $ term typed ctx)
  where var    = Var <$> index ctx (token identifier) 
        lambda = do
            vs <- token (char '\\') >> repeat1 (token identifier)
            t  <-     (Just <$> (guard typed *> token (char ':') *> ttype)) 
                  <|> (guard (not typed) >> return Nothing)
            e  <- token (char '.') >> term typed (reverse vs ++ ctx)
            return $ Lambda vs t e

term :: Bool -> [String] -> Parser Term
term typed ctx = do
  x <- term0 typed ctx
  attemptRecurse x (term0 typed ctx) App

ttype :: Parser Type
ttype = do 
  x <- (TVar <$> token identifier) <|> parens ttype
  attempt x (token (string "->") >> ttype) Arrow



statement :: Bool -> [String] -> Parser Statement
statement b ctx = typed <|> untyped <|> eval <|> typeOf <|> define
  where typed   = between (token $ string "(#") (token $ string "#)") (token $ string "TYPED") >> return TBlock
        untyped = between (token $ string "(#") (token $ string "#)") (token $ string "UNTYPED") >> return UTBlock
        eval    = Eval <$> ((token $ string "eval") >> term b ctx)
        typeOf  = TypeOf <$> ((token $ string "typeof") >> term b ctx)
        define  = do
          token $ string "define"
          name <- token identifier
          args <-     fmap (\ss -> map (\s -> (s, Nothing)) ss) (guard (not b) >> repeat (token $ identifier))
                  <|> (guard b >> (repeat $ parens $ do {v <- token identifier; token $ string ":"; t <- token ttype; return (v, Just t)}))
          token $ string ":="
          e <- term b $ (reverse $ fst $ unzip args) ++ name : ctx
          return $ Define name args e


statements :: Parser [Statement]
statements = Parser $ 
  \s ->
    let state s = do {
        (b, ctx) <- get;
        case parse (statement b ctx) s of
          Just (s', result) -> do
            case result of
              TBlock       -> put (True,  [])
              UTBlock      -> put (False, [])
              Define n _ _ -> put (b,     n:ctx)
              _            -> return ()
            (xs, s'') <- state s'
            return (result : xs, s'')
          Nothing -> return ([], s)
    } 

    in Just $ swap $ fst $ run (state s) (False, [])
          