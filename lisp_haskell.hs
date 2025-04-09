import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List

data Exp = Atom String | List Exp Exp deriving (Show)

type Parser = StateT String Maybe

parseChar :: Parser Char
parseChar = StateT f
  where
    f [] = Nothing
    f (x : xs) = Just (x, xs)

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> runStateT p s

parseMany :: Parser a -> Parser [a]
parseMany p =
  let nonEmpty = do
        x <- p
        xs <- parseMany p
        return (x : xs)
   in nonEmpty <|> pure []

parseSpace :: Parser ()
parseSpace = do
  ' ' <- parseChar
  pure ()

parseExp :: Parser Exp
parseExp = spaces >> (atom <|> parseList)
  where
    spaces = parseMany parseSpace
    atom = Atom <$> parseAtom

parseList :: Parser Exp
parseList =
  let parseExpList :: Parser Exp
      parseExpList =
        let nonEmpty = do
              e <- parseExp
              List e <$> parseExpList
         in nonEmpty <|> pure (Atom "Nil")
   in do
   	parseMany parseSpace
        '(' <- parseChar
        list <- parseExpList
   	parseMany parseSpace
        ')' <- parseChar
        pure list

validAtomChar :: Parser Char
validAtomChar = do
  c <- parseChar
  guard (isAlphaNum c)
  pure c

parseAtom :: Parser String
parseAtom = do
  c <- validAtomChar
  (c :) <$> (parseAtom <|> pure "")

printExp :: Exp -> String
printExp (Atom s) = s
printExp (List l r) = let
  printList (Atom "Nil") = ""
  printList (Atom s) = " . " ++ s
  printList (List ll (Atom "Nil")) = printExp ll
  printList (List ll lr) = printExp ll ++ " " ++ printList lr
  in
    "(" ++ printList (List l r) ++ ")"

car :: Exp -> Exp
car (Atom _) = undefined
car (List x _) = x

cdr :: Exp -> Exp
cdr (Atom _) = undefined
cdr (List _ y) = y


lookup :: Exp -> Exp -> Exp
lookup = undefined

eval :: Exp -> Exp -> Exp
eval env exp = match exp of
	Atom x -> lookup env x
