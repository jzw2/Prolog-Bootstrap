import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

data Exp = Lambda Env Exp | Atom String | List Exp Exp deriving (Show, Eq)

type Parser = StateT String Maybe



parseChar :: Parser Char
-- parseChar = StateT f
--   where
--     f [] = Nothing
--     f (x : xs) = Just (x, xs)
parseChar = do
  (x : xs) <- get
  put xs
  pure x

runParse :: Parser a -> String -> Maybe a
runParse p s = fst <$> runStateT p s

parse :: String -> Maybe Exp
parse = runParse parseExp

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
printExp (Lambda _ body) = "<<" ++ printExp body ++ ">>"
printExp (List l r) =
  let printList (Atom "Nil") = ""
      printList (Atom s) = " . " ++ s
      printList (List ll (Atom "Nil")) = printExp ll
      printList (List ll lr) = printExp ll ++ " " ++ printList lr
   in "(" ++ printList (List l r) ++ ")"

car :: Exp -> Either String Exp
car (Atom x) = Left $ "Cannot car " ++ x
car (List x _) = Right x

cdr :: Exp -> Either String Exp
cdr (Atom x) = Left $ "Cannot cdr " ++ x
cdr (List _ y) = Right y

cons :: Exp -> Exp -> Exp
cons = List

nil = Atom "Nil"

type Env = [(String, Int)]

envLookup :: (Env,Store) -> String -> Maybe Exp
envLookup (env, store) string = do
  index <- lookup string (env)
  listToMaybe $ drop index store

type Store = [Exp]

type ProgramState = StateT (Env, Store) (Either String) 

readEval :: String -> ProgramState Exp
readEval str = case parse str of 
  Just x -> return x
  Nothing -> lift (Left "parse error") 

  
  


addBinding :: String -> Exp -> ProgramState ()
addBinding var exp = do
    (env, store) <- get
    put ((var, length store) : env, store ++ [exp])
    return ()

repl :: (Env, Store) -> IO ()
repl oldState = do
  input <- getLine
  case runStateT ((readEval >=> eval) input) oldState of
    Left error -> do
      putStrLn $ "Error: " ++  error
      repl oldState
    Right (output, state) -> do
      putStrLn $ printExp output 
      repl state

main :: IO ()
main = repl ([], [])
  



-- also evaluates the right argument
patternMatch :: Exp -> Exp -> ProgramState (Env, Store)
patternMatch x y | trace ("matching " ++ printExp x ++ " to " ++ printExp y) False = undefined
patternMatch (Atom "Nil") (Atom "Nil") = get
patternMatch (Atom v) exp = do
  eExp <- evalList exp
  addBinding v eExp
  get
patternMatch (List (Atom v) patternXs) (List exp listXs) = do
  eExp <- eval exp
  addBinding v exp
  patternMatch patternXs listXs
patternMatch pattern values = lift $ Left $ "Cannot match " ++ printExp pattern ++ "with " ++ printExp values
  
-- evaluate one after the other
evalList :: Exp -> ProgramState Exp
evalList a | trace ("evallist: " ++ printExp a) False = eval a
evalList (List a (Atom "Nil")) = eval a
evalList (List a (Atom _)) = lift $ Left "bad lambda"
evalList (List a b) = eval a >> eval b

eval :: Exp -> ProgramState Exp
eval exp | trace (printExp exp) False = undefined
eval exp = case exp of
  Atom x -> do
    env <- get
    lift $ Right $ fromMaybe (Atom x) (envLookup env x)
  List (Atom "car") (Atom _) -> lift $ Left "Car called with no arguments"
  List (Atom "car") (List arg (Atom "Nil")) -> do
    e1 <- eval arg
    lift (car e1)
  List (Atom "car") (List _ _) -> lift $ Left "Car with too many arguments"
  List (Atom "cons") (List arg1 (List arg2 (Atom "Nil"))) -> do
    e1 <- eval arg1
    e2 <- eval arg2
    pure $ cons e1 e2
  List (Atom "cons") _ -> lift . Left $ "Arguments for cons incorrect"
  List (Atom "cdr") (List arg (Atom "Nil")) -> do
    e1 <- eval arg
    lift (cdr e1)
  List (Atom "cdr") _ -> lift . Left $ "Incorrect arguments for cdr"
  List (Atom "def") (List (Atom var) (List body (Atom "Nil"))) -> do
    addBinding var body
    return body
  List (Atom "def") _ -> lift $ Left "Incorrect arguments for def"
  (List (Atom "lambda") body) -> do
    (env, _) <- get
    return $ Lambda env body
  List (Lambda lamEnv (List pattern body)) outer | trace ("evaluating " ++ printExp body ++ " applied to " ++ printExp outer) False -> undefined
  List (Lambda lamEnv (List pattern body)) outer -> do
    (env, store) <- get
    trace ("old envs" ++ show (env, store)) $ return ()
    put (lamEnv, store)
    trace ("lam envs" ++ show (lamEnv, store)) return ()
    patternMatch pattern outer
    ret <- evalList body
    (_, newStore) <- get
    put (env, newStore)
    return ret
  List left right -> do
    eLeft <- eval left
    if left == eLeft 
      then lift . Left $ "Cannot further simplify " ++ printExp left
      else eval (List eLeft right)

  other -> lift $ Left $ "unable to evaluate " ++ printExp other

programs :: [(String, String)]
programs = 
  [
   ("(car (cons 1 2))", "1")
   , ("(cdr (cons 1 2))", "2")
   , ("(def x 12)", "12")
   , ("x", "12")
    , ("((lambda x 3) 2)", "3")
    , ("((lambda x x) 2)", "2")
    , ("(((lambda z (lambda y z)) 3) 2)", "3")
  ]
runMultiple :: [String] -> ProgramState [Exp]
runMultiple = mapM (readEval >=> eval)

compareTest :: Either String [(String, String, String)]
compareTest = let p = map fst programs
                  expect = map snd programs
                  actual = evalStateT (runMultiple p) ([], []) in
                  zip3 p expect  . map printExp <$> actual

