module Parsing(parse) where

import Data.Function ((&))
import Text.Regex.Posix
import Control.Monad.Loops
import Data.String.Utils
import Data.Char
import Control.Monad.State.Lazy hiding (mapM)

import Grammar
import Debug.Trace (trace)
import Text.Read (readMaybe)
import Control.Monad.Identity (Identity)

 -- Parses the given String into a StatementBlock, or prints errors if there are any.
parse :: String -> IO StatementBlock
parse text = do
  let t = tokens text
  print t
  let result = evalStateT parseStatements t
  case result of
    Success program -> do
      print program
      return program
    Error e -> error e


 -- Converts a multiline program string to a list of tokens
tokens :: String -> [String]
tokens s = "{" : removeCombinations (words $ replace "\n" " ; " $ putSpacesAroundSymbols (s ++ "\n")) ++ ["}"]

putSpacesAroundSymbols :: String -> String
putSpacesAroundSymbols text = foldr (uncurry replace) text symbols
  where
    symbols = [([c], [' ', c, ' ']) | c <- ['=', '+', '{', '}', '(', ')', ';', ',']]

removeCombinations :: [String] -> [String]
removeCombinations (";":";":r) = removeCombinations (";":r)
removeCombinations (";":"{":r) = removeCombinations ("{":r)
removeCombinations ("{":";":r) = removeCombinations ("{":r)
removeCombinations (h:r) = h : removeCombinations r
removeCombinations [] = []


 ----- Statement parsing -----

 -- Parse tokens into list of Statements
parseStatements :: Parser StatementBlock
parseStatements = do
  popTokenMatching "{"
  stmts <- whileM (("}" /=) <$> peekToken) parseStatement
  popTokenMatching "}"
  return $ StatementBlock stmts

parseStatement :: Parser Statement
parseStatement = do
  tokens <- get
  stmt <- case tokens of
    ("if":_) -> do -- If statement
      popTokenMatching "if"
      popTokenMatching "("
      expr <- parseExpression
      popTokenMatching ")"
      IfStatement expr <$> parseStatements
    ("while":_) -> do -- While statement
      popTokenMatching "while"
      popTokenMatching "("
      expr <- parseExpression
      popTokenMatching ")"
      WhileStatement expr <$> parseStatements
    (varName:"=":_) -> do -- Assignment
      varName <- popToken
      popToken
      AssignmentStatement (VariableNameLiteral varName) <$> parseExpression
    _ -> -- Expression
      ExpressionStatement <$> parseExpression
  popTokenMatching ";"
  return stmt

splitNextEndl s = case break (";" ==) s of
  (expr, ";":rem) -> (expr, rem)
  (expr, []) -> (expr, [])


 ----- Expression parsing -----

 -- Parse expression
parseExpression :: Parser Expression
parseExpression = do
  tokens <- peekTokens 2
  if (tokens !! 1) == "("
    then do -- Function call
      fName <- popToken
      popTokenMatching "("
      params <- parseFunctionParams
      popTokenMatching ")"
      return $ BuiltinFunctionExpression (head tokens) params
    else do
      token <- popToken
      if isNumber $ head token
             -- Byte literal
        then lift $
             ByteLiteralExpression <$>
             fromMaybe (readMaybe token :: Maybe Int) ("Error parsing number: " ++ token ++ ".")
             -- Variable
        else return $ VariableExpression $ VariableNameLiteral token

 -- Parse function params
parseFunctionParams :: Parser FunctionParams
parseFunctionParams = do
  fst <- peekToken
  if fst == ")"
    then -- Empty brackets
      return $ FunctionParams []
    else do
      firstExpression <- parseExpression
      otherExpressions <- whileM ((")" /=) <$> peekToken) $ popTokenMatching "," >> parseExpression
      return $ FunctionParams $ firstExpression : otherExpressions

 ----- Utilities -----

data Failable t = Success t | Error String
instance Functor Failable where
  fmap = liftM
instance Applicative Failable where
  pure = return
  (<*>) = ap
instance Monad Failable where
  return = Success
  (>>=) (Success t) f = f t
  (>>=) (Error s) _ = Error s

 -- Convert Maybe to Failable with given error message
fromMaybe :: Maybe b -> String -> Failable b
fromMaybe (Just a) _ = Success a
fromMaybe Nothing error = Error error

 -- Replace error string if there is an error
withError :: String -> Failable t -> Failable t
withError new (Error orig) = Error $ new ++ "\nCaused by: " ++ orig
withError _ t = t

type Parser t = StateT [String] Failable t

popToken :: Parser String
popToken = head <$> popTokens 1

popTokenMatching :: String -> Parser String
popTokenMatching s = do
  token <- popToken
  if token == s
    then return token
    else lift $ unexpectedTokenError s token

popTokens :: Int -> Parser [String]
popTokens n = do
  tokens <- get
  if length tokens >= n
    then state $ splitAt n
    else lift $ Error $ "Couldn't find enough tokens. Expected: " ++ show n ++ ", found: " ++ show (length tokens)

 -- Get next token without consuming
peekToken :: Parser String
peekToken = head <$> peekTokens 1

 -- Get next token without consuming
peekTokens :: Int -> Parser [String]
peekTokens n = do
  tokens <- get
  if length tokens >= n
    then return $ fst $ splitAt n tokens
    else lift $ Error $ "Couldn't find enough tokens. Expected: " ++ show n ++ ", found: " ++ show (length tokens)

unexpectedTokenError :: String -> String -> Failable t
unexpectedTokenError expected found = Error $ "Expected \"" ++ expected ++ "\", found \"" ++ found ++ "\"."
