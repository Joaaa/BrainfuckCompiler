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

 -- Parses the given String into a StatementBlock, or returns errors if there are any.
parse :: String -> Either String StatementBlock
parse text = evalStateT parseStatements $ tokens text

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

type Error = Either String
type Parser t = StateT [String] Error t

throwError :: String -> Error t
throwError = Left

 -- Convert Maybe to Error with given error message
fromMaybe :: Maybe b -> String -> Error b
fromMaybe (Just a) = const $ Right a
fromMaybe Nothing = throwError

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
    else lift $ throwError $ "Couldn't find enough tokens. Expected: " ++ show n ++ ", found: " ++ show (length tokens)

 -- Get next token without consuming
peekToken :: Parser String
peekToken = head <$> peekTokens 1

 -- Get next token without consuming
peekTokens :: Int -> Parser [String]
peekTokens n = do
  tokens <- get
  if length tokens >= n
    then return $ fst $ splitAt n tokens
    else lift $ throwError $ "Couldn't find enough tokens. Expected: " ++ show n ++ ", found: " ++ show (length tokens)

unexpectedTokenError :: String -> String -> Error t
unexpectedTokenError expected found = throwError $ "Expected \"" ++ expected ++ "\", found \"" ++ found ++ "\"."
