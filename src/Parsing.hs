module Parsing where

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

 -- Entry point: Parses the given String
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
tokens s = "{" : removeCombinations (words $ replace "\n" " ; " $ putSpacesAroundSymbols s) ++ ["}"]

putSpacesAroundSymbols :: String -> String
putSpacesAroundSymbols text = foldr (uncurry replace) text symbols
  where
    symbols = [([c], [' ', c, ' ']) | c <- ['=', '+', '{', '}', '(', ')', ';']]

removeCombinations :: [String] -> [String]
removeCombinations (";":";":r) = removeCombinations (";":r)
removeCombinations (";":"{":r) = removeCombinations ("{":r)
removeCombinations (";":"}":r) = removeCombinations ("}":r)
removeCombinations ("{":";":r) = removeCombinations ("{":r)
removeCombinations ("}":";":r) = removeCombinations ("}":r)
removeCombinations (h:r) = h : removeCombinations r
removeCombinations [] = []


 ----- Statement parsing -----

 -- Parse tokens into list of Statements
parseStatements :: Parser StatementBlock
parseStatements = do
  token <- popToken
  sb <- case token of
    "{" -> do
      stmts <- whileM (("}" /=) <$> peekToken) parseStatement
      return $ StatementBlock stmts
    _ -> lift $ unexpectedTokenError "{" token
  token <- popToken
  if token == "}"
    then return sb
    else lift $ unexpectedTokenError "}" token

parseStatement :: Parser Statement
parseStatement = do
  tokens <- get
  stmt <- case tokens of
    (varName:"=":_) -> do
      varName <- popToken
      popToken
      AssignmentStatement (VariableNameLiteral varName) <$> parseExpression
    _ -> ExpressionStatement <$> parseExpression
  token <- popToken
  if token == ";"
    then return stmt
    else lift $ unexpectedTokenError ";" token

splitNextEndl s = case break (";" ==) s of
  (expr, ";":rem) -> (expr, rem)
  (expr, []) -> (expr, [])


 ----- Expression parsing -----

 -- Parse expression
parseExpression :: Parser Expression
parseExpression = do
  token <- popToken
  if isNumber $ head token
    then -- Byte literal
      lift $ ByteLiteralExpression <$> fromMaybe (readMaybe token :: Maybe Int) ("Error parsing number: "++token++".")
    else -- Variable
      return $ VariableExpression $ VariableNameLiteral token


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
