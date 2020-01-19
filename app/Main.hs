module Main where

import Lib
import Control.Monad.State.Lazy hiding (mapM)
import GHC.Base hiding (mapM)
import Util
import Text.Read (readMaybe)
import Data.List

newtype VariableName = VariableName String deriving (Eq, Show)
newtype MemoryAddress = MemoryAddress Int deriving (Eq)
data Variable = Variable {variableName :: VariableName, variableAddress :: MemoryAddress}
data Env = Env {stackPointer :: MemoryAddress, variables :: [Variable], code :: String}
emptyEnv = Env (MemoryAddress 0) [] ""

newtype Token = Token String

variableExists :: VariableName -> State Env Bool
variableExists name = elem name . map variableName . variables <$> get

memoryDiff :: MemoryAddress -> MemoryAddress -> Int
memoryDiff (MemoryAddress a) (MemoryAddress b) = a - b

findAddress :: VariableName -> State Env MemoryAddress
findAddress name = do
  env <- get
  case find (\v -> name == variableName v) $ variables env of
    Just var -> return $ variableAddress var
    Nothing -> error $ "Variable " ++ show name ++ " not found."

-- Find variable offset from current stack pointer
memoryOffset :: VariableName -> State Env Int
memoryOffset name = do
  address <- findAddress name
  sp <- stackPointer <$> get
  return $ memoryDiff sp address

addCode :: String -> State Env ()
addCode c = do
  env <- get
  let code' = code env ++ c
  let env' = Env (stackPointer env) (variables env) code'
  put env'

createVar :: VariableName -> State Env ()
createVar name = do
  env <- get
  let variables' = variables env ++ [Variable name $ stackPointer env]
  let sp' = MemoryAddress $ a + 1 where MemoryAddress a = stackPointer env
  put $ Env sp' variables' $ code env
  addCode ">"

sqrtAndRem :: Int -> (Int, Int)
sqrtAndRem n = intSqrt' n 1 where intSqrt' n a = if a*a > n then (a-1, n-((a-1)*(a-1))) else intSqrt' n $ a+1

moveCommand :: Int -> State Env ()
moveCommand n
  | n > 0 = moveRightCommand n
  | otherwise = moveLeftCommand (negate n)
  where
    moveRightCommand = addCode . flip replicate '>'
    moveLeftCommand = addCode . flip replicate '<'
onRelativePosition :: Int -> State Env () -> State Env ()
onRelativePosition offset s = do
  moveCommand offset
  s
  moveCommand (negate offset)
incrementCommand :: Int -> State Env ()
incrementCommand n
  | n < 10 = incrementSimple n
  | otherwise = do
    let (root, rem) = sqrtAndRem n
    onRelativePosition 1 $ do
      incrementSimple root
      inBrackets $ do
        decrementCommand 1
        onRelativePosition (-1) $ incrementSimple root
    incrementSimple rem
  where
    incrementSimple a = addCode $ replicate a '+'
decrementCommand :: Int -> State Env ()
decrementCommand = addCode . flip replicate '-'

printCommand = addCode "."
readCommand = addCode ","
inBrackets s = do
  addCode "["
  s
  addCode "]"

-- Copies variable value into current stack address
readVariableCommand :: VariableName -> State Env ()
readVariableCommand name = do
  offset <- memoryOffset name
  onRelativePosition (negate offset) $
    inBrackets $ do
      decrementCommand 1
      onRelativePosition offset $ do
        incrementCommand 1
        onRelativePosition 1 $ incrementCommand 1
  onRelativePosition 1 $
    inBrackets $ do
      decrementCommand 1
      onRelativePosition (negate (offset + 1)) $ incrementCommand 1

-- Moves current stack address value into variable
setVariableCommand :: VariableName -> State Env ()
setVariableCommand name = do
  offset <- memoryOffset name
  inBrackets $ do
    decrementCommand 1
    onRelativePosition (negate offset) $ incrementCommand 1

-- Sets current stack value to zero
reset :: State Env ()
reset = inBrackets $ decrementCommand 1

-- Adds value into current stack value. Value can be int or variable
readValueCommand :: String -> State Env ()
readValueCommand value = case readMaybe value :: Maybe Int of
  Just value' -> incrementCommand value'
  Nothing -> readVariableCommand $ VariableName value

-- Reads char into current stack value.
readCharCommand :: Char -> State Env ()
readCharCommand c = incrementCommand $ ord c

-- Creates var if it doesn't exist, returns its name
getVar :: String -> State Env VariableName
getVar varName = do
  let var = VariableName varName
  exists <- variableExists var
  unless exists $ createVar var
  return var

processLine :: [String] -> State Env ()
processLine ["set", varName, value] = do
  var <- getVar varName
  readValueCommand value
  setVariableCommand var
processLine ["print", value] = do
  readValueCommand value
  printCommand
  reset
processLine ["printString", value] =
  forM_ value $ \c -> do
    readCharCommand c
    printCommand
    reset
processLine ["add", varName, value1, value2] = do
  var <- getVar varName
  readValueCommand value1
  readValueCommand value2
  setVariableCommand var
processLine ["read", varName] = do
  var <- getVar varName
  readCommand
  setVariableCommand var
processLine _ = return ()

main :: IO ()
main = do
  lines <- readFileLines "test.bf"
--  print $ map parseTokens lines
  putStrLn $ code $ execState (mapM (processLine . parseTokens) lines) emptyEnv

readFileLines :: String -> IO [String]
readFileLines fn = lines <$> readFile fn

parseTokens :: String -> [String]
parseTokens "" = []
parseTokens (' ':s) = [] : parseTokens s
parseTokens ('"':s) = parseString s where
  parseString ('"':s') = parseTokens s'
  parseString (h:t) = case parseString t of
                        [] -> [[h]]
                        h':t' -> (h:h'):t'
parseTokens (h:t) = case parseTokens t of
  [] -> [[h]]
  h':t' -> (h:h'):t'
