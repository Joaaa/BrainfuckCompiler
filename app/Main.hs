module Main where

import Control.Monad.State.Lazy hiding (mapM)
import GHC.Base hiding (mapM)
import Util
import Text.Read (readMaybe)
import Data.List
import Data.Time.Clock.POSIX

import Grammar
import qualified Parsing

newtype MemoryAddress = MemoryAddress {rawValue :: Int} deriving (Eq)

data Variable = Variable {variableName :: VariableNameLiteral, variableAddress :: MemoryAddress}
data Env = Env {stackPointer :: MemoryAddress, variables :: [Variable], code :: String}
emptyEnv = Env (MemoryAddress 0) [] ""

variableExists :: VariableNameLiteral -> State Env Bool
variableExists name = elem name . map variableName . variables <$> get

memoryDiff :: MemoryAddress -> MemoryAddress -> Int
memoryDiff (MemoryAddress a) (MemoryAddress b) = a - b

findAddress :: VariableNameLiteral -> State Env MemoryAddress
findAddress name = do
  env <- get
  case find (\v -> name == variableName v) $ variables env of
    Just var -> return $ variableAddress var
    Nothing -> error $ "Variable " ++ show name ++ " not found."

-- Find variable offset from current stack pointer
memoryOffset :: MemoryAddress -> State Env Int
memoryOffset address = do
  sp <- stackPointer <$> get
  return $ memoryDiff sp address

setStackPointer :: MemoryAddress -> State Env ()
setStackPointer sp = get >>= \env -> put $ env {stackPointer = sp}

setVariables :: [Variable]-> State Env ()
setVariables vars = get >>= \env -> put $ env {variables = vars}

setCode :: String -> State Env ()
setCode code = get >>= \env -> put $ env {code = code}

addCode :: String -> State Env ()
addCode c = do
  env <- get
  setCode $ code env ++ c

 -- Get the current stack memory address and move the stack pointer by one
reserveStackAddress :: State Env MemoryAddress
reserveStackAddress = do
  env <- get
  let address = stackPointer env
  setStackPointer $ MemoryAddress $ rawValue address + 1
  return address

createVar :: VariableNameLiteral -> State Env ()
createVar name = do
  env <- get
  address <- reserveStackAddress
  setVariables $ variables env ++ [Variable name $ stackPointer env]
  addCode ">"

sqrtAndRem :: Int -> (Int, Int)
sqrtAndRem n = intSqrt' n 1 where intSqrt' n a = if a*a > n then (a-1, n-((a-1)*(a-1))) else intSqrt' n $ a+1

moveCommand :: Int -> State Env ()
moveCommand n = do
  sp <- stackPointer <$> get
  setStackPointer $ MemoryAddress $ rawValue sp + n
  addCode $ if n > 0 then replicate n '>' else replicate (negate n) '<'

onRelativePosition :: Int -> State Env () -> State Env ()
onRelativePosition offset s = do
  moveCommand offset
  s
  moveCommand (negate offset)

onPosition :: MemoryAddress -> State Env () -> State Env ()
onPosition address s = do
  offset <- memoryOffset address
  onRelativePosition offset s

onGreaterThanZero :: State Env () -> State Env ()
onGreaterThanZero s =
  inBrackets $ do
    onRelativePosition 1
      s
    reset

incrementCommand :: Int -> State Env ()
incrementCommand n
  | n < 16 = incrementSimple n
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
readVariableCommand :: VariableNameLiteral -> State Env ()
readVariableCommand name = do
  address <- findAddress name
  offset <- memoryOffset address
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
setVariableCommand :: VariableNameLiteral -> State Env ()
setVariableCommand name = do
  exists <- variableExists name
  if exists then do
    address <- findAddress name
    offset <- memoryOffset address
    onRelativePosition (negate offset) reset
    inBrackets $ do
      decrementCommand 1
      onRelativePosition (negate offset) $ incrementCommand 1
  else
    createVar name

-- Sets current stack value to zero
reset :: State Env ()
reset = inBrackets $ decrementCommand 1

-- Clear the stack frame starting at the given address
clearStackFrame :: MemoryAddress -> State Env ()
clearStackFrame framePointer = do
  reset
  sp <- stackPointer <$> get
  when (rawValue sp > rawValue framePointer) $ do
    moveCommand $ negate 1
    clearStackFrame framePointer

-- Adds value into current stack value. Value can be int or variable
readValueCommand :: String -> State Env ()
readValueCommand value = case readMaybe value :: Maybe Int of
  Just value' -> incrementCommand value'
  Nothing -> readVariableCommand $ VariableNameLiteral value

-- Reads char into current stack value.
readCharCommand :: Char -> State Env ()
readCharCommand c = incrementCommand $ ord c

processExpression :: Expression -> State Env ()
processExpression (ByteLiteralExpression b) = incrementCommand b
processExpression (VariableExpression variableName) = readVariableCommand variableName
processExpression (BuiltinFunctionExpression "read" _) = readCommand
processExpression (BuiltinFunctionExpression "print" (FunctionParams [e])) = processExpression e >> printCommand
processExpression (BuiltinFunctionExpression "add" (FunctionParams [e1, e2])) = do
  processExpression e1
  onRelativePosition 1 $ do
    processExpression e2
    inBrackets $ do
      decrementCommand 1
      onRelativePosition (negate 1) $ incrementCommand 1
processExpression (BuiltinFunctionExpression "subtract" (FunctionParams [e1, e2])) = do
  processExpression e1
  onRelativePosition 1 $ do
    processExpression e2
    inBrackets $ do
      decrementCommand 1
      onRelativePosition (negate 1) $ decrementCommand 1
processExpression (BuiltinFunctionExpression "zero" (FunctionParams [e])) = do
  processExpression e
  onRelativePosition 1 $ incrementCommand 1
  inBrackets $ do
    onRelativePosition 1 $ decrementCommand 1
    reset
  onRelativePosition 1 $ inBrackets $ do
    decrementCommand 1
    onRelativePosition (negate 1) $ incrementCommand 1
processExpression (BuiltinFunctionExpression "not" (FunctionParams [e])) =
  processExpression (BuiltinFunctionExpression "zero" (FunctionParams [e]))
processExpression (BuiltinFunctionExpression "eq" (FunctionParams [e1, e2])) =
  processExpression (BuiltinFunctionExpression "zero" (FunctionParams [BuiltinFunctionExpression "subtract" (FunctionParams [e1, e2])]))

inNewStackFrame :: State Env () -> State Env ()
inNewStackFrame s = do
  framePointer <- stackPointer <$> get
  s
  clearStackFrame framePointer

convertStatement :: Statement -> State Env ()
convertStatement (ExpressionStatement expression) = do
  processExpression expression
  reset
convertStatement (AssignmentStatement variableName expression) = do
  processExpression expression
  setVariableCommand variableName
convertStatement (IfStatement condition body) = do
  processExpression condition
  onGreaterThanZero $ inNewStackFrame $ processBlock body
convertStatement (WhileStatement condition body) = do
  processExpression condition
  inBrackets $ do
    reset
    inNewStackFrame $ processBlock body
    processExpression condition

processBlock :: StatementBlock -> State Env ()
processBlock = foldl1 (>>) . map convertStatement . statements

main :: IO ()
main = do
  startTime <- getPOSIXTime
  text <- readFile "toUpper.bf"
  readFileTime <- getPOSIXTime
  putStrLn $ "Reading file took " ++ show (readFileTime - startTime) ++ "."

  program <- Parsing.parse text
  parseTime <- getPOSIXTime
  putStrLn $ "Parsing took " ++ show (parseTime - readFileTime) ++ "."

  let resultingCode = code $ execState (inNewStackFrame $ processBlock program) emptyEnv
  generateTime <- getPOSIXTime
  putStrLn $ "Generating code took " ++ show (generateTime - parseTime) ++ "."
  putStrLn "Result:"
  putStrLn resultingCode