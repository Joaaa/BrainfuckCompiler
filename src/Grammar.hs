module Grammar where

 -- A block of statements
newtype StatementBlock = StatementBlock {statements :: [Statement]} deriving (Show)
emptyStatementBlock = StatementBlock []

  -- A statement, can contain other statements
data Statement =
  ExpressionStatement Expression |
  AssignmentStatement VariableNameLiteral Expression |
  IfStatement {condition :: Expression, body :: StatementBlock} |
  WhileStatement {condition :: Expression, body :: StatementBlock} deriving (Show)

  -- An expression, can be on right hand of assignment or on its own, can be nested
data Expression =
  ByteLiteralExpression Int |
  VariableExpression VariableNameLiteral |
  BuiltinFunctionExpression String FunctionParams deriving (Show)

newtype VariableNameLiteral = VariableNameLiteral String deriving (Eq, Show)

  -- Params to a function
newtype FunctionParams = FunctionParams [Expression] deriving (Show)
emptyFunctionParams = FunctionParams []
