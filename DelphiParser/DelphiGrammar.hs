module DelphiGrammar where

data DelphiProgram
 = Program String
 deriving (Show, Eq)

data DelphiUses
 = Uses [String]
 deriving (Show, Eq)

data DelphiDecl
 = Var [String] String -- Identifier list, Type
 deriving (Show, Eq)

data DelphiIdent
 = Id String
 deriving (Show, Eq)

data DelphiExpr 
 = Str String
 | Int String
 deriving (Show, Eq)

data DelphiStatement 
 = FunctionCall String String [ DelphiExpr ]  -- Object, Method, Parameters
 | Assign String DelphiExpr                   -- Identifier Value
 deriving (Show, Eq)
