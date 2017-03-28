module Math.ExpressionTypes where

data Expression = Add Expression Expression
                | Sub Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | Pow Expression Expression
                | Cos Expression
                | Sin Expression
                | Tan Expression
                | Exp Expression
                | Ln Expression 
                | Log Expression
                | Neg Expression
                | Num Integer
                | Var String
                | Terms [Expression]
                deriving (Show, Eq)
