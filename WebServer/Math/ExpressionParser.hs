module Math.ExpressionParser (parseExpression) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Math.ExpressionTypes

TokenParser{ parens = m_parens
           , integer = m_integer
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , whiteSpace = m_whiteSpace } = makeTokenParser emptyDef

expressionParser :: Parser Expression
expressionParser = m_whiteSpace >> buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "-" >> return Neg)]
        , [Prefix (m_reserved "log" >> return Log)]
        , [Prefix (m_reserved "ln" >> return Ln)]
        , [Prefix (m_reserved "exp" >> return Exp)]
        , [Prefix (m_reserved "tan" >> return Tan)]
        , [Prefix (m_reserved "sin" >> return Sin)]
        , [Prefix (m_reserved "cos" >> return Cos)]
        , [Infix (m_reservedOp "^" >> return Pow) AssocLeft]
        , [Infix (m_reservedOp "*" >> return Mul) AssocLeft]
        , [Infix (m_reservedOp "/" >> return Div) AssocLeft]
        , [Infix (m_reservedOp "-" >> return Sub) AssocLeft]
        , [Infix (m_reservedOp "+" >> return Add) AssocLeft]
        ]

term = m_parens expressionParser
       <|> fmap Var m_identifier
       <|> fmap Num m_integer

parseExpression = parse expressionParser ""       
