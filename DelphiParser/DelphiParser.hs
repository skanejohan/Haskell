module DelphiParser where

import Data.Maybe
import DelphiTokens
import DelphiGrammar

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as Token_parser

-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

comma_sep_parser = skipMany1 (space <|> char ',')
semicolon_sep_parser = skipMany1 (space <|> char ';')

-----------------------------------------------------------
-- Delphi-specific
-----------------------------------------------------------
	   
program_parser :: Parser ( Maybe ( DelphiProgram, DelphiUses, [DelphiDecl], [DelphiStatement] ) )
program_parser = do { reserved "program"
                    ; p <- identifier
                    ; symbol ";"
                    ; b <- program_block_parser
                    ; symbol "."
                    ; case b of Just (u, d, s) -> return $ Just (Program p, u, d, s) 
                    }

program_block_parser :: Parser ( Maybe ( DelphiUses, [DelphiDecl], [DelphiStatement] ) )
program_block_parser = do { u <- uses_parser
                          ; b <- block_parser
                          ; case u of Just u2 -> case b of Just (d, s) -> return $ Just (u2, d, s)
                          }

uses_parser :: Parser ( Maybe DelphiUses )
uses_parser = do { reserved "uses"
                 ; names <- sepBy1 identifier comma_sep_parser 
                 ; symbol ";"
                 ; return $ Just $ Uses names
                 }

block_parser :: Parser ( Maybe ( [DelphiDecl], [DelphiStatement] ) )
block_parser = do { d <- many decl_section_parser
                  ; s <- compound_stmt_parser
                  ; case s of Just s2 -> return $ Just (catMaybes d, s2)
                  }

decl_section_parser :: Parser ( Maybe DelphiDecl )
decl_section_parser = do { v <- var_section_parser
                         ; return v
                         }

var_section_parser :: Parser ( Maybe DelphiDecl )
var_section_parser = do { reserved "var"
                        ; decl_list <- sepBy1 identifier comma_sep_parser 
                        ; symbol ":"
                        ; id <- identifier
                        ; symbol ";"
                        ; return $ Just (Var decl_list id)
                        }

compound_stmt_parser :: Parser ( Maybe [DelphiStatement] )
compound_stmt_parser = do { reserved "begin"
                          ; sl <- stmt_list_parser
                          ; reserved "end"
                          ; return sl
                          }
 
stmt_list_parser :: Parser ( Maybe [DelphiStatement] )
stmt_list_parser = do { s <- sepEndBy statement_parser semicolon_sep_parser
                      ; return $ Just (catMaybes s)
                      }

statement_parser :: Parser ( Maybe DelphiStatement )
statement_parser = do { s <- simple_stmt_parser
                      ; return s
                      }

simple_stmt_parser :: Parser (Maybe DelphiStatement)
simple_stmt_parser =   try(function_call_parser) 
                   <|> assignment_parser 

function_call_parser :: Parser (Maybe DelphiStatement)
function_call_parser = do { d <- designator_parser
                          ; symbol "("
                          ; e <- expr_list_parser
                          ; symbol ")"
                          ; case d of Just (Id s) -> case e of Just e2 -> return $ Just (FunctionCall "" s e2)
                          }

assignment_parser :: Parser (Maybe DelphiStatement)
assignment_parser = do { d <- designator_parser
                       ; reserved ":="
                       ; e <- expression_parser
                       ; case d of Just (Id s) -> case e of Just e2 -> return $ Just (Assign s e2)
                       }

designator_parser :: Parser ( Maybe DelphiIdent ) -- incorrect type DelphiIdent!
designator_parser = do { i <- ident_parser
                       ; return i
                       }

expr_list_parser :: Parser ( Maybe [DelphiExpr] )
expr_list_parser = do { e <- expression_parser
                      ; case e of
                          Just expr -> return $ Just [expr]
                      }

expression_parser :: Parser ( Maybe DelphiExpr )
expression_parser = do { e <- simple_expression_parser
                       ; return e
                       }

simple_expression_parser :: Parser ( Maybe DelphiExpr )
simple_expression_parser = do { t <- term_parser
                              ; return t
                              }

term_parser :: Parser ( Maybe DelphiExpr )
term_parser = do { f <- factor_parser
                 ; return f
                 }

factor_parser :: Parser ( Maybe DelphiExpr )
factor_parser = do { s <- string_parser
                   ; return s
                   }

string_parser :: Parser ( Maybe DelphiExpr )
string_parser = do { symbol "'"
	               ; s <- manyTill anyChar (symbol "'")
				   ; return $ Just $ Str s 
				   }
				   
ident_parser :: Parser (Maybe DelphiIdent)
ident_parser = do { i <- identifier
    	          ; return $ Just (Id i)
                  }
				
