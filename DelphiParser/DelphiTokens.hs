module DelphiTokens where

import DelphiLanguage
import qualified Text.ParserCombinators.Parsec.Token as TPPT

delphi_lexer :: TPPT.TokenParser ()
delphi_lexer =  TPPT.makeTokenParser delphiDef

whiteSpace= TPPT.whiteSpace delphi_lexer
lexeme    = TPPT.lexeme delphi_lexer
symbol    = TPPT.symbol delphi_lexer
natural   = TPPT.natural delphi_lexer
parens    = TPPT.parens delphi_lexer
semi      = TPPT.semi delphi_lexer
identifier= TPPT.identifier delphi_lexer
reserved  = TPPT.reserved delphi_lexer
reservedOp= TPPT.reservedOp delphi_lexer

