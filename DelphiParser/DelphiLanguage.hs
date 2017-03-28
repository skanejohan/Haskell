module DelphiLanguage where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

delphiDef :: LanguageDef st
delphiDef =  LanguageDef 
    {
       commentStart   = "{"
     , commentEnd     = "}"
     , commentLine    = "//"
     , nestedComments = True
     , identStart     = letter <|> char '_'
     , identLetter    = alphaNum <|> digit <|> char '_'
     , opStart        = opLetter delphiDef
     , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
     , reservedOpNames= []
     , reservedNames  = ["begin", "end"]
     , caseSensitive  = True
    }
