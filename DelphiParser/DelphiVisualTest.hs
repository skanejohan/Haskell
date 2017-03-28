module DelphiVisualTest where

import DelphiParser
import Text.ParserCombinators.Parsec

run :: Show a => Parser a -> String -> IO ()
run p input
	= case (parse p "" input) of
		Left err -> do { putStr "parse error at "
			       ; print err
			       }
		Right x -> print x

p1 = run string_parser "'Hello, world!'"
fp = run factor_parser "'Hello, world!'"
ep = run expression_parser "'Hello, world!'"
elp = run expr_list_parser "'Hello, world!'"
ip1 = run ident_parser "'Hello, world!'"
ip2 = run ident_parser "_apa"
ssp = run simple_stmt_parser "WriteLn('Hello, world!')"
sp = run statement_parser "WriteLn('Hello, world!')"
slp = run stmt_list_parser "WriteLn('Hello, world!');"
csp = run compound_stmt_parser "begin WriteLn('Hello, world!'); end"
bp = run block_parser "begin WriteLn('Hello, world!'); end"
up = run uses_parser "uses SysUtils;"
pbp = run program_block_parser "uses SysUtils; begin WriteLn('Hello, world!'); end"
pp = run program_parser "program dp; uses SysUtils; begin WriteLn('Hello, world!'); end."
pp2 = run program_parser "program dp2; uses SysUtils; var S: String; begin S := 'Hello, world!'; WriteLn('Hello, world!'); end."

