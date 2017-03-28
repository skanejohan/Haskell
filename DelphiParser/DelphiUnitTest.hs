module Test where

import Test.HUnit
import DelphiParser
import DelphiGrammar
import Text.ParserCombinators.Parsec

run_test :: Parser a -> String -> Maybe a
run_test p input = 
  case (parse p "" input) of
    Left err -> Nothing
    Right x -> Just x


specificTestCase string expected parser = TestCase $ assertEqual string expected ( run_test parser string )

-- Program

programTestCase string expected = specificTestCase string expected program_parser

program_test1 = programTestCase "program dp; uses SysUtils; begin WriteLn('Hello, world!'); end." ( Just (Just (Program "dp",Uses ["SysUtils"],[],[FunctionCall "" "WriteLn" [Str "Hello, world!"]])))
program_test2 = programTestCase "program dp; uses SysUtils; begin S := 'Hello'; WriteLn('Hello, world!'); end." ( Just (Just (Program "dp",Uses ["SysUtils"],[],[Assign "S" (Str "Hello"), FunctionCall "" "WriteLn" [Str "Hello, world!"]])))
program_tests = [program_test1, program_test2]

-- Run tests

all_tests = program_tests ++ []

run_all = runTestTT $ TestList all_tests
