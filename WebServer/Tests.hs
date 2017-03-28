module Tests where

import Math.ExpressionTypes
import Math.ExpressionParser
import Math.ExpressionUtils
import Math.Diff.Differentiation

----------------------------------------------------------
-- Verify that running the parser on specific input yields
-- the expected output.
----------------------------------------------------------

parseAndSimplify input = case parseExpression input of
    Right result -> repr (clean result)
    Left err     -> "error"

parserTests = map assertParser tests
    where 
        assertParser (input,output) = (parseAndSimplify input == output,input)
        tests = [("x","x"), 
                 ("y","y"),
                 ("-1","-1"),
                 ("sin(5*x)","sin(5*x)"),
                 (" sin ( 5 * x ) ","sin(5*x)"),
                 ("sin(4*x+3)","sin(4*x+3)"),
                 ("x^2","x^2"),

                 -- Addition
                 ("3+3","6"),
                 ("x+x","2*x"),
                 ("x+x+x","3*x"),   
                 ("2*x+x","3*x"),   
                 ("x+2*x","3*x"),   
                 ("2*x+2*x","4*x"),   

                 -- Multiplication
                 ("3*4","12"),   
                 ("3*4*5","60"), 

                 -- Addition and multiplication combined
                 ("3*4+5","17"),   

                 -- Polynomials   
                 ("4*x^2","4*x^2"),
                 ("x+3","x+3"),
                 ("3+x","x+3"),
                 ("2*x+3","2*x+3"),
                 ("3+2*x","2*x+3"),
                 ("x^2+3","x^2+3"),
                 ("3+x^2","x^2+3"),
                 ("2*x^3+3","2*x^3+3"),
                 ("3+2*x^3","2*x^3+3"),
                 ("x^2+x","x^2+x"),
                 ("x+x^2","x^2+x"),
                 ("2*x^3+x","2*x^3+x"),
                 ("x+2*x^3","2*x^3+x"),
                 ("x^2+4*x","x^2+4*x"),
                 ("4*x+x^2","x^2+4*x"),
                 ("2*x^3+3*x","2*x^3+3*x"),
                 ("3*x+2*x^3","2*x^3+3*x"),
                 ("x^3+x^2","x^3+x^2"),
                 ("x^2+x^3","x^3+x^2"),
                 ("2*x^3+x^2","2*x^3+x^2"),
                 ("x^2+2*x^3","2*x^3+x^2"),
                 ("x^3+2*x^2","x^3+2*x^2"),
                 ("2*x^2+x^3","x^3+2*x^2"),
                 ("2*x^3+2*x^2","2*x^3+2*x^2"),
                 ("2*x^2+2*x^3","2*x^3+2*x^2"),
                 ("4*x^2+9*x+3","4*x^2+9*x+3"),
                 ("4*x^2+3+9*x","4*x^2+9*x+3"),
                 ("3+9*x+4*x^2","4*x^2+9*x+3"),
                 ("3+4*x^2+9*x","4*x^2+9*x+3"),
                 ("9*x+4*x^2+3","4*x^2+9*x+3"),
                 ("9*x+3+4*x^2","4*x^2+9*x+3"),
                 ("4*x^2+x^2","5*x^2"),
                 ("4*x^2+3*x^2","7*x^2"),
                 ("x*x*x+x*x*x","2*x^3"),

                 ("x-3","x-3"),
                 --("3-x","-x+3"),
                 ("x-x","0"),
                 ("3-3","0"),
                 ("2*x-3","2*x-3"),
                 --("3-2*x","-2*x+3"),

                 --("x^2+3","x^2+3"),
                 --("3+x^2","x^2+3"),
                 --("2*x^3+3","2*x^3+3"),
                 --("3+2*x^3","2*x^3+3"),
                 --("x^2+x","x^2+x"),
                 --("x+x^2","x^2+x"),
                 --("2*x^3+x","2*x^3+x"),
                 --("x+2*x^3","2*x^3+x"),
                 --("x^2+4*x","x^2+4*x"),
                 --("4*x+x^2","x^2+4*x"),
                 --("2*x^3+3*x","2*x^3+3*x"),
                 --("3*x+2*x^3","2*x^3+3*x"),
                 --("x^3+x^2","x^3+x^2"),
                 --("x^2+x^3","x^3+x^2"),
                 --("2*x^3+x^2","2*x^3+x^2"),
                 --("x^2+2*x^3","2*x^3+x^2"),
                 --("x^3+2*x^2","x^3+2*x^2"),
                 --("2*x^2+x^3","x^3+2*x^2"),
                 --("2*x^3+2*x^2","2*x^3+2*x^2"),
                 --("2*x^2+2*x^3","2*x^3+2*x^2"),
                 --("4*x^2+9*x+3","4*x^2+9*x+3"),
                 --("4*x^2+3+9*x","4*x^2+9*x+3"),
                 --("3+9*x+4*x^2","4*x^2+9*x+3"),
                 --("3+4*x^2+9*x","4*x^2+9*x+3"),
                 --("9*x+4*x^2+3","4*x^2+9*x+3"),
                 --("9*x+3+4*x^2","4*x^2+9*x+3"),
                 --("4*x^2+x^2","5*x^2"),
                 --("4*x^2+3*x^2","7*x^2"),
                 --("x*x*x+x*x*x","2*x^3"),

                 (" sin ( 4 * x ^ 2 + 5 * x ) + cos ( tan ( 3 * x ) ) ", "sin(4*x^2+5*x)+cos(tan(3*x))"),  
                 ("-1*x","-x"),
                 ("x+x","2*x"),
                 ("","error")]

----------------------------------------------------------
-- Verify that running the parser on specific input and
-- differenatiating the resulting expression yields the
-- expected output.
----------------------------------------------------------

parseDiffAndSimplifyExpression var input = case parseExpression input of
    Right result -> repr (clean (diff var (clean result)))
    Left err     -> show err

diffTests = map assertDiff tests
    where
        assertDiff (var,input,output) = (parseDiffAndSimplifyExpression var input == output,input)
        tests = [
                 -- Polynomials
                 ("x","5","0"), 
                 ("x","x","1"),
                 ("y","x","x"),
                 ("x","-x","-1"),
                 ("x","x^2","2*x"),
                 ("x","x*x","2*x"),
                 ("x","x*x*x","3*x^2"),
                 ("x","x*x*x+x*x*x","6*x^2"),
                 ("x","2*x+3*x^2","6*x+2"),
                 ("x","5*x^4+3*x^3+6*x^2+15*x+3","20*x^3+9*x^2+12*x+15"),

                 -- Trigonometric functions
                 ("x","sin(x)","cos(x)"),
                 ("x","cos(x)","-sin(x)"),
                 ("x","tan(x)","1/cos(x)^2"),
                 ("x","4*x^3+sin(2*x)+3","12*x^2+2*cos(2*x)"),
                 ("x","cos(cos(x))","sin(x)*sin(cos(x))")
                ]

--------------------------------------------------------------------------
-- allTests runs all tests, returning a list of (Boolean,String) where the
-- first element indicates the result of each individual test and the second
-- element is the expression under test.
--
-- failedTests returns a list of the failed tests only.
--
-- assertAll returns True or False depending on whether all tests pass.
---------------------------------------------------------------------------

allTests = concat [parserTests, diffTests]

failedTests = filter (\x -> not $ fst x) 

assertAll = (failedTests allTests) == []

--------------------------------------------------------------------------
-- display is useful in ghci, to print the tree representation 
-- of a parsed expression. displayClean prints the tree representation 
-- of a parsed expression that has been "cleaned up"
--------------------------------------------------------------------------

display s = case parseExpression s of
    Right r -> putStr $ repr' 0 r
    Left  _ -> putStr "error"

displayClean s = case parseExpression s of
    Right r -> putStr $ repr' 0 (clean r)
    Left  _ -> putStr "error"    