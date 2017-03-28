module Math.ExpressionUtils (clean, repr, repr', mathjax) where

import Data.List as L
import Math.ExpressionTypes

-- Returns an expression tree where all "Sub a b" have been replaced with "Add a (Neg b)"
subToAdd :: Expression -> Expression
subToAdd (Add e1 e2) = Add (subToAdd e1) (subToAdd e2)
subToAdd (Sub e1 e2) = Add (subToAdd e1) (Neg $ subToAdd e2)
subToAdd (Mul e1 e2) = Mul (subToAdd e1) (subToAdd e2)
subToAdd (Div e1 e2) = Div (subToAdd e1) (subToAdd e2)
subToAdd (Pow e1 e2) = Pow (subToAdd e1) (subToAdd e2)
subToAdd (Cos e)     = Cos (subToAdd e)
subToAdd (Sin e)     = Sin (subToAdd e)
subToAdd (Tan e)     = Tan (subToAdd e)
subToAdd (Exp e)     = Exp (subToAdd e)
subToAdd (Ln e)      = Ln (subToAdd e)
subToAdd (Log e)     = Log (subToAdd e)
subToAdd (Neg e)     = Neg (subToAdd e)
subToAdd e           = e

-- Returns an expression tree where subsequent Add operations have been combined into term lists
termify e = if e' == termify' e' then e' else termify (termify' e')
            where e' = subToAdd e
                  termify' (Add e1 (Add e2 e3)) = Terms $ [termify' e1] ++ [termify' e2] ++ [termify' e3]
                  termify' (Add (Add e1 e2) e3) = Terms $ [termify' e1] ++ [termify' e2] ++ [termify' e3]
                  termify' (Mul e1 e2) = Mul (termify' e1) (termify' e2)
                  termify' (Div e1 e2) = Div (termify' e1) (termify' e2)
                  termify' (Pow e1 e2) = Pow (termify' e1) (termify' e2)
                  termify' (Cos e)     = Cos (termify' e)
                  termify' (Sin e)     = Sin (termify' e)
                  termify' (Tan e)     = Tan (termify' e)
                  termify' (Exp e)     = Exp (termify' e)
                  termify' (Ln e)      = Ln (termify' e)
                  termify' (Log e)     = Log (termify' e)
                  termify' (Neg e)     = Neg (termify' e)
                  termify' (Terms terms) = Terms $ termify'' terms where
                      termify'' ((Add e1 e2):xs) = [e1] ++ [e2] ++ xs
                      termify'' ((Terms es):xs)  = es ++ xs
                      termify'' (e:es)           = [e] ++ termify'' es
                      termify'' []               = []
                  termify' anythingElse = anythingElse

unNegTerms :: Expression -> Expression
unNegTerms (Terms ts) = Terms (unNegTerms' ts) where
    unNegTerms' ((Neg (Mul (Num n) e)):ts) = (Mul (Num (-n)) e):(unNegTerms' ts)
    unNegTerms' (t:ts) = t:(unNegTerms' ts)
    unNegTerms' []     = []
unNegTerms e = e

-- Returns an expression tree where term lists have been replaced with subsequent Add operations 
unTermify e = if e == unTermify' e then e else unTermify (unTermify' e)
            where unTermify' (Add e1 e2) = Add (unTermify' e1) (unTermify' e2)
                  unTermify' (Mul e1 e2) = Mul (unTermify' e1) (unTermify' e2)
                  unTermify' (Div e1 e2) = Div (unTermify' e1) (unTermify' e2)
                  unTermify' (Pow e1 e2) = Pow (unTermify' e1) (unTermify' e2)
                  unTermify' (Cos e)     = Cos (unTermify' e)
                  unTermify' (Sin e)     = Sin (unTermify' e)
                  unTermify' (Tan e)     = Tan (unTermify' e)
                  unTermify' (Exp e)     = Exp (unTermify' e)
                  unTermify' (Ln e)      = Ln (unTermify' e)
                  unTermify' (Log e)     = Log (unTermify' e)
                  unTermify' (Neg e)     = Neg (unTermify' e)
                  unTermify' (Terms ts)  = unTermify'' ts where
                      unTermify'' (t1:t2:[]) = Add t1 t2
                      unTermify'' (t1:ts)    = Add t1 (unTermify'' ts)
                      unTermify'' _          = Num 0 -- Should not happen
                  unTermify' anythingElse = anythingElse

-- Returns an expression tree where certain negations have been removed
unNeg :: Expression -> Expression
unNeg (Add e1 (Neg e2)) = Sub (unNeg e1) (unNeg e2)
unNeg (Add e1 e2)       = Add (unNeg e1) (unNeg e2)
unNeg (Sub e1 (Neg e2)) = Add (unNeg e1) (unNeg e2)
unNeg (Sub e1 e2)       = Sub (unNeg e1) (unNeg e2)
unNeg (Mul e1 e2)       = Mul (unNeg e1) (unNeg e2)
unNeg (Div e1 e2)       = Div (unNeg e1) (unNeg e2)
unNeg (Pow e1 e2)       = Pow (unNeg e1) (unNeg e2)
unNeg (Cos e)           = Cos (unNeg e)
unNeg (Sin e)           = Sin (unNeg e)
unNeg (Tan e)           = Tan (unNeg e)
unNeg (Exp e)           = Exp (unNeg e)
unNeg (Ln e)            = Ln (unNeg e)
unNeg (Log e)           = Log (unNeg e)
unNeg (Neg e)           = Neg (unNeg e)
unNeg e                 = e

-- Returns an expression tree where all term lists have been sorted.
sortTerms (Add e1 e2)   = Add (sortTerms e1) (sortTerms e2)
sortTerms (Sub e1 e2)   = Add (sortTerms e1) (sortTerms e2)
sortTerms (Mul e1 e2)   = Mul (sortTerms e1) (sortTerms e2)
sortTerms (Div e1 e2)   = Div (sortTerms e1) (sortTerms e2)
sortTerms (Pow e1 e2)   = Pow (sortTerms e1) (sortTerms e2)
sortTerms (Cos e)       = Cos (sortTerms e)
sortTerms (Sin e)       = Sin (sortTerms e)
sortTerms (Tan e)       = Tan (sortTerms e)
sortTerms (Exp e)       = Exp (sortTerms e)
sortTerms (Ln e)        = Ln (sortTerms e)
sortTerms (Log e)       = Log (sortTerms e)
sortTerms (Neg e)       = Neg (sortTerms e)
sortTerms (Terms terms) = Terms (reverse $ sortTermList terms) where
    sortTermList = L.sortBy orderTerms where
        orderTerms (Mul (Num _) (Pow (Var _) (Num e1))) (Mul (Num _) (Pow (Var _) (Num e2))) = compare e1 e2 -- a*x^e1 > b*x^e2 if e1 > e2
        orderTerms (Mul (Num _) (Pow (Var _) (Num e1))) (Pow (Var _) (Num e2))               = compare e1 e2 -- a*x^e1 > x^e2 if e1 > e2
        orderTerms (Mul (Num _) (Pow (Var _) (Num e1))) (Var _)                              = compare e1 1  -- a*x^e1 > x if e1 > 1
        orderTerms (Mul (Num _) (Pow (Var _) (Num e1))) _                                    = GT            -- a*x^e1 > anything else
        orderTerms (Pow (Var _) (Num e1)) (Mul (Num _) (Pow (Var _) (Num e2)))               = compare e1 e2 -- x^e1 > b*x^e2 if e1 > e2
        orderTerms (Pow (Var _) (Num e1)) (Pow (Var _) (Num e2))                             = compare e1 e2 -- x^e1 > x^e2 if e1 > e2
        orderTerms (Pow (Var _) (Num e1)) (Var _)                                            = compare e1 1  -- x^e1 > x if e1 > 1
        orderTerms (Pow (Var _) (Num e1)) _                                                  = GT            -- x^e1 > anything else
        orderTerms (Mul (Num _) (Var _)) (Mul (Num _) (Pow (Var _) (Num e2)))                = compare 1 e2  -- a*x > b*x^e2 if e2 < 1
        orderTerms (Mul (Num _) (Var _)) (Pow (Var _) (Num e2))                              = compare 1 e2  -- a*x > x^e2 if e2 < 1
        orderTerms (Mul (Num _) (Var _)) (Var _)                                             = EQ            -- a*x = x
        orderTerms (Mul (Num _) (Var _)) _                                                   = GT            -- a*x > anything else
        orderTerms (Var _) (Mul (Num _) (Pow (Var _) (Num e2)))                              = compare 1 e2  -- x > b*x^e2 if e2 < 1
        orderTerms (Var _) (Pow (Var _) (Num e2))                                            = compare 1 e2  -- x > x^e2 if e2 < 1
        orderTerms (Var _) _                                                                 = GT            -- x > anything else
        orderTerms (Num _) (Mul (Num _) (Pow (Var _) (Num e2)))                              = LT            -- a < b*x^e2
        orderTerms (Num _) (Pow (Var _) (Num e2))                                            = LT            -- a < x^e2
        orderTerms (Num _) (Mul (Num _) (Var _))                                             = LT            -- a < b*x
        orderTerms (Num _) (Var _)                                                           = LT            -- a < x
        orderTerms (Num n1) (Num n2)                                                         = compare n1 n2 -- a > b if numerically greater
        orderTerms (Num _) _                                                                 = GT
        orderTerms _ (Num _)                                                                 = LT
        orderTerms _ _                                                                       = EQ
sortTerms e             = e

-- Returns an expression tree where certain simplifications have been made to the term lists
simplifyTermList (Add e1 e2)   = Add (simplifyTermList e1) (simplifyTermList e2)
simplifyTermList (Sub e1 e2)   = Add (simplifyTermList e1) (simplifyTermList e2)
simplifyTermList (Mul e1 e2)   = Mul (simplifyTermList e1) (simplifyTermList e2)
simplifyTermList (Div e1 e2)   = Div (simplifyTermList e1) (simplifyTermList e2)
simplifyTermList (Pow e1 e2)   = Pow (simplifyTermList e1) (simplifyTermList e2)
simplifyTermList (Cos e)       = Cos (simplifyTermList e)
simplifyTermList (Sin e)       = Sin (simplifyTermList e)
simplifyTermList (Tan e)       = Tan (simplifyTermList e)
simplifyTermList (Exp e)       = Exp (simplifyTermList e)
simplifyTermList (Ln e)        = Ln (simplifyTermList e)
simplifyTermList (Log e)       = Log (simplifyTermList e)
simplifyTermList (Neg e)       = Neg (simplifyTermList e)
simplifyTermList (Terms terms) = Terms (simplifyTermList' terms) where
    simplifyTermList' (Num n1:Num n2:ts) = Num (n1+n2):simplifyTermList' ts
    simplifyTermList' ((Mul (Num n1) (Pow (Var v1) (Num e1))):(Mul (Num n2) (Pow (Var v2) (Num e2))):ts)
                      | v1 == v2 && e1 == e2 = (Mul (Num (n1+n2)) (Pow (Var v1) (Num e1))):simplifyTermList' ts
    simplifyTermList' (t:ts)             = t:simplifyTermList' ts
    simplifyTermList' []                 = []
simplifyTermList e             = e

-- Returns an expression tree where certain simplifications have been made
simplify :: Expression -> Expression
simplify e = if e == simplify' e then e else simplify (simplify' e)
             where 
                -- Addition ----------------------------------------------------------------------------

                -- 0 + expression = expression
                simplify' (Add (Num 0) e2) = simplify' e2

                -- expression + 0 = expression
                simplify' (Add e1 (Num 0)) = simplify' e1

                -- n1 + n2 = their sum
                simplify' (Add (Num n1) (Num n2)) = Num (n1+n2)
                
                -- x + x = 2 * x
                simplify' (Add (Var v1) (Var v2)) | v1 == v2 = Mul (Num 2) (Var v1)

                -- x^3 + x^3 = 2 * x^3
                simplify' (Add (Pow (Var v1) (Num e1)) (Pow (Var v2) (Num e2))) | v1 == v2 && e1 == e2 = Mul (Num 2) (Pow (Var v1) (Num e1))

                -- 7*x^3 + x^3 = 8 * x^3
                simplify' (Add (Mul (Num n1) (Pow (Var v1) (Num e1))) (Pow (Var v2) (Num e2))) | v1 == v2 && e1 == e2 = Mul (Num $ n1+1) (Pow (Var v1) (Num e1))
                
                -- 4 * x^2 + 3 * x^2 = 7 * x^2
                simplify' (Add (Mul (Num n1) (Pow (Var v1) (Num e1))) (Mul (Num n2) (Pow (Var v2) (Num e2)))) | v1 == v2 && e1 == e2 = Mul (Num $ n1+n2) (Pow (Var v1) (Num e1))

                -- x + 3 * x = 4 * x
                simplify' (Add (Var v1) (Mul (Num n) (Var v2))) | v1 == v2 = Mul (Num (n+1)) (Var v2)

                -- 3 * x + x = 4 * x
                simplify' (Add (Mul (Num n) (Var v1)) (Var v2)) | v1 == v2 = Mul (Num (n+1)) (Var v2)
                
                -- 5 * x + 3 * x = 8 *x
                simplify' (Add (Mul (Num n1) (Var v1)) (Mul (Num n2) (Var v2))) | v1 == v2 = Mul (Num (n1+n2)) (Var v2)

                -- polynomials: 3 + x -> x + 3
                simplify' (Add (Num n) (Var v)) = Add (Var v) (Num n)
                
                -- polynomials: 3 + 2 * x -> 2 * x + 3
                simplify' (Add (Num n1) (Mul (Num n2) (Var v))) = Add (Mul (Num n2) (Var v)) (Num n1)
                
                -- polynomials: 3 + x^2 -> x^2 + 3
                simplify' (Add (Num n1) (Pow (Var v) (Num n2))) = Add (Pow (Var v) (Num n2)) (Num n1)

                -- polynomials: 3 + 2 * x^3 -> 2 * x^3 + 3
                simplify' (Add (Num n1) (Mul (Num n2) (Pow (Var v) (Num n3)))) = Add (Mul (Num n2) (Pow (Var v) (Num n3))) (Num n1)

                -- polynomials: x + x^2 -> x^2 + x
                simplify' (Add (Var v1) (Pow (Var v2) (Num n2))) | v1 == v2 = Add (Pow (Var v2) (Num n2)) (Var v1)

                -- polynomials: x + 2 * x^3 -> 2 * x^3 + x
                simplify' (Add (Var v1) (Mul (Num n1) (Pow (Var v2) (Num n2)))) 
                    | v1 == v2 = Add (Mul (Num n1) (Pow (Var v2) (Num n2))) (Var v1)

                -- polynomials: 4 * x + x^2 -> x^2 + 4 * x
                simplify' (Add (Mul (Num n1) (Var v1)) (Pow (Var v2) (Num n3)))
                    | v1 == v2 = Add (Pow (Var v2) (Num n3)) (Mul (Num n1) (Var v1))

                -- polynomials: 4 * x + 3 * x^2 -> 3 * x^2 + 4 * x
                simplify' (Add (Mul (Num n1) (Var v1)) (Mul (Num n2) (Pow (Var v2) (Num n3)))) 
                    | v1 == v2 = Add (Mul (Num n2) (Pow (Var v2) (Num n3))) (Mul (Num n1) (Var v1))

                -- polynomials: x^2 + x^3 -> x^3 + x^2
                simplify' (Add (Pow (Var v1) (Num n1)) (Pow (Var v2) (Num n2)))
                    | v1 == v2 && n1 < n2 = Add (Pow (Var v2) (Num n2)) (Pow (Var v1) (Num n1))

                -- polynomials: x^2 + 2 * x^3 -> 2 * x^3 + x^2
                simplify' (Add (Pow (Var v1) (Num n1)) (Mul (Num n2) (Pow (Var v2) (Num n3))))
                    | v1 == v2 && n1 < n3 = Add (Mul (Num n2) (Pow (Var v2) (Num n3))) (Pow (Var v1) (Num n1))

                -- polynomials: 2 * x^2 + x^3 -> x^3 + 2 * x^2
                simplify' (Add (Mul (Num n1) (Pow (Var v1) (Num e1))) (Pow (Var v2) (Num e2)))
                    | v1 == v2 && e1 < e2 = Add (Pow (Var v2) (Num e2)) (Mul (Num n1) (Pow (Var v1) (Num e1)))

                -- polynomials: 2 * x^2 + 2 * x^3 -> 2 * x^3 + 2 * x^2
                simplify' (Add (Mul (Num n1) (Pow (Var v1) (Num e1))) (Mul (Num n2) (Pow (Var v2) (Num e2))))
                    | v1 == v2 && e1 < e2 = Add (Mul (Num n2) (Pow (Var v2) (Num e2))) (Mul (Num n1) (Pow (Var v1) (Num e1)))

                -- for all other additions, just simplify the sub-expressions
                simplify' (Add e1 e2) = Add (simplify' e1) (simplify' e2)
        
                simplify' (Sub (Num 0) e2)        = Neg (simplify' e2)
                simplify' (Sub e1 (Num 0))        = simplify' e1
                simplify' (Sub (Num n1) (Num n2)) = Num (n1-n2)
                
                -- x - x = 0
                simplify' (Sub (Var v1) (Var v2)) 
                  | v1 == v2 = Num 0
                
                simplify' (Sub e1 e2)             = Sub (simplify' e1) (simplify' e2)
                
                simplify' (Mul e1 (Num 0))        = Num 0
                simplify' (Mul (Num 0) e2)        = Num 0
                simplify' (Mul e1 (Num 1))        = simplify' e1
                simplify' (Mul (Num 1) e2)        = simplify' e2
                simplify' (Mul (Num n1) (Num n2)) = Num (n1*n2)
                
                simplify' (Mul (Num n1) (Mul (Num n2) e2)) = Mul (Num (n1*n2)) e2
                simplify' (Mul (Mul (Num n1) e1) (Num n2)) = Mul (Num (n1*n2)) e1
                simplify' (Mul (Neg (Num 1)) e2)  = Neg (simplify' e2)
                simplify' (Mul e1 (Neg (Num 1)))  = Neg (simplify' e1)

                -- x * x = x^2    
                simplify' (Mul (Var v1) (Var v2)) 
                    | v1 == v2                    = Pow (Var v1) (Num 2)

                -- 5 * x * x = 5 * x^2    
                simplify' (Mul (Mul (Num n1) (Var v1)) (Var v2))
                    | v1 == v2                    = Mul (Num n1) (Pow (Var v1) (Num 2))
                
                -- x * x^3 = x^4    
                simplify' (Mul (Var v1) (Pow (Var v2) (Num n)))
                    | v1 == v2                    = Pow (Var v1) (Num $ n+1)

                -- x^5 * x = x^6    
                simplify' (Mul (Pow (Var v1) (Num n)) (Var v2))
                    | v1 == v2                    = Pow (Var v1) (Num $ n+1)

                -- x^5 * x^2 = x^7    
                simplify' (Mul (Pow (Var v1) (Num n1)) (Pow (Var v2) (Num n2)))
                    | v1 == v2                    = Pow (Var v1) (Num $ n1+n2)

                simplify' (Mul e1 e2)             = Mul (simplify' e1) (simplify' e2)

                simplify' (Div e1 e2)      = Div (simplify' e1) (simplify' e2)
                
                

                simplify' (Pow e1 (Num 1)) = simplify' e1
                simplify' (Pow e1 e2)      = Pow (simplify' e1) (simplify' e2)
                
                simplify' (Cos e)          = Cos (simplify' e)
                simplify' (Sin e)          = Sin (simplify' e)
                simplify' (Tan e)          = Tan (simplify' e)
                simplify' (Exp e)          = Exp (simplify' e)
                simplify' (Ln e)           = Ln (simplify' e)
                simplify' (Log e)          = Log (simplify' e)
                simplify' (Neg (Neg e))    = simplify' e
                simplify' (Neg e)          = Neg (simplify' e)
                simplify' e                = e

-- clean returns an expression tree that has been maximally simplified
clean = simplify . unNeg . unTermify . simplifyTermList . sortTerms . unNegTerms . termify . simplify

-- repr returns a "readable" string representing the expression
repr :: Expression -> String
repr (Add e1 e2) = repr e1 ++ "+" ++ repr e2
repr (Sub e1 e2) = repr e1 ++ "-" ++ repr e2
repr (Mul e1 e2) = repr e1 ++ "*" ++ repr e2
repr (Div e1 e2) = repr e1 ++ "/" ++ repr e2
repr (Pow e1 e2) = repr e1 ++ "^" ++ repr e2
repr (Cos e)     = "cos(" ++ repr e ++ ")"
repr (Sin e)     = "sin(" ++ repr e ++ ")"
repr (Tan e)     = "tan(" ++ repr e ++ ")"
repr (Exp e)     = "exp(" ++ repr e ++ ")"
repr (Ln e)      = "ln(" ++ repr e ++ ")"
repr (Log e)     = "log(" ++ repr e ++ ")"
repr (Num i)     = show i
repr (Var v)     = v
repr (Neg e)     = "-" ++ repr e
repr (Terms ts)  = "[" ++ concat (map repr ts) ++ "]"

-- repr' returns a string representing the expression tree
repr' ind (Add e1 e2) = indent ind ++ "ADD\n" ++ repr' (ind + 2) e1 ++ repr' (ind + 2) e2
repr' ind (Sub e1 e2) = indent ind ++ "SUB\n" ++ repr' (ind + 2) e1 ++ repr' (ind + 2) e2
repr' ind (Mul e1 e2) = indent ind ++ "MUL\n" ++ repr' (ind + 2) e1 ++ repr' (ind + 2) e2
repr' ind (Div e1 e2) = indent ind ++ "DIV\n" ++ repr' (ind + 2) e1 ++ repr' (ind + 2) e2
repr' ind (Pow e1 e2) = indent ind ++ "POW\n" ++ repr' (ind + 2) e1 ++ repr' (ind + 2) e2
repr' ind (Cos e)     = indent ind ++ "COS\n" ++ repr' (ind + 2) e
repr' ind (Sin e)     = indent ind ++ "SIN\n" ++ repr' (ind + 2) e
repr' ind (Tan e)     = indent ind ++ "TAN\n" ++ repr' (ind + 2) e
repr' ind (Exp e)     = indent ind ++ "EXP\n" ++ repr' (ind + 2) e
repr' ind (Ln e)      = indent ind ++ "LN\n"  ++ repr' (ind + 2) e
repr' ind (Log e)     = indent ind ++ "LOG\n" ++ repr' (ind + 2) e
repr' ind (Num n)     = indent ind ++ "NUM: " ++ show n ++ "\n"
repr' ind (Var v)     = indent ind ++ "VAR: " ++ v ++ "\n"
repr' ind (Neg e)     = indent ind ++ "NEG\n" ++ repr' (ind + 2) e
repr' ind (Terms ts)  = indent ind ++ "TERMS\n" ++ concat (map (repr' (ind + 2)) ts)
indent ind = map (\_ -> ' ') [1..ind]

-- mathjax returns a string representing the expression formatted for use with MathJax
mathjax (Add e1 e2) = mathjax e1 ++ "+" ++ mathjax e2
mathjax (Sub e1 e2) = mathjax e1 ++ "-" ++ mathjax e2
mathjax (Mul e1 e2) = mathjax e1 ++ "*" ++ mathjax e2
mathjax (Div e1 e2) = "\\frac{" ++ mathjax e1 ++ "}{" ++ mathjax e2 ++ "}" 
mathjax (Pow e1 e2) = mathjax e1 ++ "^" ++ mathjax e2
mathjax (Cos e)     = "cos(" ++ mathjax e ++ ")"
mathjax (Sin e)     = "sin(" ++ mathjax e ++ ")"
mathjax (Tan e)     = "tan(" ++ mathjax e ++ ")"
mathjax (Exp e)     = "exp(" ++ mathjax e ++ ")"
mathjax (Ln e)      = "ln(" ++ mathjax e ++ ")"
mathjax (Log e)     = "log(" ++ mathjax e ++ ")"
mathjax (Num i)     = show i
mathjax (Var v)     = v
mathjax (Neg e)     = "-" ++ mathjax e

