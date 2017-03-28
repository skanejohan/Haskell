module Math.Diff.Differentiation where

import Math.ExpressionTypes

diff               :: String -> Expression -> Expression
diff v (Add f g)  = Add (diff v f) (diff v g)
diff v (Sub f g)  = Sub (diff v f) (diff v g)
diff v (Mul f g)  = Add (Mul (diff v f) g) (Mul f (diff v g))
diff v (Div f g)  = Div numerator denominator where
                        numerator = (Sub (Mul (diff v f) g) (Mul f (diff v g)))
                        denominator = Pow g (Num 2)
diff v (Pow f (Num n)) = Mul (Mul (Num n) (Pow f (Num (n-1)))) (diff v f)
diff v (Pow f g)  = undefined -- TODO
diff v (Cos f)    = Mul (Neg (diff v f)) (Sin f)
diff v (Sin f)    = Mul (diff v f) (Cos f)
diff v (Tan f)    = Div (diff v f) (Pow (Cos f) (Num 2))
diff v (Exp f)    = Mul (diff v f) (Exp f)
diff v (Ln f)     = Div (diff v f) f 
diff v (Log e)    = undefined
diff _ (Num _)   = Num 0
diff v (Var c)
  | v == c     = Num 1
  | otherwise  = Var c
diff v (Neg e)   = Neg (diff v e)
