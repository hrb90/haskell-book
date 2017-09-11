data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add x1 x2) = (eval x1) + (eval x2)

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add x1 x2) = "(" ++ (printExpr x1) ++ " + " ++ (printExpr x2) ++ ")"
