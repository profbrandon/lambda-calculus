
module Lambda
  (
    Term(..)
  , showTerm
  , showDeBruijn
  , smashLambda
  )
where

data Term =
    Var    Int
  | App    Term Term 
  | Lambda [String] Term
  deriving Eq



instance Show Term where
  show = showTerm []



showTerm :: [String] -> Term -> String
showTerm vs (Var    i)       = case drop i vs of [] -> '@' : show i ; vs' -> head vs'
showTerm vs (App    e1 e2)   = wrapLambda vs e1 ++ " " ++ wrapBoth vs e2
showTerm vs (Lambda us e) = "\\" ++ showVars us ++ ". " ++ showTerm (reverse us ++ vs) e

wrapLambda :: [String] -> Term -> String
wrapLambda vs e@(Lambda _ _ ) = "(" ++ showTerm vs e ++ ")"
wrapLambda vs e               = showTerm vs e

wrapBoth :: [String] -> Term -> String
wrapBoth vs e@(App _ _) = "(" ++ showTerm vs e ++ ")"
wrapBoth vs e           = wrapLambda vs e

showVars :: [String] -> String
showVars [s]    = s
showVars (x:xs) = x ++ " " ++ showVars xs

showDeBruijn :: Term -> String
showDeBruijn (Var    i)       = show i
showDeBruijn (App    e1 e2)   = s1 ++ " " ++ s2
  where s1 = case e1 of Lambda _ _ -> "(" ++ showDeBruijn e1 ++ ")"; _ -> showDeBruijn e1
        s2 = case e2 of Var j -> show j; _ -> "(" ++ showDeBruijn e2 ++ ")"
showDeBruijn (Lambda [v]    e) = "\\ " ++ showDeBruijn e
showDeBruijn (Lambda (v:vs) e) = "\\ " ++ showDeBruijn (Lambda vs e)

smashLambda :: Term -> Term
smashLambda (Lambda ns e@(Lambda ns' e')) = Lambda (ns ++ ns') $ smashLambda e'
smashLambda (Lambda vs e)  = Lambda vs $ smashLambda e
smashLambda (App    e1 e2) = App (smashLambda e1) (smashLambda e2)
smashLambda x              = x