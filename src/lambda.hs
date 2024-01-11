
module Lambda
  (
    Term(..)
  , Type(..)
  , showType
  , showTerm
  , showDeBruijn
  , buildLambda
  , smashLambda
  )
where

data Term =
    Var    Int
  | App    Term Term 
  | Lambda [String] (Maybe Type) Term
  deriving Eq

data Type = 
    TVar  String
  | Arrow Type Type
  deriving Eq




instance Show Term where
  show = showTerm []

instance Show Type where
  show (TVar name) = name
  show (Arrow s t) = src ++ " -> " ++ show t where src = case s of Arrow _ _ -> "(" ++ show s ++ ")"; _ -> show s




showType :: Type -> String
showType = show

showTerm :: [String] -> Term -> String
showTerm vs (Var    i)       = case drop i vs of [] -> '@' : show i ; vs' -> head vs'
showTerm vs (App    e1 e2)   = wrapLambda vs e1 ++ " " ++ wrapBoth vs e2
showTerm vs (Lambda us t  e) = "\\" ++ showVars us ++ tp ++ ". " ++ showTerm (reverse us ++ vs) e
  where tp = case t of Just t' -> " : " ++ show t'; Nothing -> ""

wrapLambda :: [String] -> Term -> String
wrapLambda vs e@(Lambda _ _ _ ) = "(" ++ showTerm vs e ++ ")"
wrapLambda vs e                 = showTerm vs e

wrapBoth :: [String] -> Term -> String
wrapBoth vs e@(App _ _) = "(" ++ showTerm vs e ++ ")"
wrapBoth vs e           = wrapLambda vs e

showVars :: [String] -> String
showVars [s]    = s
showVars (x:xs) = x ++ " " ++ showVars xs

showDeBruijn :: Term -> String
showDeBruijn (Var    i)       = show i
showDeBruijn (App    e1 e2)   = s1 ++ " " ++ s2
  where s1 = case e1 of Lambda _ _ _ -> "(" ++ showDeBruijn e1 ++ ")"; _ -> showDeBruijn e1
        s2 = case e2 of Var j -> show j; _ -> "(" ++ showDeBruijn e2 ++ ")"
showDeBruijn (Lambda [v]  _  e) = "\\ " ++ showDeBruijn e
showDeBruijn (Lambda (v:vs) t e) = "\\ " ++ showDeBruijn (Lambda vs t e)




buildLambda :: [(String, Maybe Type)] -> Term -> Term
buildLambda vs e = smashLambda $ lambda vs e

lambda :: [(String, Maybe Type)] -> Term -> Term
lambda []               e = e
lambda ((s,Nothing):vs) e = Lambda [s] Nothing $ lambda vs e
lambda ((s,t):vs)       e = Lambda [s] t $ lambda vs e

smashLambda :: Term -> Term
smashLambda (Lambda ns t e@(Lambda ns' t' e'))
  | t == t'   = smashLambda $ Lambda (ns ++ ns') t e'
  | otherwise = Lambda ns t $ smashLambda e
smashLambda (Lambda vs t  e) = Lambda vs t $ smashLambda e
smashLambda (App    e1 e2)   = App (smashLambda e1) (smashLambda e2)
smashLambda x                = x