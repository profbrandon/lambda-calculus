
module Statements 
  (
    Statement(..)
  , showStatement
  , showStatements
  )
where

import Lambda (Term(..), showTerm)




data Statement =
    Define String [String] Term
  | Eval   Term
  | Step   (Maybe Int) Term
  deriving Eq




instance Show Statement where
  show = showStatement []




showStatement :: [(String, Term)] -> Statement -> String
showStatement ctx (Define s [] e) = "define " ++ s ++ " := " ++ showTerm (s : (map fst ctx)) e
showStatement ctx (Define s vs e) = "define " ++ s ++ showVars vs ++ " := " ++ showTerm ctx' e
  where showVars []               = ""
        showVars (s:vs)           = " " ++ s ++ showVars vs
        ctx'                      = reverse vs ++ s : (map fst ctx)
showStatement ctx (Eval   e)      = "eval " ++ showTerm (map fst ctx) e
showStatement ctx (Step mi e)     = "step " ++ s ++ " " ++ showTerm (map fst ctx) e where s = case mi of Just i -> show i; Nothing -> "?"

showStatements :: [(String, Term)] -> [Statement] -> String
showStatements ctx []     = ""
showStatements ctx (d:ds) = showStatement ctx d ++ "\n" ++
  case d of
    Define s [] e -> showStatements ((s,e):ctx) ds
    Define s vs e -> showStatements ((s, Lambda vs e):ctx) ds
    _            -> showStatements ctx ds