
module Statements 
  (
    Statement(..)
  , showStatement
  , showStatements
  )
where

import Lambda (Term(..), Type(..), showType, showTerm, buildLambda)




data Statement =
    TBlock
  | UTBlock
  | Define String [(String, Maybe Type)] Term
  | Eval   Term
  | TypeOf Term
  deriving Eq




instance Show Statement where
  show = showStatement []




showStatement :: [(String, Term)] -> Statement -> String
showStatement _   TBlock          = "(# TYPED #)"
showStatement _   UTBlock         = "(# UNTYPED #)"
showStatement ctx (Define s [] e) = "define " ++ s ++ " := " ++ showTerm (s : (fst $ unzip ctx)) e
showStatement ctx (Define s vs e) = "define " ++ s ++ showVars vs ++ " := " ++ showTerm ctx' e
  where showVars []               = ""
        showVars ((s,Nothing):vs) = " " ++ s ++ showVars vs
        showVars ((s,Just t):vs)  = " (" ++ s ++ " : " ++ showType t ++ ")" ++ showVars vs
        ctx'                      = reverse (fst $ unzip vs) ++ s : (fst $ unzip ctx)
showStatement ctx (Eval   e)      = "eval " ++ showTerm (fst $ unzip ctx) e
showStatement ctx (TypeOf e)      = "typeof " ++ showTerm (fst $ unzip ctx) e

showStatements :: [(String, Term)] -> [Statement] -> String
showStatements ctx []     = ""
showStatements ctx (d:ds) = showStatement ctx d ++ "\n" ++
  case d of
    Define s [] e -> showStatements ((s,e):ctx) ds
    Define s vs e -> showStatements ((s, buildLambda vs e):ctx) ds
    _            -> showStatements ctx ds