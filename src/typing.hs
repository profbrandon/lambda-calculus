
module Typing
  (
    typeof
  , subT
  )
where

import Lambda (Term(..), Type(..))




typeof :: [Type] -> Term -> Maybe Type
typeof ts (Var i)     = Just $ head (drop i ts) -- if i >= length ts then Nothing else Just $ head (drop i ts)
typeof ts (App e1 e2) = do
  t1 <- typeof ts e1
  t2 <- typeof ts e2
  case t1 of
    Arrow a b  -> if t2 == a then return b else Nothing
    _          -> Nothing
typeof ts (Lambda [v] t e) = do
  t1 <- t
  t2 <- typeof (t1 : ts) e
  return $ Arrow t1 t2
typeof ts (Lambda (v:vs) t e) = do
  t1 <- t
  t2 <- typeof (t1:ts) (Lambda vs t e)
  return $ Arrow t1 t2

subT :: String -> Type -> Type -> Type
subT s t t'@(TVar s')
  | s == s'            = t
  | otherwise          = t'
subT s t (Arrow t1 t2) = Arrow (subT s t t1) (subT s t t2)