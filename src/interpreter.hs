
module Interpreter
  (
    interpret
  )
where

import Eval (eval, evalCtx)
import Statements (Statement(..))
import State (State(..), get, put)
import Lambda (Term(..), showTerm, showDeBruijn)




type Env = [(String, Term)]




interpret :: Statement -> State Env String 
interpret s = do
  ctx <- get
  case s of
    Define name vars e -> do
      -- insert dummy binding
      let l = evalCtx (("", e):ctx) $ if null vars then e else Lambda vars e
      put $ (name,l) : ctx
      return $ "'" ++ name ++ "' defined"

    Eval e -> do
      let ctx1 = map (\(x,_) -> x) ctx
      return $ "==> " ++ showTerm ctx1 (evalCtx ctx e)