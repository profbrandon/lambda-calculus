
module Interpreter
  (
    interpret
  )
where

import Eval (eval, eval1, evalCtx, shift)
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
      let l = evalCtx eval (("", e):ctx) $ if null vars then e else Lambda vars e
      put $ (name,l) : ctx
      return $ "'" ++ name ++ "' defined"

    Eval e -> do
      let ctx1 = map (\(x,_) -> x) ctx
      return $ "==> " ++ showTerm ctx1 (evalCtx eval ctx e)

    Step mi e -> do
      let ctx1 = map (\(x,_) -> x) ctx
      case mi of
        Just i -> do
          let evalI = foldr (.) id (take i $ repeat eval1)
          return $ "==> " ++ showTerm ctx1 (evalCtx evalI ctx e)
        
        Nothing -> do
          let (i, n) = normalize (0, e)
          return $ "==> " ++ show i ++ " steps until normalization to " ++ showTerm ctx1 n
          where
            normalize (i, e) = let e' = evalCtx eval1 ctx e in if e == e' then (i, e) else normalize (i + 1, e')