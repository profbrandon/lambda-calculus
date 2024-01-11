
module Interpreter
  (
    interpret
  )
where

import Typing (typeof, subT)
import Eval (evalCtx, shift)
import Statements (Statement(..))
import State (State(..), get, put)
import Lambda (Term(..), Type(..), buildLambda, showTerm, showType, showDeBruijn)




type Env = (Bool, [(String, Term, Maybe Type)])




interpret :: Statement -> State Env String 
interpret s = do
  (typed, bindings) <- get
  let ts = if typed then map (\(_,_,Just z) -> z) bindings else []
  case s of
    TBlock  -> do {put (True, []); return ""}
    UTBlock -> do {put (False, []); return ""}

    Define name vars e -> do
      let l = if null vars then e else buildLambda vars e
      if not typed then do
        put (typed, (name,l,Nothing) : bindings)
        return $ "'" ++ name ++ "' defined"
      else do
        put (typed, (name,l, Just (TVar $ "$" ++ name)) : bindings)
        (_, bindings') <- get
        let ts = map (\(_,_,Just z) -> z) bindings'
        case typeof ts l of
          Nothing -> return $ "Error: Definition of '" ++ name ++  "' failed to typecheck"
          Just t  -> do
            put (typed, (name, l, Just $ subT ("$" ++ name) t t) : bindings)
            return $ "'" ++ name ++ "' defined"

    Eval e -> do
      let ctx1 = map (\(x,_,_) -> x) bindings
      let ctx2 = map (\(x,y,_) -> (x,y)) bindings
      if not typed then do
        return $ "==> " ++ showTerm ctx1 (evalCtx ctx2 e)
      else do
        case typeof ts e of
          Nothing -> return $ "Error: Term failed to typecheck in evalutation statement"
          Just _  -> do
            return $ "==> " ++ showTerm ctx1 (evalCtx ctx2 e)

    TypeOf e ->
      if not typed then do
        return $ "Error: Type check exists in an untyped context"
      else do
        case typeof ts e of
          Nothing -> return $ "Error: Term failed to typecheck"
          Just t  -> do
            return $ "  : " ++ showType t