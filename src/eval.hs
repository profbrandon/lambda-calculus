
module Eval 
  (
    eval
  , evalCtx
  , shift
  )
where

import Lambda (Term(..), smashLambda)
import Parser




evalCtx :: [(String, Term)] -> Term -> Term
evalCtx ctx e = eval $ help ctx e
  where help []           e = e
        help ((s,e'):ctx) e = 
          let e'' = shift (-1) 1 e'
          in help ctx $ App (Lambda [s] Nothing e) e''

eval :: Term -> Term
eval e = if e' == e then e' else eval e' where e' = eval0 e

eval0 :: Term -> Term
eval0 (App    e1 e2)   = 
  case e1' of
    Lambda [v]    _ e -> shift (-1) 0 $ sub 0 (shift 1 0 e2) e

    Lambda (v:vs) t e -- Since there are (length vs) lambdas still present, we must shift
                      -- the variables in e by this value and one more to compensate for
                      -- the value we will shift by afterwards. Furthermore, the value of 
                      -- the variable v is also (length vs) so this is the value being 
                      -- substituted in e2' for e. All values above (length vs) need to be
                      -- shifted by (-1) since the outer lambda is being destroyed.
                      -> Lambda vs t $ eval $ shift (-1) n $ sub n (shift (n + 1) 0 e2) e 
                         where n = length vs

    _                 -> App e1' (eval e2)
  where e1' = eval e1
eval0 (Lambda [v]    t  (App e@(Var _) (Var 0))) = shift (-1) 0 e -- Eta reduction
eval0 (Lambda [v]    t  e)               = Lambda [v] t $ eval e
eval0 (Lambda (v:vs) t  e)               = smashLambda $ Lambda [v] t $ eval $ Lambda vs t e
eval0 e                                  = e

sub :: Int -> Term -> Term -> Term
sub i e (Var    j)
  | i == j                = e
  | otherwise             = Var j
sub i e (App    e1 e2)    = App (sub i e e1) (sub i e e2)
sub i e (Lambda vs t  e') = Lambda vs t $ sub (i + length vs) (shift (length vs) 0 e) e'

deepSub :: Int -> Term -> Term -> Term
deepSub i e2 e1
  | e1 == e1' = e1
  | otherwise = deepSub i e2 e1'
  where e1' = sub i e2 e1

shift :: Int -> Int -> Term -> Term
shift i above (Var    j)      = if j >= above then Var (j + i) else Var j
shift i above (App    e1 e2)  = App (shift i above e1) (shift i above e2)
shift i above (Lambda vs t e) = Lambda vs t (shift i (above + length vs) e)