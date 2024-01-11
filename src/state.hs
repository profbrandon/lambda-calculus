
module State
  (
    State(..)
  , get
  , put
  )
where

newtype State s a = State {run :: s -> (a, s)}




instance Functor (State s) where
  fmap f m = State $ \s -> let (a,s') = run m s in (f a, s')

instance Applicative (State s) where
  pure     = return
  mf <*> m = mf >>= (\f -> m >>= (\a -> return $ f a))

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  m >>= f  = State $ \s -> let (a', s') = run m s in run (f a') s'




get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)