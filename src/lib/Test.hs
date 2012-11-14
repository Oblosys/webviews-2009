module Test where
import Control.Monad.State

-- Experimentation module that is not imported by any other module. (allows EclipseFP support with webviews packages loaded and modules available for import)


type S = ()

newtype EditM db a = EditM (S -> IO (a, S))

instance Monad (EditM db) where
  return a = EditM $ \state -> return (a, state)
  (EditM ma) >>= f = EditM $ \state ->
    do { (a,state') <- ma state
       ; let EditM mb = f a
       ; mb state'
       } 
    
instance MonadIO (EditM db) where
  liftIO io = EditM $ \s -> do { a <- io
                               ; return (a,s)
                               } 