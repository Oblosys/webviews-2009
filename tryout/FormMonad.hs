{-# OPTIONS -fglasgow-exts #-}
module FormMonad where



data FormOrVal a = Form String | Val a deriving Show

newtype FormMonad a = FM {runFM :: ([String] -> (FormOrVal a, [String]))}

instance Monad FormMonad where
  return a = FM $ \env -> (Val a, env)
  FM ma >>= f = FM $ \env -> case ma env of
                               (Form str, env') -> (Form str, env') 
                               (Val a, env') -> let FM fb = f a
                                                in fb env'

form :: Read a => String -> FormMonad a
form str = FM $ \env -> case env of
                          [] -> (Form str, [])
                          (v:env') -> (Val $ read v, env')

stringForm :: FormMonad String
stringForm = form "<String Input>"

intForm :: FormMonad Int
intForm = form "<Int Input>"

emptyForm :: String -> FormMonad ()
emptyForm str = form str
 
test :: FormMonad ()
test = do { name <- stringForm
          ; age <- intForm
          ; () <- emptyForm $ "Hello " ++ name ++ " of age " ++ show age
          ; test
          }               
{-
form str = FM $ \env ->
                  case env of
                    [] -> Form str
                   -- (str: env') -> 

-}
{-
instance Monad (FormMonad (FormOrValue)) where
  return a = FM $ \env -> 
               case env of
                 []      -> a
                 (v:env) -> undefined

  (FM ma) >>= f = FM $ \env -> 
                    case env of
                      [] -> ma env


runForm (FM fm) =
  let str = fm [] 
  in  str

form :: FormMonad (FormOrValue ())
form = 
 do { name <- nameForm
    ; age <- ageForm
    ; helloForm (valueOf name) (valueOf age)
    }

nameForm = return $ Form "Name form"
ageForm = return $ Form "Age form"
helloForm name age  = return $ Form $ "Hello form: "++name++age         
-}
{-
data FormOrValue = Form String | Value { valueOf :: String} deriving Show

bind :: FormOrValue -> (String -> [String] -> FormOrValue) -> ([String] -> FormOrValue)
bind m f = \env ->
             case env of 
               []     -> m
               (v:vs) -> f v vs
 
form = Form

terminator _ = error "terminator evaluated"

myForm = nameForm `bind`  \name ->
         ageForm `bind` \age ->
         helloForm name age `bind` \_ ->
         myForm

nameForm = form "<input Name>"
ageForm = form "<input Age>"
helloForm name age = form $ "Hello "++name++", "++age
-}

