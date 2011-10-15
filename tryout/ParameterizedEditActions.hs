{-# OPTIONS -fglasgow-exts -XUndecidableInstances #-}
module Main where

--class JSCall f where
--  mkJSCall :: f -> String
  
--instance JSCall EditAction where
 
data EditMonad = EditMonad String deriving Show

data EditAction t = EditAction ([String] -> EditMonad)

f0 = EditMonad

f1 :: Char -> EditMonad
f1 c = EditMonad $ show c

f2 :: Char -> Bool -> EditMonad
f2 c b = EditMonad $ show c ++ show b


{-

In a webView spec, you can pass a n-ary function (fn :: x1->x2->xn->EditMonad) to mkEditAction, to create an 
ea :: EditAction (x1 -> x2 -> .. -> xn -> Z), which contains a function \[x1,x2 .. xn] -> fn (read p1) (read p2) .. (read pn) 

and with mkJsCall ea, we get \p1 p2 .. pn -> [p1, p2 .. pn]
with which we can construct "callJsFunction args .." in the script
-}

main =
 do { let a@(EditAction eaf) = mkEditAction f2
    ; putStrLn $ show $ eaf ["'a'","True"]
    ; putStrLn $ show $ mkJsCall a "a" "b"
    }

data Z
data S n
-- instead of S, we can use a function type (a -> b -> Z) if we need the parameter types at jsCall

class MkEditAction f g | f -> g where
  mkEditAction :: f -> EditAction g

instance MkEditAction EditMonad Z where
  mkEditAction ea = EditAction $ \[] -> ea

-- instances by hand
{-
instance Read a => MkEditAction  (a -> EditMonad) (S Z) where
  mkEditAction f = EditAction $ \[a] -> f (read a)
 
instance (Read a, Read b) => MkEditAction (a -> b -> EditMonad) (S (S Z)) where
  mkEditAction f = EditAction $ \[a,b] -> f (read a) (read b)
-}
-- generic instance
instance (MkEditAction f n, Read a) => MkEditAction (a -> f) (S n) where
  mkEditAction f = EditAction $ \(a:xs) -> let EditAction lstF = mkEditAction $ f (read a)
                                           in  lstF xs




mkJsCall ea = mkJsCall' ea []
{-
mkJsCall (ea :: EditAction Z) =                []  
mkJsCall (ea :: EditAction (S Z)) =    \a   -> [a] 
mkJsCall (ea :: EditAction (S(S z))) = \a b -> [a,b]  
-}

class MkJsCall f g | f -> g where
  mkJsCall' :: f -> ([String] -> g)
  
instance MkJsCall (EditAction Z) [String] where
  mkJsCall' _ = \ps  -> reverse ps   

-- instance by hand
{-
instance MkJsCall (EditAction (S Z)) (String ->String) where
  mkJsCall (ea:: EditAction (S Z)) = let recursiveCall = mkJsCall (undefined :: (EditAction Z)) 
                                     in  \ps p -> recursiveCall (p:ps) 
-}
--instance MkJsCall (EditAction (S(S Z))) (String -> String -> String) where
--  mkJsCall _ = \a b -> "call ["++a++","++b++"]"
instance (MkJsCall (EditAction n) f) => MkJsCall (EditAction (S n)) (String ->f) where
  mkJsCall' _ = let recursiveCall = mkJsCall' (undefined :: (EditAction n)) 
                in  \ps p -> recursiveCall (p:ps) 

