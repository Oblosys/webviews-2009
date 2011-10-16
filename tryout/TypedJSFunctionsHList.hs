{-# OPTIONS -XTemplateHaskell -XViewPatterns -XTypeOperators -XEmptyDataDecls -XNoMonomorphismRestriction -XScopedTypeVariables -XDeriveDataTypeable #-}
module Main where


--import Language.Haskell.TH
--import TemplateHaskell
import Data.HList
import Data.HList.Label4
import Data.HList.TypeEqGeneric1
import Data.HList.MakeLabels

$(makeLabels ["pres", "subm", "disen"])

{-
data View1 = View1 WebView Char WebView deriving Show

data View2 = View2 [Char] Char deriving Show

view1 = View1 (WebView 1 'x') 'a' (WebView 2 'y')
view2 = View2 ['a','b'] 'b'

deriveWebView ''View1

deriveWebView ''View2

main::IO ()
main =
 do { --putStrLn $ show $ getTopLevelWebViews_View1 view1
    ; --putStrLn $ show $ getTopLevelWebViews_View2 view2
    ; return ()
    }
-}

data Present = Present deriving Show
data Disenable = Disenable deriving Show
data Submit = Submit deriving Show

data N

data WebView v fns = WebView v [String] deriving Show
  
mkWebView :: v -> ([String],fns) -> WebView v fns
mkWebView v (decls, fns) = WebView v decls 
 
data MyView1 functions = MyView1 Bool [String] deriving Show

mkMyView1 = mkWebView (MyView1 True 
             [ callFnOnWebView mkMyView2 Present
             ] )$ 
             addFunction ("body of present", Present) $
             addFunction ("body of disenable", Disenable) $
             ([], HNil)
             
data MyView2 functions = MyView2 Char [String] deriving Show
mkMyView2 = mkWebView (MyView2 'a'
             [ callFnOnWebView mkMyView1 Disenable
             ] ) $ 
             addFunction ("body of present", Present) $
             addFunction ("body of submit", Submit) $
             ([], HNil)


addFunction :: HExtend t ts tts => (a, t) -> ([a], ts) -> ([a], tts)
addFunction (def,tp) (defs, tps) = (def:defs, tp .*. tps)


functions1 = addFunction ("body of present", Present) $
             addFunction ("body of disenable", Disenable) $
             ([], HNil)


functions2 = Present .*. Submit .*. HNil


data Param2 x = Param2 x deriving Show


--labeledfs2 :: Record HNil
labeledfs = pres .=. Z .*. emptyRecord

--labeledfs2 :: Submit :=: () :*: Record HNil 
labeledfs2 =  pres .=. S Z .*. subm .=. S (S Z) .*.  emptyRecord

getFnType :: WebView v ftp -> ftp
getFnType = undefined

--callFnOnWebView :: forall a l . (HOccurs a l, Show a) => l -> a -> [Char]
callFnOnWebView wv fn = callFn fn (getFnType wv)

callFn :: (HOccurs a l, Show a) => a -> l -> [Char]
callFn fn fns = let _ = [fn, hOccurs fns]
                 in "calling " ++ show fn

data Z = Z deriving Show
data S a = S a deriving Show

--class Exists a where
--  exists :: a -> a
--  exists a = a
 
exists a = let x = show a ++ "" in ()
-- we need show to enforce the type error if label doesn't exist ???


--instance Exists Z
--instance Exists n => Exists (S n)

--contains :: tp -> tp
--contains cnstr = hOccurs
test = let _ = hOccurs functions2 :: Present
           _ = [Submit, hOccurs functions2]
           _ = callFn Submit functions2
           --x = exists $ (labeledfs # ())
           y = exists $ labeledfs2 # subm
       --    _ = labeledfs2 .!. Submit 
       in  ()
main =
 do { putStrLn $ callFn Submit functions2
    --; putStrLn $ show test
    }
--contains :: forall t . t -> t
--contains fn _ = let _ = hOccurs fn
--                in  ()


 
-- hOccurs myTipyCow :: Breed
-- Cow
{-
 *TIP> hExtend BSE myTipyCow
 TIP (HCons BSE
     (HCons (Key 42)
     (HCons (Name "Angus")
     (HCons Cow
     (HCons (Price 75.5)
      HNil)))))

 *TIP> BSE .*. myTipyCow
 --- same as before ---
-}    