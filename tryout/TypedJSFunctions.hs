{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses -XScopedTypeVariables -XEmptyDataDecls -XFlexibleInstances -XFunctionalDependencies #-}
module Main where

{-
Problem: we cannot guarantee declaration or prevent multiple declarations.

TODO: use the params argument.
TODO: maybe add a return type?
-}
data Z

data WebView1 = WebView1
data WebView2 = WebView2


class Show fName => JSFunction webView fName params | webView fName -> params

data Disenable = Disenable deriving Show
data Present = Present deriving Show

instance JSFunction WebView1 Present (String -> String -> Z) 
instance JSFunction WebView2 Present (String -> String -> Z) 
instance JSFunction WebView1 Disenable Z

{- 
$(jsf WebView1 "Present" 3) 
data Present = Present deriving Show
instance JSFunction WebView1 Present (String -> String -> String -> Z)
-}
test =
 do { let wv1 = WebView 3 WebView1
          wv2 = WebView 2 WebView2
          vid1 = getViewId wv1
          vid2 = getViewId wv2        
    ; putStrLn $  declareFunction vid1 Present "present wv1"
    ; putStrLn $  declareFunction vid1 Disenable "disenable"
    ; putStrLn $  declareFunction vid2 Present "present wv2"
    ; putStrLn $  callFunction vid2 Present
    ; putStrLn $  callFunction vid2 Present
    }

data WebView x = WebView Int x

data ViewId t = ViewId Int

getViewId :: WebView t -> ViewId t
getViewId (WebView i _) = ViewId i

declareFunction :: JSFunction webView fName params => ViewId webView -> fName -> String -> String
declareFunction vid fName body = show fName++" = "++show body

callFunction  :: JSFunction webView fName params => ViewId webView -> fName -> String
callFunction vid fName = show fName