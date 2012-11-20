{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Xprez where

import BlazeHtml
import Text.Blaze.Html
import ObloUtils
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Internal
import Data.String
import Data.List
import Text.Show.Functions
  
data Attrs = Attrs { getHStretch :: Bool, getVStretch :: Bool, getStyle :: String }

data Xprez = Html Html | Row [Xprez] | Col [Xprez] | Table [[Xprez]] | With (Attrs -> Attrs) Xprez deriving Show
  
run :: Xprez -> IO ()
run x = writeFile "Xprez.html" . renderHtml . mkTestPage . xp $ x

mkTestPage :: Html -> Html
mkTestPage h = H.html  $
                 (H.head $ H.style $ preEscapedString xprezCss) >>
                 (H.body h)
 where xprezCss = unlines $ [ "body {padding: 0px; margin: 0px;}"
                            , "table.Xprez {font: inherit; color: inherit; border-spacing:0; border-collapse: collapse; border: 2}"
                            , "table.Xprez td, table.Xprez th { padding: 0;}"
                            ]

test = row [ col [ h $ H.div ! A.style "background-color: yellow" $ "hello"
                 , h $ H.div ! A.style "background-color: red" $ "bla" ]
           , col [ stretch $ h $ H.div ! A.style "background-color: green" $ "more text" ]
           , col [ stretch $ h $ H.div ! A.style "background-color: grey" $ "text" ]
           ]

hStretch = With (\a->a{getHStretch=True})
vStretch = With (\a->a{getVStretch=True})
stretch  = With (\a->a{getHStretch=True, getVStretch=True})

addStyle :: String -> Xprez -> Xprez
addStyle stl x = With (\a->a{getStyle=stl ++";" ++ getStyle a}) x

xp :: Xprez -> Html
xp x = let (html, attrs) = renderX x 
       in  setAttrs Nothing Nothing (getHStretch attrs) (getVStretch attrs) (getStyle attrs) $ html

{-
TODO: 
- Maybe we can even use 0% for width/height of stretching elts? It seems to lead to even distribution in Firefox & Safari.
- Test vertical stretching.



Stretching: take proportional amount of space. Non-stretching, take minimal space. How to specify something not to stretch, but still take
maximum space? So "this text" is not broken into "this" and "text"

-}
renderX (Html h) = (h, Attrs False False "")
renderX (Row xs) = let (hs, attrss) = unzip $ map renderX xs
                       styles = map getStyle attrss
                       hStretches = map getHStretch attrss
                       hStretch = or hStretches
                       childWidth = 100 / (fromIntegral $ length $ filter id hStretches)
                       vStretches = map getVStretch attrss
                       vStretch = and vStretches
                   in  ( H.table ! A.class_ "Xprez" $
                           H.tr $ concatHtml $ [ setAttrs (Just childWidth) Nothing hStr vStr "" $ H.td $ 
                                                   setAttrs Nothing Nothing hStr vStr stl $ H.div $ h 
                                               | (h,hStr, vStr, stl) <- zip4 hs hStretches vStretches styles 
                                               ] 
                       , Attrs { getHStretch = hStretch, getVStretch = vStretch, getStyle = "" }
                       )
renderX (Col xs) = let (hs, attrss) = unzip $ map renderX xs
                       styles = map getStyle attrss
                       hStretches = map getHStretch attrss
                       hStretch = and hStretches
                       vStretches = map getVStretch attrss
                       vStretch = or vStretches
                       childHeight = 100 / (fromIntegral $ length $ filter id vStretches)
                   in  ( H.table ! A.class_ "Xprez" $ concatHtml $
                           [ H.tr $ setAttrs Nothing (Just childHeight) hStr vStr "" $ H.td $ 
                                      setAttrs Nothing Nothing hStr vStr stl $ H.div $ h 
                           | (h,hStr, vStr, stl) <- zip4 hs hStretches vStretches styles 
                           ]                            
                       , Attrs { getHStretch = hStretch, getVStretch = vStretch, getStyle = "" }
                       )
renderX (With f x) = let (hs, attrs) = renderX x in (hs, f attrs)

setAttrs mWdth mHght hStr vStr stl h = if null styleStr then h else h ! A.style ( toValue styleStr)
 where styleStr = (if hStr then "width: "++widthStr mWdth++";" :: String else "") ++
                  (if vStr then "height: "++heightStr mHght++";" else "") ++
                  stl
       widthStr Nothing = "100%"
       widthStr (Just wdth) = showFloat 2 wdth ++ "%"
       heightStr Nothing = "100%"
       heightStr (Just hght) = showFloat 2 hght ++ "%"
       

row :: [Xprez]-> Xprez
row xs = Row xs

col :: [Xprez]-> Xprez
col xs = Col xs

h :: Html -> Xprez
h html = Html html

