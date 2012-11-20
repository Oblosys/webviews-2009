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


data VAlign = Top | Middle | Bottom

data Attrs = Attrs { getHStretch :: Bool, getVStretch :: Bool
                   , getVAlign :: VAlign
                   , getStyle :: String
                   }
{-
Horizontal css align doesn't work like vertical align. It only seems to work on text, and in contrast to vertical align, we
can set it on a div inside the table cell, so we don't need an attribute for it.
-}
defaultAttrs :: Attrs
defaultAttrs = Attrs False False Middle ""

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


vAlign algn = With (\a->a{getVAlign=algn})

addStyle :: String -> Xprez -> Xprez
addStyle stl x = With (\a->a{getStyle=stl ++";" ++ getStyle a}) x

xp :: Xprez -> Html
xp x = let (html, attrs) = renderX x 
       in  setChildAttrs (getHStretch attrs) (getVStretch attrs) (getStyle attrs) $ html

{-
TODO: 
- Maybe we can even use 0% for width/height of stretching elts? It seems to lead to even distribution in Firefox & Safari.
- Test vertical stretching.


-- use style "display: table-cell" to get rid of extra divs?

Stretching: take proportional amount of space. Non-stretching, take minimal space. How to specify something not to stretch, but still take
maximum space? So "this text" is not broken into "this" and "text"

-}
renderX (Html h) = (h, defaultAttrs)
renderX (Row xs) = let (hs, attrss) = unzip $ map renderX xs
                       styles = map getStyle attrss
                       vAlgns = map getVAlign attrss
                       hStretches = map getHStretch attrss
                       vStretches = map getVStretch attrss
                       hStretch = or hStretches
                       vStretch = and vStretches
                       childWidth = 100 / (fromIntegral $ length $ filter id hStretches)
                   in  ( H.table ! A.class_ "Xprez" $
                           H.tr $ concatHtml $ [ setTDAttrs (Just childWidth) Nothing hStr vStr vAlgn $ H.td $ 
                                                   setChildAttrs hStr vStr stl $ H.div $ h 
                                               | (h,hStr, vStr, vAlgn, stl) <- zip5 hs hStretches vStretches vAlgns styles 
                                               ] 
                       , defaultAttrs { getHStretch = hStretch, getVStretch = vStretch }
                       )
renderX (Col xs) = let (hs, attrss) = unzip $ map renderX xs
                       styles = map getStyle attrss
                       vAlgns = map getVAlign attrss
                       hStretches = map getHStretch attrss
                       vStretches = map getVStretch attrss
                       hStretch = and hStretches
                       vStretch = or vStretches
                       childHeight = 100 / (fromIntegral $ length $ filter id vStretches)
                   in  ( H.table ! A.class_ "Xprez" $ concatHtml $
                           [ H.tr $ setTDAttrs Nothing (Just childHeight) hStr vStr vAlgn $ H.td $ 
                                      setChildAttrs hStr vStr stl $ H.div $ h 
                           | (h,hStr, vStr, vAlgn, stl) <- zip5 hs hStretches vStretches vAlgns styles 
                           ]                            
                       , defaultAttrs { getHStretch = hStretch, getVStretch = vStretch }
                       )
renderX (With f x) = let (hs, attrs) = renderX x in (hs, f attrs)


setTDAttrs mWdth mHght hStr vStr vAlgn h = if null styleStr then h else h ! A.style ( toValue styleStr)
 where styleStr = widthHeightStyle mWdth mHght hStr vStr ++
                  case vAlgn of
                    Top -> "vertical-align:top;"
                    Middle -> ""
                    Bottom -> "vertical-align:bottom;"
 
setChildAttrs hStr vStr stl h = if null styleStr then h else h ! A.style ( toValue styleStr)
 where styleStr = widthHeightStyle Nothing Nothing hStr vStr ++
                  stl
       
widthHeightStyle mWdth mHght hStr vStr = (if hStr then "width: "++widthStr mWdth++";" :: String else "") ++
                                         (if vStr then "height: "++heightStr mHght++";" else "") 
 where widthStr Nothing = "100%"
       widthStr (Just wdth) = showFloat 2 wdth ++ "%"
       heightStr Nothing = "100%"
       heightStr (Just hght) = showFloat 2 hght ++ "%"

row :: [Xprez]-> Xprez
row xs = Row xs

col :: [Xprez]-> Xprez
col xs = Col xs

h :: Html -> Xprez
h html = Html html

