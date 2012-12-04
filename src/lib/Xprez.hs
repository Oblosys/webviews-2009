{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Xprez where

-- run with:   ghci src/lib/Xprez.hs -isrc/lib

import BlazeHtml
import Text.Blaze.Html
import ObloUtils
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Internal hiding (text)
import Data.String
import Data.List
import Text.Show.Functions


data VAlign = Top | Middle | Bottom

data Attrs = Attrs { getHStretch :: Bool, getVStretch :: Bool
                   , getVAlign :: VAlign -- doesn't work in columns, so use flexVSpace there
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
run x = writeFile "XprezTest.html" . renderHtml . mkTestPage . xp $ x

mkTestPage :: Html -> Html
mkTestPage h = H.html  $
                 (H.head $ (H.style $ preEscapedString xprezCss) +++
                           (H.link ! A.href "scr/css/WebViews.css" ! A.rel "stylesheet" ! A.type_ "text/css")) >>
                 (H.body h)
 where xprezCss = unlines $ [ -- Use this for ad-hoc styles we don't immediately want in WebViews.css.
                            ]

test = row [ col [ h $ H.div ! A.style "background-color: yellow" $ "hello"
                 , h $ H.div ! A.style "background-color: red" $ "bla" ]
           , col [ stretch $ h $ H.div ! A.style "background-color: green" $ "more text" ]
           , col [ stretch $ h $ H.div ! A.style "background-color: grey" $ "text" ]
           ]
test2 = row [hStretch $ col [ hStretch $ h $ H.div ! A.style "background-color: yellow" $ "hello" ]]
testAlign = row [ letter 100, vAlign Top $ letter 10, vAlign Middle $ letter 10, vAlign Bottom $ letter 10 ]
test3 = addStyle "background-color: green" $ col [ vAlign Bottom $ vStretch $ text "bla", text "bla" ]
letter size = addStyle ("font-size: " ++ show size ++"px") $ text "X"

hStretch = With (\a->a{getHStretch=True})
vStretch = With (\a->a{getVStretch=True})
stretch  = With (\a->a{getHStretch=True, getVStretch=True})

flexSpace = stretch $ h noHtml

flexHSpace = hStretch $ h noHtml

flexVSpace = vStretch $ h noHtml

vAlign algn = With (\a->a{getVAlign=algn})

-- append the style to the end of the styles (and inserts a ;)
addStyle :: String -> Xprez -> Xprez
addStyle stl x = With (\a->a{getStyle=stl ++";" ++ getStyle a}) x

xp :: Xprez -> Html
xp x = let (html, attrs) = renderX x 
       in  setChildAttrs (getHStretch attrs) (getVStretch attrs) (getStyle attrs) $ html

{-
TODO: 

Dangerous to set width/heigth with an addStyle. Becomes problematic in http://localhost:8101/#items&q=blaaa.
How to handle setting a constaint on a stretching column?



maybe possible to use ul also when stretching?
- Maybe we can even use 0% for width/height of stretching elts? It seems to lead to even distribution in Firefox & Safari.
- Balance between Html and Xprez is tricky. Do everything in Xprez? Expensive and need to lift all Html functions.
- Is it possible to let content respect max width? So row [ "very long title" ] with width 30px stays within 30px?

-- use style "display: table-cell" to get rid of extra divs?

Alternative for tables for long pages so we can render faster?


Stretching: take proportional amount of space. Non-stretching, take minimal space. How to specify something not to stretch, but still take
maximum space? For text, this is no longer a problem because of the white-space: nowrap in rows.

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
                   in  ( H.table ! A.class_ "Xprez Row" $ -- Row class to allow specific css styling for row/col elements
                           H.tr $ concatHtml $ [ setTDAttrs (Just childWidth) Nothing hStr vStr vAlgn $ H.td $ 
                                                   setChildAttrs hStr vStr stl $ h 
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
                   in  if vStretch
                       then ( H.table ! A.class_ "Xprez Col" $ concatHtml $ -- Col class to allow specific css styling for row/col elements
                                [ H.tr $ setTDAttrs Nothing (Just childHeight) hStr vStr vAlgn $ H.td $ 
                                           setChildAttrs hStr vStr stl $ h 
                                | (h,hStr, vStr, vAlgn, stl) <- zip5 hs hStretches vStretches vAlgns styles 
                                ]                            
                            , defaultAttrs { getHStretch = hStretch, getVStretch = vStretch }
                            )
                       else ( H.ul ! A.class_ "Xprez Col" $ concatHtml $
                                [ H.li $ setChildAttrs hStr vStr stl $ H.div $ h 
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

text :: String -> Xprez
text str = h $ div_ $ toHtml str
