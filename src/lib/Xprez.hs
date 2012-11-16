{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}
module Xprez where

import Text.Blaze.Html
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Internal
import Data.Monoid (mempty)
import Data.String

noHtml :: Html
noHtml = mempty

concatHtml :: [Html] -> Html
concatHtml hs = foldr (>>) mempty hs

instance Show Html where
  show html = show (renderHtml html) 
  
data Attrs = Attrs { getHStretch :: Bool, getVStretch :: Bool }

data Xprez = Html Html | Row [Xprez] | Col [Xprez] | Table [[Xprez]] | With (Attrs -> Attrs) Xprez deriving Show

instance Show (Attrs -> Attrs) where
  show f = "(Attrs -> Attrs)"
  
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
           , col [ stretch $ h $ H.div ! A.style "background-color: grey" $ "text" ]
           , col [ h $ H.div ! A.style "background-color: green" $ "more text" ]
           ]

hStretch = With (\a->a{getHStretch=True})
vStretch = With (\a->a{getVStretch=True})
stretch  = With (\a->a{getHStretch=True, getVStretch=True})


xp :: Xprez -> Html
xp x = let (html, attrs) = renderX x in html

renderX (Html h) = (h, Attrs False False)
renderX (Row xs) = let (hs, attrss) = unzip $ map renderX xs
                       hStretches = map getHStretch attrss
                       hStretch = or hStretches
                       vStretches = map getVStretch attrss
                       vStretch = and vStretches
                   in  ( htmlStretch hStretch vStretch $
                           H.table ! A.class_ "Xprez" $
                             H.tr $ concatHtml $ [ htmlStretch hStr vStr $ H.td $ htmlStretch hStr  vStr $ H.div $ h 
                                                 | (h,hStr, vStr) <- zip3 hs hStretches vStretches 
                                                 ] 
                       , Attrs { getHStretch = hStretch, getVStretch = vStretch }
                       )
renderX (Col xs) = let (hs, attrss) = unzip $ map renderX xs
                       hStretches = map getHStretch attrss
                       hStretch = and hStretches
                       vStretches = map getVStretch attrss
                       vStretch = or vStretches
                   in  ( htmlStretch hStretch vStretch $
                           H.table ! A.class_ "Xprez" $ concatHtml $
                             [ H.tr $ htmlStretch hStr vStr $ H.td $ htmlStretch hStr vStr $ H.div $ h 
                             | (h,hStr, vStr) <- zip3 hs hStretches vStretches 
                             ]                            
                       , Attrs { getHStretch = hStretch, getVStretch = vStretch }
                       )
renderX (With f x) = let (hs, attrs) = renderX x in (hs, f attrs)

htmlStretch hStr vStr x = x ! A.style ( toValue $ (if hStr then "width: 100%;" :: String else "") ++
                                                  (if vStr then "height: 100%;" else "")) 

row :: [Xprez]-> Xprez
row xs = Row xs

col :: [Xprez]-> Xprez
col xs = Col xs

h :: Html -> Xprez
h html = Html html

