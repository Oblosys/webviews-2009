{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, NoMonomorphismRestriction, OverloadedStrings #-}
module BlazeHtml (module Text.Blaze.Html5, module Text.Blaze.Html5.Attributes
                 , module BlazeHtml) where

import Data.String
import Data.Monoid

import Text.Blaze.Html5 hiding (div,span,button,map,   style, table)
import Text.Blaze.Html5.Attributes hiding (id,min,max,   cite, form, label, span, summary, title)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Text.Blaze.Html4.Transitional as H4
import qualified Text.Blaze.Html4.Transitional.Attributes as A4

import Text.Blaze.Html.Renderer.String
import qualified Text.Blaze.Html.Renderer.Utf8 as BlazeUtf8
import Text.Blaze.Internal


infixl 8 !* 
html !* attrs = foldl (!) html attrs

div_ = H.div
span_ = H.span
id_ = A.id

button_ = H.button
map_ = H.map
min_ = A.min
max_ = A.max


noHtml = mempty
primHtml = preEscapedString

white = "white"


-- ???

strAttr :: String -> String -> Attribute
strAttr attr val = customAttribute (fromString attr) (toValue val)

instance Show Html where
  show html = renderHtml html

-- Expensive!! Only use sparingly
instance Eq Html where
  h1 == h2 = BlazeUtf8.renderHtml h1  == BlazeUtf8.renderHtml h2

-- todo: why are these not closed? (also not in Text.Html)
textfield str = input ! type_ "text" ! name (fromString str)
password str = input ! type_ "password" ! name (fromString str)
radio str val = input ! type_ "radio" ! name (fromString str) ! value (fromString val)

table = H.table
-- TODO
{-  

operator for attrs instead of attr? 
figure out toValue crap
enable overloaded Strings? helps with some toHtml and toValue cases
-}
-- transition
type HtmlAttr = Attribute



thediv = div_
thespan = span_
theclass = class_ . fromString
thestyle = style . fromString

infixr 2 +++  -- combining Html
(+++) = (>>)

infixr 7 <<   -- nesting Html
html << child = html child

infixl 8 !!! 

html !!! attrs = foldl (!) html attrs

primTag  = customParent


concatHtml :: [Html] -> Html
concatHtml hs = foldr (>>) mempty hs


-- apparently, these are not in html 5
align =  A4.align . fromString
valign = A4.valign . fromString
border = A4.border . fromString
cellpadding = A4.cellpadding . fromString
cellspacing = A4.cellspacing . fromString
center = H4.center
--

test = div_ ! style "dsD" << noHtml


-- Nasty bits for extracting script text that is part of the html tree. This code depends on the internals of blaze html :-(
-- Being able to store scripts in the html makes handling and combining presentations much easier though.
-- TODO: figure out if we can't do the extraction client side.

scriptTag = "WebViewsJavaScript"
-- tag <WebViewsJavaScript> should not be used anywhere else!
-- it is extracted from the html and sent separately to the client (which evaluates it)
-- Scripts are evaluated on creation and change of a WebView
mkScript scriptTxt = primTag scriptTag $ primHtml scriptTxt

-- removes the script elements and returns them in a list
extractScriptHtml :: MarkupM a -> (MarkupM a, [String])
extractScriptHtml (Parent t o e c)           = let (c', scr) = extractScriptHtml c in (Parent t o e c', scr)
extractScriptHtml (Content c)                = (Content c, [])
extractScriptHtml (Append c1 c2)             = let (c1', scr1) = extractScriptHtml c1
                                                   (c2', scr2) = extractScriptHtml c2
                                               in  (Append c1' c2', scr1 ++ scr2)
extractScriptHtml (AddAttribute r k v c)     = let (c', scr) = extractScriptHtml c in (AddAttribute r k v c', scr)
extractScriptHtml (AddCustomAttribute k v c) = let (c', scr) = extractScriptHtml c in (AddCustomAttribute k v c', scr)
extractScriptHtml (CustomParent t c)         = 
  case t of 
    Static sstr | getString sstr "" == scriptTag -> (Empty, [renderHtml $ Append c Empty])                                                                  
    _                                            -> let (c', scr) = extractScriptHtml c in (CustomParent t c', scr)
extractScriptHtml h                          = (h, [])
