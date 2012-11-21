module HtmlLib (module Xprez, module HtmlLib) where

import BlazeHtml
import Xprez
import Data.Char hiding (Space)
import Data.List
import Data.String



{-

TODO:
Abstraction for attributes, so multiple style attrs don't replace each other but are concatenated. 

-}

mkDiv str elt = div_ ! id_ (fromString str) $ elt

mkClassDiv classStr elt = div_ ! class_ (fromString classStr) $ elt

mkSpan str elt = span_ ! id_ (fromString str) $ elt

decrease x = x - 1

increase x = x + 1

updateReplaceHtml :: String -> Html -> Html
updateReplaceHtml targetId newElement =
  thediv!!![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

nbsp = primHtml "&nbsp;"


hSpace w = thediv!!![thestyle $ "width: "++show w++"px; height: 0px;"] << noHtml
vSpace h = thediv!!![thestyle $ "height: "++show h++"px; width: 0px;"] << noHtml
                 
nbspaces i = primHtml $ concat $ replicate i "&nbsp;"

-- put <br>'s in between lines, and add an extra <br>&nbsp; if the last line is a \n, 
-- since lines "..." == lines "...\n" (it ignores a single trailing \n), and also we need the nbsp if the last line is an \n
multiLineStringToHtml :: String -> Html
multiLineStringToHtml text = 
  do { sequence_ $ intersperse br $ map toHtml $ lines text 
     ; case text of "" -> return () 
                    txt -> if last txt == '\n' then br >> nbsp else return ()
     }
     
boxed :: Html -> Html
boxed elt = boxedEx 1 4 elt

boxedEx :: Int -> Int -> Html -> Html
boxedEx lineWidth padding elt = thediv!!![thestyle $ "border:solid; border-width:"++show lineWidth++"px; padding:"++show padding++"px;"] << elt

roundedBoxed mColor elt =
  mkTable attrs [] [] [[elt]]
 where attrs = theclass "roundedBorder" : 
               case mColor of Nothing -> []
                              Just color -> [thestyle $ "background-color: "++htmlColor color] 
{-  (thespan!!![theclass"tl"] << noHtml +++ thespan!!![theclass"tr"] << noHtml +++ 
   thespan!!![thestyle"width:95%"] << elt +++
   thespan!!![theclass"bl"] << noHtml +++ thespan!!![theclass"br"] << noHtml)
-}
 {- 
<div class="rounded_colhead" style="background-color: red;">
  <div class="tl"></div><div class="tr"></div>
    I'm pure CSS3 for maximum simplicity and speed in Firefox and Safari,
    but I still look good in IE7 and above!
    Thanks to the magic of conditional CSS and a small bit of
    extra markup, it all comes together.
    In IE6, I look square, but IE6 users, a dwindling breed, are used to ugliness.
  <div class="bl"></div><div class="br"></div>
</div>


 -}
  
-- TODO: name!!!
data ListElt = E Html | Stretch Html

space = Stretch noHtml

-- TODO make hList elts height 100% and vList elts width 100%
hStretchList :: [ListElt] -> Html
hStretchList listElts = table ! width "100%" $ tr $ sequence_ 
                          [ case listElt of E html       -> td ! style "white-space: nowrap" $ html
                                            Stretch html -> td ! thestyle ("width: "++show spaceWidth++"%") $  html 
                          | listElt <- listElts 
                          ]
 where nrOfSpaces = length [ () | Stretch _ <- listElts ]
       spaceWidth = if nrOfSpaces == 0 then 0 else 100 `div` nrOfSpaces 

vStretchList :: [ListElt] -> Html
vStretchList listElts = table ! height "100%" ! style "background-color: red" $ sequence_ 
                          [ case listElt of E html       -> tr $ td $ html
                                            Stretch html -> tr $ td ! thestyle ("height: "++show spaceWidth++"%") $ html 
                          | listElt <- listElts 
                          ]
 where nrOfSpaces = length [ () | Stretch _ <- listElts ]
       spaceWidth = if nrOfSpaces == 0 then 0 else 100 `div` nrOfSpaces 

hDistribute e1 e2 =
  mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],e1), ([align "right"],e2) ]]

hCenter elt = with [thestyle "text-align:center"] elt

hList' elts = ul!!![theclass "hList"] $ sequence_ [ li << elt | elt <- elts ] -- for drag & drop experiments

hList elts = hListEx [] elts

hListEx attrs [] = noHtml
hListEx attrs  elts = simpleTable ([] ++ attrs) [theclass "draggable"] [ elts ]

hListCenter elts = mkTableEx [] [] [style "vertical-align: middle"] [[ ([],elt) |elt <- elts ]] 

vListCenter elts = mkTableEx [] [] [style "text-align: center"] [ [([],elt)] |elt <- elts ] 

vList elts = vListEx [] elts

vListEx attrs  [] = noHtml
vListEx attrs  elts = simpleTable attrs [] [ [e] | e <- elts ]

-- todo: to be removed
simpleTable :: [HtmlAttr] -> [HtmlAttr] -> [[Html]] -> Html
simpleTable tableAttrs allCellAttrs rows = mkTable tableAttrs [] allCellAttrs rows 


mkTable :: [HtmlAttr] -> [[HtmlAttr]] -> [HtmlAttr] -> [[Html]] -> Html
mkTable tableAttrs rowAttrss allCellAttrs rows =
  table!!!tableAttrs $ tbody ! valign "top" $ concatHtml
    [ tr !!! rowAttrs $ mapM_ (td!!!allCellAttrs) row 
    | (rowAttrs, row) <- zip (rowAttrss++repeat []) rows
    ] -- if no row attrss are given (or not enough), just assume no attrs ([])

mkTableEx :: [HtmlAttr] -> [[HtmlAttr]] -> [HtmlAttr] -> [[([HtmlAttr],Html)]] -> Html
mkTableEx tableAttrs rowAttrss allCellAttrs rows =
  table!!!tableAttrs $ tbody ! valign "top" $ concatHtml
    [ tr !!! rowAttrs $ sequence_ [ td!!!(allCellAttrs++cellAttrs) $ cell | (cellAttrs,cell)<-row] 
    | (rowAttrs, row) <- zip (rowAttrss++repeat []) rows
    ] -- if no row attrss are given (or not enough), just assume no attrs ([])

-- make a stretching page with the contents aligned at the top
mkPage :: [HtmlAttr] -> Html -> Html
mkPage attrs elt = table !* ([height "100%", width "100%"]++attrs) $ 
                      tr $ sequence_ [td ! width "50%" $ noHtml, td !* [valign "top", align "center"] $ elt, td ! width "50%" $ noHtml] --[valign "top", align "center"] 
                            --[[div_ ! width "50%" $ noHtml, elt, div_ ! width "50%" $ noHtml]]
                            -- this way the centered div gets minimal size

data Color = Rgb Int Int Int
           | Color String deriving Show

with attrs elt = thediv !!! attrs << elt

withStyle :: String -> Html -> Html
withStyle stl elt = with [style $ toValue stl] elt

withColor color elt = thediv !!! [colorAttr color] << elt

htmlColor :: Color -> String
htmlColor (Rgb r g b) = "#" ++ toHex2 r ++ toHex2 g ++ toHex2 b
htmlColor (Color colorStr) = colorStr


-- style attribute declarations cannot be combined :-( so setting color and then size as attrs
-- will fail. But as Html->Html with divs, there is no problem
fgbgColorAttr color bgColor = 
  thestyle $ "color: "++htmlColor color++"; background-color: "++htmlColor bgColor++";"

colorAttr color = 
  thestyle $ "color: "++htmlColor color++";"

withBgColor color elt = thediv !!! [bgColorAttr color] << elt

bgColorAttr color = 
  thestyle $ "background-color: "++htmlColor color++";"

gradientStyle :: Maybe Int -> String -> String -> String
gradientStyle mHeight topColor bottomColor =
    "background: -moz-linear-gradient("++topColor++" 0px, "++bottomColor++ maybe "" (\h -> " "++show h++"px") mHeight ++ "); "
  ++"background: -webkit-gradient(linear, left top, left "++maybe "bottom" show mHeight ++", from("++topColor++"), to("++bottomColor++"));"

withSize width height elt = thediv!!! [thestyle $ "width: "++show width++"px;" ++
                                                "height: "++show height++"px;" ++
                                                "overflow: auto" ] << elt
withHeight height elt = thediv!!! [thestyle $ "height: "++show height++"px;" ++
                                            "overflow: auto" ] << elt

constrain mn mx x = (mn `max` x) `min` mx

toHex2 :: Int -> String
toHex2 d = [toHexDigit $ d `div` 16] ++ [toHexDigit $ d `mod` 16]

toHexDigit d = let d' = constrain 0 15 d
               in  chr $ d' + if d < 10 then ord '0' else ord 'A' - 10  

withPad left right top bottom h =
  thediv !!! [thestyle $ "padding: "++show top++"px "++show right++"px "++
                       show bottom++"px "++show left++"px;"] << h


image :: String -> Html -- type sig is necessary, otherwise overloaded strings give confusing errors
image filepath = img ! src (toValue $ "/img/"++ filepath)

-- present rating using solid and outlined stars
presentRating :: Int -> Int -> Html
presentRating max rating = primHtml $ concat $ take max $ replicate rating "&#9733;" ++ repeat "&#9734;"

pluralS 1 = ""
pluralS n = "s" 

listCommaAnd :: [String] -> String
listCommaAnd [] = ""
listCommaAnd [s]  = s
listCommaAnd ss@(_:_) = (concat . intersperse ", " $ init ss) ++ " and " ++ last ss 
             

