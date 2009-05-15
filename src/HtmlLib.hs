module HtmlLib where

import Text.Html
import Data.Char
import Data.List

decrease x = x - 1

increase x = x + 1

updateReplaceHtml :: String -> Html -> Html
updateReplaceHtml targetId newElement =
  thediv![strAttr "op" "replace", strAttr "targetId" targetId ] 
    << newElement

nbsp = primHtml "&nbsp;"

mkDiv str elt = thediv![identifier str] << elt

mkSpan str elt = thespan![identifier str] << elt

hSpace w = thediv![thestyle $ "width: "++show w++"px; height: 0px;"] << noHtml
vSpace h = thediv![thestyle $ "height: "++show h++"px; width: 0px;"] << noHtml
                 
nbspaces i = primHtml $ concat $ replicate i "&nbsp;"

multiLineStringToHtml text = 
  (intersperse br $ map stringToHtml $ lines text) +++
  case text of "" -> []
               txt -> if last txt == '\n' then [br+++nbsp] else []

boxed elt = thediv![thestyle "border:solid; border-width:1px; padding:4px;"] << elt

roundedBoxed mColor elt =
  thediv!(theclass "rounded_colhead" : 
          case mColor of Nothing -> []
                         Just color -> [thestyle $ "background-color: "++htmlColor color]) << 
    elt
{-  (thespan![theclass"tl"] << noHtml +++ thespan![theclass"tr"] << noHtml +++ 
   thespan![thestyle"width:95%"] << elt +++
   thespan![theclass"bl"] << noHtml +++ thespan![theclass"br"] << noHtml)
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
hDistribute e1 e2 =
  mkTableEx [width "100%", border 0, cellpadding 0, thestyle "border-collapse: collapse;"] [] [valign "top"]
       [[ ([],e1), ([align "right"],e2) ]]
        

hList [] = noHtml
hList views = simpleTable [] [] [ views ]

vList [] = noHtml
vList views = simpleTable [] [] [ [v] | v <- views ]

mkTable :: [HtmlAttr] -> [[HtmlAttr]] -> [HtmlAttr] -> [[Html]] -> Html
mkTable tableAttrs rowAttrss cellAttrs rows =
  table!tableAttrs << concatHtml
    [ tr!rowAttrs << map (td!cellAttrs) row 
    | (rowAttrs, row) <- zip (rowAttrss++repeat []) rows
    ] -- if no row attrss are given (or not enough), just assume no attrs ([])

mkTableEx :: [HtmlAttr] -> [[HtmlAttr]] -> [HtmlAttr] -> [[([HtmlAttr],Html)]] -> Html
mkTableEx tableAttrs rowAttrss allCellAttrs rows =
  table!tableAttrs << concatHtml
    [ tr!rowAttrs << [ td!(allCellAttrs++cellAttrs) << cell | (cellAttrs,cell)<-row] 
    | (rowAttrs, row) <- zip (rowAttrss++repeat []) rows
    ] -- if no row attrss are given (or not enough), just assume no attrs ([])

data Color = Rgb Int Int Int
           | Color String deriving Show

with_ attrs elt = thediv ! attrs << elt

withColor color elt = thediv ! [colorAttr color] << elt

htmlColor :: Color -> String
htmlColor (Rgb r g b) = "#" ++ toHex2 r ++ toHex2 g ++ toHex2 b
htmlColor (Color colorStr) = colorStr


-- style attribute declarations cannot be combined :-( so setting color and then size as attrs
-- will fail. But as Html->Html with divs, there is no problem
fgbgColorAttr color bgColor = 
  thestyle $ "color: "++htmlColor color++"; background-color: "++htmlColor bgColor++";"

colorAttr color = 
  thestyle $ "color: "++htmlColor color++";"

withBgColor color elt = thediv ! [bgColorAttr color] << elt

bgColorAttr color = 
  thestyle $ "background-color: "++htmlColor color++";"

withSize width height elt = thediv! [thestyle $ "width: "++show width++"px;" ++
                                                "height: "++show height++"px;" ++
                                                "overflow: auto" ] << elt
withHeight height elt = thediv! [thestyle $ "height: "++show height++"px;" ++
                                            "overflow: auto" ] << elt

constrain mn mx x = (mn `max` x) `min` mx

toHex2 :: Int -> String
toHex2 d = [toHexDigit $ d `div` 16] ++ [toHexDigit $ d `mod` 16]

toHexDigit d = let d' = constrain 0 15 d
               in  chr $ d' + if d < 10 then ord '0' else ord 'A' - 10  

withPad left right top bottom h =
  thediv ! [thestyle $ "padding: "++show top++"px "++show right++"px "++
                       show bottom++"px "++show left++"px;"] << h

image filename = Text.Html.image ! [src $ "/img/"++ filename ]

pluralS 1 = ""
pluralS n = "s" 

listCommaAnd :: [String] -> String
listCommaAnd [] = ""
listCommaAnd [s]  = s
listCommaAnd ss@(_:_) = (concat . intersperse ", " $ init ss) ++ " and " ++ last ss 


