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

mkDiv str elt = thediv![identifier str] << elt

mkSpan str elt = thespan![identifier str] << elt

boxed html = thediv![thestyle "border:solid; border-width:1px; padding:4px;"] << html

hList [] = noHtml
hList views = simpleTable [] [] [ views ]

vList [] = noHtml
vList views = simpleTable [] [] [ [v] | v <- views ]

data Color = Rgb Int Int Int
           | Color String deriving Show

withBgColor color elt = thediv ! [bgColorAttr color] << elt

bgColorAttr color = 
  let colorStr = case color of
                     Rgb r g b      -> "#" ++ toHex2 r ++ toHex2 g ++ toHex2 b
                     Color colorStr -> colorStr
  in thestyle $ "background-color: "++colorStr++";"

withSize width height elt = thediv! [thestyle $ "width: "++show width++"px;" ++
                                                "height: "++show height++"px;" ++
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


