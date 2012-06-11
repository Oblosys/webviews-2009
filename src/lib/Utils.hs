module Utils where

import Data.Generics

import Generics
import Types
import Data.Tree
import WebViewPrim
import HtmlLib

shallowShowWebNode (WebViewNode wv) = "WebNode: " ++ shallowShowWebView wv
shallowShowWebNode (WidgetNode viewId stubId id w) = "WebNode: ("++show viewId++", stub:"++show (unId stubId)++", id:"++show (unId id)++") " ++ show w 

shallowShowWebView (WebView vid sid id _ v) =
  "<WebView: "++show vid ++ ", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ " " ++ show (typeOf v)++ ">"

drawWebNodes webnode = drawTree $ treeFromView webnode
 where treeFromView (WebViewNode wv@(WebView vid sid id _ v)) =
         Node ("("++show vid ++ ", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ ") : " ++ show (typeOf v)) $
              map treeFromView $ getTopLevelWebNodesWebNode v
       treeFromView (WidgetNode vid sid id w) =
         Node ("("++show vid++", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ ") : " ++ showAnyWidget w) $
              map treeFromView $ getTopLevelWebNodesWebNode w
        where showAnyWidget (LabelWidget (LabelView id t)) = "Label "++ show id ++" "++ show t
              showAnyWidget (TextWidget (TextView id t s _)) = "TextView "++ show id ++" "++ show t ++ " " ++ show s
              showAnyWidget (RadioViewWidget (RadioView id is i e)) = "RadioView " ++ show id ++" " ++ show i ++(if e then "enabled" else "disabled") ++ ": "++ show is
              showAnyWidget (ButtonWidget (Button id t _ _ _ _)) = "Button " ++ show id ++ show t
              showAnyWidget (JSVarWidget (JSVar id n v)) = "JSVar "++ show id ++" "++ show n ++ " " ++ show v


getTopLevelWebNodesForWebNode (WidgetNode _ _ _ wn) = []
getTopLevelWebNodesForWebNode (WebViewNode (WebView _ _ _ _ v)) = getTopLevelWebNodesWebNode v

