module Utils where

import Data.IORef
import Data.Generics

import Generics
import Types
import Data.Tree
import WebViewPrim
import HtmlLib

getRootView :: SessionStateRef db -> IO (WebView db)
getRootView sessionStateRef =
 do { sessionState <- readIORef sessionStateRef
    ; return $ getSStateRootView sessionState
    }
 
setRootView :: SessionStateRef db -> WebView db -> IO ()
setRootView sessionStateRef rootView =
 do { sessionState <- readIORef sessionStateRef
    ; writeIORef sessionStateRef sessionState{ getSStateRootView = rootView }
    }

getSessionHashArgs :: SessionStateRef db -> IO HashArgs
getSessionHashArgs sessionStateRef =
 do { sessionState <- readIORef sessionStateRef
    ; return $ getSStateHashArgs sessionState
    }
 
setSessionHashArgs :: SessionStateRef db -> HashArgs -> IO ()
setSessionHashArgs sessionStateRef hashArgs =
 do { sessionState <- readIORef sessionStateRef
    ; writeIORef sessionStateRef sessionState{ getSStateHashArgs = hashArgs }
    }



shallowShowWebNode (WebViewNode wv) = "WebNode: " ++ shallowShowWebView wv
shallowShowWebNode (WidgetNode viewId stubId id w) = "WebNode: ("++show viewId++", stub:"++show (unId stubId)++", id:"++show (unId id)++") " ++ show w 

shallowShowWebView (WebView vid sid id _ v) =
  "<WebView: "++show vid ++ ", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ " " ++ show (typeOf v)++ ">"

drawWebNodes webnode = drawTree $ treeFromView webnode
 where treeFromView (WebViewNode wv@(WebView vid sid id _ v)) =
         Node ("("++show vid ++ ", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ ") : " ++ show (typeOf v)) $
              map treeFromView $ getTopLevelWebNodes wv
       treeFromView (WidgetNode vid sid id w) =
         Node ("("++show vid++", stub:" ++ show (unId sid) ++ ", id:" ++ show (unId id) ++ ") : " ++ showAnyWidget w) $
              []
        where showAnyWidget (LabelWidget (LabelView id t _)) = "Label "++ show id ++" "++ show t
              showAnyWidget (TextWidget (TextView id t s _ _ _ _)) = "TextView "++ show id ++" "++ show t ++ " " ++ show s
              showAnyWidget (RadioViewWidget (RadioView id is i e _ _)) = "RadioView " ++ show id ++" " ++ show i ++(if e then "enabled" else "disabled") ++ ": "++ show is
              showAnyWidget (SelectViewWidget (SelectView id is i e _ _)) = "SelectView " ++ show id ++" " ++ show i ++(if e then "enabled" else "disabled") ++ ": "++ show is
              showAnyWidget (ButtonWidget (Button id t _ _ _ _)) = "Button " ++ show id ++ show t
              showAnyWidget (JSVarWidget (JSVar id n v)) = "JSVar "++ show id ++" "++ show n ++ " " ++ show v


getTopLevelWebNodesForWebNode :: (Data db) => WebNode db -> [WebNode db]
getTopLevelWebNodesForWebNode (WidgetNode _ _ _ wn) = []
getTopLevelWebNodesForWebNode (WebViewNode wv) = getTopLevelWebNodes wv

