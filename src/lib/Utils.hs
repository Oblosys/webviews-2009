module Utils where

import Data.IORef
import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 
import System.IO
import Control.Monad.Trans (MonadIO, liftIO)
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
              showAnyWidget (EditActionWidget (EditAction id _)) = "EditAction "++ show id


getTopLevelWebNodesForWebNode :: (Data db) => WebNode db -> [WebNode db]
getTopLevelWebNodesForWebNode (WidgetNode _ _ _ wn) = []
getTopLevelWebNodesForWebNode (WebViewNode wv) = getTopLevelWebNodes wv


-- Template utils

substitute ::  [(String, String)] -> String -> String
substitute subsList str = substitute' str  
 where subs = Map.fromList subsList 
       substitute' ""            = ""
       substitute' ('_':'_':str) =
         let (placeholder,_:_:rest) = break (== '_') str
         in  case Map.lookup placeholder subs of
               Just value -> value ++ substitute' rest
               Nothing -> "__"++placeholder++"__" ++ substitute' rest
       substitute' (c:cs) = c:substitute' cs 

getPlaceholders [] = []
getPlaceholders ('_':'_':str) =
  let (placeholder,_:_:rest) = break (== '_') str
  in  placeholder : getPlaceholders rest
getPlaceholders (c:cs) = getPlaceholders cs

-- IO

-- shorthand for liftIO
io :: MonadIO m => IO a -> m a
io = liftIO

-- this function handles UTF files well, unlike readFile

readUTFFile filePath =
 do { h <- openFile  filePath ReadMode
    ; c <- hGetContents h
    ; seq (length c) $ return ()
    ; hClose h
    ; return c
    }
