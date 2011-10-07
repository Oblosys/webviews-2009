{-# OPTIONS -XScopedTypeVariables #-}
module Incrementality where

import Control.Monad.Trans
import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
import Data.Generics
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Data.Tree
import Debug.Trace

import Types
import Generics
import WebViewPrim
import HtmlLib

{-
Right now, incrementality is extremely fragile. Any views that are not presented cause
updates to missing stubs, giving errors.

Problem, parts of the old view that are not updated have their old id's in the browser,
so the rootView are updated to reflect these id's (also for internal id's in widgets)
RestoreId is used for this

pressing the lowest button shows a bug in the incrementality engine.
-}
data Update = Move String IdRef IdRef 
            | RestoreId IdRef IdRef
              deriving Show
              -- move element target 

isMove (Move _ _ _) = True
isMove _          = False




-- TODO: no need to compute new or changed first, can be put in Update list
--       do have to take into account addChangedViewChildren then
diffViews :: Data db => WebView db -> WebView db -> ([WebNode db], [Update])
diffViews oldRootView rootView = 
  let newWebNodeMap = mkWebNodeMap rootView
      oldWebNodeMap = mkWebNodeMap oldRootView
      newOrChangedIdsWebNodes = getNewOrChangedIdsWebNodes oldWebNodeMap newWebNodeMap
      (newOrChangedWebNodeIds, newOrChangedWebNodes) = unzip $ reverse newOrChangedIdsWebNodes
  in  --trace ("\nOld view map\n"++showWebNodeMap oldWebNodeMap ++ "\nNew view map\n" ++ showWebNodeMap newWebNodeMap) 
    ( newOrChangedWebNodes
    , computeMoves oldRootView oldWebNodeMap newOrChangedWebNodeIds rootView)

getNewOrChangedIdsWebNodes :: Data db => WebNodeMap db -> WebNodeMap db -> [(ViewId, WebNode db)]
getNewOrChangedIdsWebNodes oldWebNodeMap newWebNodeMap =
 -- trace ("newWebNodeMap: "++ show (Map.keys newWebNodeMap))$
  filter isNewOrChanged $ Map.toList newWebNodeMap
 where isNewOrChanged (i, webNode) =
         case Map.lookup i oldWebNodeMap of
           Nothing -> True
           Just oldWebNode -> oldWebNode /= webNode
          
computeMoves :: Data db => WebView db -> WebNodeMap db -> [ViewId] -> WebView db -> [Update]           
computeMoves oldRootViewrootView@(WebView _ _ oldRootId _ _) 
             oldWebNodeMap changedOrNewWebNodes rootView@(WebView rootVid stubId rootId _ _) = 
  (if rootVid `elem` changedOrNewWebNodes 
   then [ Move "Root move" (mkRef $ rootId) (mkRef $ oldRootId) ] 
   else []) ++
  concatMap (computeMove oldWebNodeMap changedOrNewWebNodes)
    (getBreadthFirstWebNodes rootView)

showWebNodeMap :: WebNodeMap db -> String
showWebNodeMap wnmap = unlines [ "<"++show k++":"++shallowShowWebNode wn++">" 
                               | (k,wn) <- Map.toList wnmap ] 

-- TODO: If webnodes get fresh view id's, the reuse of the incrementality algorithme gets quite bad.
-- inserting an extra widget at the top of the page may cause a lot of redraws.
-- But at least it seems to be correct for now.

-- if we do the comparison here, also take into account moving the immediate children of a changed
-- view
computeMove :: forall db . Data db => WebNodeMap db -> [ViewId] -> WebNode db -> [Update]
computeMove oldWebNodeMap changedOrNewWebNodes webNode =  
  if getWebNodeViewId webNode `notElem` changedOrNewWebNodes 
  then -- parent has not changed
       let Just oldWebNode = Map.lookup (getWebNodeViewId webNode) oldWebNodeMap 
       in  [RestoreId (mkRef $ getWebNodeId webNode) (mkRef $ getWebNodeId oldWebNode)] ++
           -- restore id's for parent
           
           concat
           [ if childViewId `notElem` changedOrNewWebNodes 
             then -- child has not changed
                  []
             else -- child has changed (or is new, but that doesn't happen)
                  -- Oops, it does when a new button is introduced that gets the viewId of one that
                  -- disappeared
                  -- TODO: check whether this solution is ok
                  [ Move "a" (mkRef $ getWebNodeId childWebNode) 
                              (mkRef $ getWebNodeId ocn) 
                  ]
                {- case Map.lookup childViewId oldWebNodeMap of
                     Just oldChildWebNode ->
                      [ Move "a" (mkRef $ getWebNodeId childWebNode) 
                                 (mkRef $ getWebNodeId oldChildWebNode)
                      ] -- this may not be right if child changed
                     Nothing ->
                      [ Move "a*" (mkRef $ getWebNodeId childWebNode) 
                                  (mkRef $ getWebNodeId ocn) ] -} 
           | let childWebNodes :: [WebNode db] = getTopLevelWebNodesForWebNode webNode
                 oldChildWebnodes :: [WebNode db] = getTopLevelWebNodesForWebNode oldWebNode
           , (childWebNode,ocn) <- --trace ("\nchildren for "++(show $ getWebNodeViewId webNode) ++ 
                             --        ":" ++ show (map shallowShowWebNode childWebNodes)) $ 
                               zip childWebNodes oldChildWebnodes
           , let childViewId = getWebNodeViewId childWebNode
           ]
  
  else -- parent has changed or is new
       concat    
           [ if childViewId `notElem` changedOrNewWebNodes 
             then -- child has not changed
                  let Just oldChildWebNode = Map.lookup childViewId oldWebNodeMap
                  in  [ Move "b" (mkRef $ getWebNodeId oldChildWebNode)  
                                 (mkRef $ getWebNodeStubId childWebNode)
                      ]
             else -- child has changed or is new
                  [ Move "c" (mkRef $ getWebNodeId childWebNode) 
                             (mkRef $ getWebNodeStubId childWebNode)
                  ]                                      
           | let childWebNodes :: [WebNode db] = getTopLevelWebNodesForWebNode webNode
           , childWebNode <- --trace ("\nchildren for "++(show $ getWebNodeViewId webNode) ++ 
                             --        ":" ++ show (map shallowShowWebNode childWebNodes)) $ 
                               childWebNodes
           , let childViewId = getWebNodeViewId childWebNode
           ]
       
getTopLevelWebNodesForWebNode (WidgetNode _ _ _ wn) = []
getTopLevelWebNodesForWebNode (WebViewNode (WebView _ _ _ _ v)) = getTopLevelWebNodesWebNode v




traceArg str x = trace (str ++ show x) x
           

webViewGetId (WebView _ _ i _ _) = i 

    
getWebNodeViewId (WebViewNode (WebView vid _ _ _ _)) = vid      
getWebNodeViewId (WidgetNode vid _ _ _) = vid

getWebNodeId (WebViewNode (WebView _ _ i _ _)) = i      
getWebNodeId (WidgetNode _ _ i _) = i


getWebNodeStubId (WebViewNode (WebView _ si _ _ _)) = si      
getWebNodeStubId (WidgetNode _ si _ _) = si


getWidgetInternalId :: AnyWidget db -> ViewId
getWidgetInternalId  (LabelWidget (LabelView id _)) = id
getWidgetInternalId  (TextWidget (TextView id _ _ _)) = id
getWidgetInternalId  (RadioViewWidget (RadioView id _ _ _)) = id
getWidgetInternalId  (ButtonWidget (Button id _ _ _ _ _)) = id
getWidgetInternalId  (JSVarWidget (JSVar id _ _)) = id
            
getBreadthFirstWebNodes :: Data db => WebView db -> [WebNode db]
getBreadthFirstWebNodes rootView =
  concat $ takeWhile (not . null) $ iterate (concatMap getTopLevelWebNodes) 
                                       [WebViewNode rootView]
 where getTopLevelWebNodes (WebViewNode wv) = getTopLevelWebNodesWebView wv
       getTopLevelWebNodes _ = []
       
mkIncrementalUpdates :: forall db . Data db => WebView db -> WebView db -> IO ([Html], WebView db)
mkIncrementalUpdates oldRootView rootView =
 do { let (newWebNodes :: [WebNode db], updates) = diffViews oldRootView rootView
    --; putStrLn $ "\nChanged or new web nodes\n" ++ unlines (map shallowShowWebNode newWebNodes) 
    --; putStrLn $ "\nUpdates\n" ++ unlines (map show updates)
    
    ; let (newCommands, mEvalCommands) = unzip $ map newWebNodeHtml newWebNodes
    ; let evalCommands = catMaybes mEvalCommands 
    ; let htmlUpdates =newCommands  ++ map updateHtml updates ++ evalCommands
    --; mapM (putStrLn . show) evalCommands
    ; let subs = concat [ case upd of  -- TODO: fix something here
                                    RestoreId (IdRef o) (IdRef n) -> [(Id o, Id n)]  
                                    Move _ _ _ -> []
                                | upd <- updates
                                ]
    --; putStrLn $ "Id updates on rootView:" ++ show subs
    -- todo: check restoration on views, and esp. on root.
    
    ; let rootView' = substituteIds subs rootView
    --; putStrLn $ "Old root:"++ (drawWebNodes $ WebViewNode rootView)
    --; putStrLn $ "Updated root:"++(drawWebNodes $ WebViewNode rootView')
    --; putStrLn $ "Html:\n" ++ show responseHtml
    ; return (htmlUpdates, rootView')
    }
                                
showViewMap viewMap = unlines $ "ViewMap:" : [ show k ++ shallowShowWebView wv | (k, wv) <- Map.toList viewMap ]

-- note, there is no update, just new and move. 
newWebNodeHtml :: WebNode db -> (Html, Maybe Html)
newWebNodeHtml (WidgetNode _ _ (Id i) w) =
  let htmlWithScript = present w
      (html, scripts) = extractScriptHtml htmlWithScript
      updateResponse = thediv![strAttr "op" "new"] << (mkSpan (show i) $ html)
      scriptResponse = thediv![strAttr "op" "eval"] << (stringToHtml $ concat scripts)
  in  (updateResponse, if null scripts then Nothing else Just scriptResponse)
newWebNodeHtml (WebViewNode (WebView _ _ (Id i) _ v)) = 
  let htmlWithScript = present v
      (html, scripts) = extractScriptHtml htmlWithScript
      updateResponse = thediv![strAttr "op" "new"] << (mkSpan (show i) $ html)
      scriptResponse = thediv![strAttr "op" "eval"] << (stringToHtml $ concat scripts)
  in  (updateResponse, if null scripts then Nothing else Just scriptResponse)  

updateHtml :: Update -> Html
updateHtml (Move _ (IdRef src) (IdRef dst)) = if src == dst then error $ "Source is destination: "++show src else
    thediv![strAttr "op" "move", strAttr "src" (show src), strAttr "dst" (show dst)] << ""
updateHtml _ = noHtml -- restoreId is not for producing html, but for adapting the rootView  

shallowShowWebNode (WebViewNode wv) = "WebNode: " ++ shallowShowWebView wv
shallowShowWebNode (WidgetNode _ _ _ w) = "WebNode: " ++ show w 

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
                 
data T = T Char [T]
t0 = T 'a' [T 'b' [T 'd' [], T 'e' []], T 'c' [], T 'f' [T 'g' []]]

bfs (T x cs) = [x] :  (map concat $ transpose $ map bfs cs)
