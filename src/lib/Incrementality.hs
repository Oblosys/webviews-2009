{-# OPTIONS -XScopedTypeVariables #-}
module Incrementality (mkIncrementalUpdates) where

import Control.Monad.Trans
import Data.List
import BlazeHtml
import Data.Generics
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace

import Types
import Generics
import WebViewPrim
import HtmlLib
import Utils

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


mkIncrementalUpdates :: forall db . Data db => WebView db -> WebView db -> IO ([Html], WebView db)
mkIncrementalUpdates oldRootView rootView =
 do { let (newWebNodes :: [WebNode db], updates) = diffViews oldRootView rootView
    ; putStrLn $ "\nChanged or new web nodes\n" ++ unlines (map shallowShowWebNode newWebNodes) 
    ; putStrLn $ "\nUpdates\n" ++ unlines (map show updates)
    
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
    ; putStrLn $ "Old root:"++ (drawWebNodes $ WebViewNode oldRootView)
    ; putStrLn $ "Updated root:"++ (drawWebNodes $ WebViewNode rootView)
    ; putStrLn $ "Restored Id root:"++(drawWebNodes $ WebViewNode rootView')
    --; putStrLn $ "Html:\n" ++ show responseHtml
    ; return (htmlUpdates, rootView')
    }


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
  --trace ("newWebNodeMap: "++ show (Map.keys newWebNodeMap))$
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
    (getBreadthFirstWebNodes rootView) -- TODO: OPT can't we use the newWebNodeMap instead of calling getBreadthFirstWebNodes here?

showWebNodeMap :: WebNodeMap db -> String
showWebNodeMap wnmap = unlines [ "<"++show k++":"++shallowShowWebNode wn++">" 
                               | (k,wn) <- Map.toList wnmap ] 

{- Unchanged nodes that keep the same viewId are efficiently reused, so we should try to keep viewIds
   as constant as possible. 
-}           
computeMove :: forall db . Data db => WebNodeMap db -> [ViewId] -> WebNode db -> [Update]
computeMove oldWebNodeMap changedOrNewWebNodes webNode =  
  if getWebNodeViewId webNode `notElem` changedOrNewWebNodes 
  then -- parent has not changed, so we move to the oldChildWebNode in the old parent
       let Just oldWebNode = Map.lookup (getWebNodeViewId webNode) oldWebNodeMap 
       in  [RestoreId (mkRef $ getWebNodeId webNode) (mkRef $ getWebNodeId oldWebNode)] ++
           -- restore id's for parent
           concat
           [ if childViewId `notElem` changedOrNewWebNodes 
             then if childViewId == oldChildViewId 
                  then [] -- same child, which hasn't changed, so do nothing
                  else -- different child, but it hasn't changed, so we move it from its old location to here
                       [ let Just oldSrcChild = Map.lookup childViewId oldWebNodeMap
                         in  Move "a" (mkRef $ getWebNodeId oldSrcChild) 
                                      (mkRef $ getWebNodeId oldChildWebNode) ]
             else -- child has changed or is new, so we move it from the new nodes to its destination
                  [ Move "b" (mkRef $ getWebNodeId childWebNode) 
                             (mkRef $ getWebNodeId oldChildWebNode) 
                  ]
                                  
           | let childWebNodes    :: [WebNode db] = getTopLevelWebNodesForWebNode webNode
                 oldChildWebnodes :: [WebNode db] = getTopLevelWebNodesForWebNode oldWebNode
                 -- Lists have equal length, otherwise this webnode would not be in the unchangedWebNodes
           , (childWebNode,oldChildWebNode) <- --trace ("\nchildren for "++(show $ getWebNodeViewId webNode) ++ 
                             --        ":" ++ show (map shallowShowWebNode childWebNodes)) $ 
                               zip childWebNodes oldChildWebnodes
           , let childViewId    = getWebNodeViewId childWebNode
           , let oldChildViewId = getWebNodeViewId oldChildWebNode
           ]
  
  else -- parent has changed or is new, so we move to its child stubs
       concat    
           [ if childViewId `notElem` changedOrNewWebNodes 
             then -- child has not changed, so we move it from its old location to here
                  -- because the parent is new, there will not be a child in place already, so we always 
                  -- need to do this move.
                  let Just oldChildWebNode = Map.lookup childViewId oldWebNodeMap
                  in  [ Move "c" (mkRef $ getWebNodeId oldChildWebNode)  
                                 (mkRef $ getWebNodeStubId childWebNode)
                      ]
             else -- child has changed or is new, so we move it from the new nodes to its destination
                  [ Move "d" (mkRef $ getWebNodeId childWebNode) 
                             (mkRef $ getWebNodeStubId childWebNode)
                  ]                                      
           | let childWebNodes :: [WebNode db] = getTopLevelWebNodesForWebNode webNode
           , childWebNode <- --trace ("\nchildren for "++(show $ getWebNodeViewId webNode) ++ 
                             --        ":" ++ show (map shallowShowWebNode childWebNodes)) $ 
                               childWebNodes
           , let childViewId = getWebNodeViewId childWebNode
           ]
       




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
getWidgetInternalId  (SelectViewWidget (SelectView id _ _ _)) = id
getWidgetInternalId  (ButtonWidget (Button id _ _ _ _ _)) = id
getWidgetInternalId  (JSVarWidget (JSVar id _ _)) = id

-- return al list of all WebNodes in rootView            
getBreadthFirstWebNodes :: Data db => WebView db -> [WebNode db]
getBreadthFirstWebNodes rootView =
  concat $ takeWhile (not . null) $ iterate (concatMap getTopLevelWebNodes) 
                                       [WebViewNode rootView]
 where getTopLevelWebNodes (WebViewNode wv) = getTopLevelWebNodesWebView wv
       getTopLevelWebNodes _ = []
                                       
showViewMap viewMap = unlines $ "ViewMap:" : [ show k ++ shallowShowWebView wv | (k, wv) <- Map.toList viewMap ]

-- note, there is no update, just new and move. 
newWebNodeHtml :: WebNode db -> (Html, Maybe Html)
newWebNodeHtml (WidgetNode _ _ (Id i) w) =
  let htmlWithScript = present w
      (html, scripts) = extractScriptHtml htmlWithScript
      updateResponse = div_ ! strAttr "op" "new"  $ (mkSpan (show i) $ html)
      scriptResponse = div_ ! strAttr "op" "eval" $ (toHtml $ concat scripts)
  in  (updateResponse, if null scripts then Nothing else Just scriptResponse)
newWebNodeHtml (WebViewNode (WebView _ _ (Id i) _ v)) = 
  let htmlWithScript = present v
      (html, scripts) = extractScriptHtml htmlWithScript
      updateResponse = div_ ! strAttr "op" "new" $ (mkSpan (show i) $ html)
      scriptResponse = div_ ! strAttr "op" "eval" $ (toHtml $ concat scripts)
  in  (updateResponse, if null scripts then Nothing else Just scriptResponse)  

updateHtml :: Update -> Html
updateHtml (Move _ (IdRef src) (IdRef dst)) = if src == dst then error $ "Source is destination: "++show src else
    div_ ! strAttr "op" "move" ! strAttr "src" (show src) ! strAttr "dst" (show dst) $ noHtml
updateHtml _ = noHtml -- restoreId is not for producing html, but for adapting the rootView  

                 
data T = T Char [T]
t0 = T 'a' [T 'b' [T 'd' [], T 'e' []], T 'c' [], T 'f' [T 'g' []]]

bfs (T x cs) = [x] :  (map concat $ transpose $ map bfs cs)
