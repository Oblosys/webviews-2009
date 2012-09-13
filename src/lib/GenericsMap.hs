module GenericsMap where

import Types
import ObloUtils

import Data.Generics
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet 
import Debug.Trace


getAllIds :: forall db . WebView db -> [Id]
getAllIds rootView = snd $ mapWebView (getAllIdsWV, getAllIdsWd, True) rootView [] 
  where getAllIdsWV :: [Id] -> WebView db -> (WebView db, [Id])
        getAllIdsWV ids wv@(WebView _ sid id _ _) = (wv, sid:id:ids)
        getAllIdsWd ids wd@(Widget sid id _)          = (wd, sid:id:ids)

clearIds :: forall db . WebView db -> WebView db
clearIds rootView = fst $ mapWebView (clearIdsWV, clearIdsWd, True) rootView () 
  where clearIdsWV :: () -> WebView db -> (WebView db, ())
        clearIdsWV _ wv@(WebView vid _ _ mkF v) = (WebView vid noId noId mkF v, ())
        clearIdsWd _ wd@(Widget _ _ w)          = (Widget noId noId w, ())

assignIdsFromList :: forall db . (Data db) => [Id] -> WebView db -> WebView db
assignIdsFromList allIds rootView =
  let assigned = fst $ mapWebView (assignIdsWV, assignIdsW, True) rootView freeIds
  in {- trace (if [] /= filter (==Id (-1))(getAll assigned :: [Id]) then show assigned else "ok") $ -} assigned
 where usedIds = IntSet.fromList $ map unId $ filter (/= noId) $ allIds 
       freeIds = (IntSet.fromList $ [0 .. length allIds - 1]) `IntSet.difference` usedIds

       assignIdsWV :: IntSet -> WebView db -> (WebView db, IntSet)
       assignIdsWV state (WebView vi sid id mkF v) = (WebView vi sid' id' mkF v, state'')
        where (sid',state') = mkId state sid
              (id',state'') = mkId state' id
       
       assignIdsW state (Widget sid id w) = (Widget sid' id' w, state'')
        where (sid',state') = mkId state sid
              (id',state'') = mkId state' id

mkId ids (Id i) = if (i == -1) 
                         then if IntSet.null ids 
                                         then error "Internal error: assign Id, empty id list"
                                         else let (newId, ids') = IntSet.deleteFindMin ids 
                                              in  (Id newId, ids') 
                                    else (Id i, ids)       

getTopLevelWebViews :: (Typeable db) => WebView db -> [WebView db]
getTopLevelWebViews wv = [w | WebViewNode w <- getTopLevelWebNodes wv]

-- get top-level WebNodes (WebViews and widgets), not including the WebView itself
getTopLevelWebNodes :: (Typeable db) => WebView db -> [WebNode db]
getTopLevelWebNodes (WebView _ _ _ _ v) = map snd $ getWebNodesAndViewIds False v

mkWebNodeMap :: (Typeable db) => WebView db -> WebNodeMap db
mkWebNodeMap wv = Map.fromList $ getWebNodesAndViewIds True wv

mkViewMap :: Typeable db => WebView db -> ViewMap db
mkViewMap wv = Map.fromList $ [ (vid, wv) | (vid, WebViewNode wv) <- getWebNodesAndViewIds True wv ]

getLabelViewByViewId :: Typeable db => ViewId -> WebView db -> LabelView db
getLabelViewByViewId i wv =
  case getAnyWidgetById i wv of
    LabelWidget x -> x
    _             -> error $ "internal error: widget with id " ++ show i ++ " is not a LabelView" 

getTextViewByViewId :: Typeable db => ViewId -> WebView db -> TextView db
getTextViewByViewId i wv =
  case getAnyWidgetById i wv of
    TextWidget x -> x
    _              -> error $ "internal error: widget with id " ++ show i ++ " is not a TextView" 

getRadioViewByViewId :: Typeable db => ViewId -> WebView db -> RadioView db
getRadioViewByViewId i wv =
  case getAnyWidgetById i wv of
    RadioViewWidget x -> x
    _                 -> error $ "internal error: widget with id " ++ show i ++ " is not a RadioView" 

getSelectViewByViewId :: Typeable db => ViewId -> WebView db -> SelectView db
getSelectViewByViewId i wv =
  case getAnyWidgetById i wv of
    SelectViewWidget x -> x
    _                  -> error $ "internal error: widget with id " ++ show i ++ " is not a SelectView" 


getButtonByViewId :: Typeable db => ViewId -> WebView db -> Button db
getButtonByViewId i wv =
  case getAnyWidgetById i wv of
    ButtonWidget x -> x
    _              -> error $ "internal error: widget with id " ++ show i ++ " is not a Button" 

getJSVarByViewId :: Typeable db => ViewId -> WebView db -> JSVar db
getJSVarByViewId i wv =
  case getAnyWidgetById i wv of
    JSVarWidget x -> x
    _             -> error $ "internal error: widget with id " ++ show i ++ " is not a JSVar" 

getWebViewById :: Typeable db => ViewId -> WebView db -> WebView db
getWebViewById i wv = 
  case getWebNodeById "getWebViewById" i wv of
    (WebViewNode wv) -> wv
    _                -> error $ "internal error: webnode with id " ++ show i ++ " is not a WebViewNode"

getAnyWidgetById :: Typeable db => ViewId -> WebView db -> AnyWidget db
getAnyWidgetById i wv = 
  case getWebNodeById "getWebViewById" i wv of
    (WidgetNode _ _ _ wd) -> wd
    _                -> error $ "internal error: webnode with id " ++ show i ++ " is not a WidgetNode"

getWebNodeById :: Typeable db => String -> ViewId -> WebView db -> WebNode db
getWebNodeById callerTag i wv = 
  case [ wn | (vid, wn) <- getWebNodesAndViewIds True wv, vid == i ] of
    [b] -> b
    []  -> error $ "internal error: getWebNodeById (called by "++callerTag++"): no webnode with id " ++ show i
    _   -> error $ "internal error: getWebNodeById (called by "++callerTag++"): multiple webnode with id " ++ show i

getWebNodesAndViewIds :: forall db v . (Typeable db, MapWebView db v) => Bool -> v -> [(ViewId, WebNode db)]
getWebNodesAndViewIds recursive v = snd $ mapWebView (getWebNodesAndViewIdsWV, getWebNodesAndViewIdsWd, recursive) v []
 where getWebNodesAndViewIdsWV :: [(ViewId, WebNode db)] -> WebView db -> (WebView db, [(ViewId, WebNode db)])
       getWebNodesAndViewIdsWV state wv@(WebView vi _ _ _ _) = (wv, (vi, WebViewNode wv):state)

       getWebNodesAndViewIdsWd :: Data (w db) => [(ViewId, WebNode db)] -> Widget (w db) -> (Widget (w db), [(ViewId, WebNode db)])
       getWebNodesAndViewIdsWd state wd = (wd, widgetNode wd ++ state) 
         where widgetNode :: (Typeable (w db), Typeable db) => Widget (w db) -> [(ViewId, WebNode db)]
               widgetNode = 
                 [] `mkQ`  (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ LabelWidget w)])
                    `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ TextWidget w)])
                    `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ RadioViewWidget w)])
                    `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ SelectViewWidget w)])
                    `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ ButtonWidget w)])
                    `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ JSVarWidget w)])
-- todo: when we do this directly (without generics), get rid of the Data and Typeable contexts that were introduced
-- with this getTopLevelWebNodesWebViewAlt
      
