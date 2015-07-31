module GenericsMap where

import Types
import ObloUtils

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet 
import qualified Data.IntMap as IntMap 
import Debug.Trace
import Data.Generics (Typeable, cast)


getAllIds :: forall db v . IsView db v => WebView db v -> [Id]
getAllIds rootView = snd $ runMapWebView rootView (getAllIdsWV, getAllIdsWd, noWidgetUpdates, True) []
  where getAllIdsWV :: WebView db v' -> [Id] -> (WebView db v', [Id])
        getAllIdsWV wv@(WebView _ sid id _ _) ids = (wv, sid:id:ids)
        getAllIdsWd wd@(Widget sid id _)      ids = (wd, sid:id:ids)

clearIds :: forall db v . IsView db v => WebView db v -> WebView db v
clearIds rootView = fst $ runMapWebView rootView (clearIdsWV, clearIdsWd, noWidgetUpdates, True) () 
  where clearIdsWV :: WebView db v' -> () -> (WebView db v', ())
        clearIdsWV wv@(WebView vid _ _ mkF v) _ = (WebView vid noId noId mkF v, ())
        clearIdsWd wd@(Widget _ _ w)          _ = (Widget noId noId w, ())

assignIdsFromList :: forall db v . IsView db v => [Id] -> WebView db v -> WebView db v
assignIdsFromList allIds rootView =
  let assigned = fst $ runMapWebView rootView (assignIdsWV, assignIdsWd, noWidgetUpdates, True) freeIds
  in {- trace (if [] /= filter (==Id (-1))(getAll assigned :: [Id]) then show assigned else "ok") $ -} assigned
 where usedIds = IntSet.fromList $ map unId $ filter (/= noId) $ allIds 
       freeIds = (IntSet.fromList $ [0 .. length allIds - 1]) `IntSet.difference` usedIds

       assignIdsWV :: WebView db v' -> IntSet -> (WebView db v', IntSet)
       assignIdsWV (WebView vi sid id mkF v) state = (WebView vi sid' id' mkF v, state'')
        where (sid',state') = mkId state sid
              (id',state'') = mkId state' id
       
       assignIdsWd (Widget sid id w) state = (Widget sid' id' w, state'')
        where (sid',state') = mkId state sid
              (id',state'') = mkId state' id

mkId ids (Id i) = if (i == -1) 
                         then if IntSet.null ids 
                                         then error "Internal error: assign Id, empty id list"
                                         else let (newId, ids') = IntSet.deleteFindMin ids 
                                              in  (Id newId, ids') 
                                    else (Id i, ids)       

getTopLevelWebViews :: MapWebView db v => WebView db v -> [UntypedWebView db]
getTopLevelWebViews wv = [w | WebViewNode w <- getTopLevelWebNodes wv]

-- get top-level WebNodes (WebViews and widgets), not including the WebView itself
getTopLevelWebNodes :: MapWebView db v => WebView db v -> [WebNode db]
getTopLevelWebNodes (WebView _ _ _ _ v) = map snd $ getWebNodesAndViewIds False v

mkWebNodeMap :: IsView db v => WebView db v -> WebNodeMap db
mkWebNodeMap wv = Map.fromList $ getWebNodesAndViewIds True wv

mkViewMap ::IsView db v => WebView db v -> ViewMap db
mkViewMap wv = Map.fromList $ [ (vid, wv) | (vid, WebViewNode wv) <- getWebNodesAndViewIds True wv ]

getLabelViewByViewId :: IsView db v => ViewId -> WebView db v -> LabelView db
getLabelViewByViewId i wv =
  case getAnyWidgetById i wv of
    LabelWidget x -> x
    _             -> error $ "internal error: widget with id " ++ show i ++ " is not a LabelView" 

getTextViewByViewId :: IsView db v => ViewId -> WebView db v -> TextView db
getTextViewByViewId i wv =
  case getAnyWidgetById i wv of
    TextWidget x -> x
    _              -> error $ "internal error: widget with id " ++ show i ++ " is not a TextView" 

getRadioViewByViewId :: IsView db v => ViewId -> WebView db v -> RadioView db
getRadioViewByViewId i wv =
  case getAnyWidgetById i wv of
    RadioViewWidget x -> x
    _                 -> error $ "internal error: widget with id " ++ show i ++ " is not a RadioView" 

getSelectViewByViewId :: IsView db v => ViewId -> WebView db v -> SelectView db
getSelectViewByViewId i wv =
  case getAnyWidgetById i wv of
    SelectViewWidget x -> x
    _                  -> error $ "internal error: widget with id " ++ show i ++ " is not a SelectView" 


getButtonByViewId :: IsView db v => ViewId -> WebView db v -> Button db
getButtonByViewId i wv =
  case getAnyWidgetById i wv of
    ButtonWidget x -> x
    _              -> error $ "internal error: widget with id " ++ show i ++ " is not a Button" 

getJSVarByViewId :: IsView db v => ViewId -> WebView db v -> JSVar db
getJSVarByViewId i wv =
  case getAnyWidgetById i wv of
    JSVarWidget x -> x
    _             -> error $ "internal error: widget with id " ++ show i ++ " is not a JSVar" 

getEditActionByViewId :: IsView db v => ViewId -> WebView db v -> EditAction db
getEditActionByViewId i wv =
  case getAnyWidgetById i wv of
    EditActionWidget x -> x
    _             -> error $ "internal error: widget with id " ++ show i ++ " is not an EditAction" 

getWebViewById :: IsView db v => ViewId -> WebView db v -> UntypedWebView db
getWebViewById i wv = 
  case getWebNodeById "getWebViewById" i wv of
    (WebViewNode wv) -> wv
    _                -> error $ "internal error: webnode with id " ++ show i ++ " is not a WebViewNode"


getAnyWidgetById :: IsView db v => ViewId -> WebView db v -> AnyWidget db
getAnyWidgetById i wv = fromMaybe (error $ "internal error: webnode with id " ++ show i ++ " is not a WidgetNode") $
                          mGetAnyWidgetById i wv

getWebNodeById :: IsView db v => String -> ViewId -> WebView db v -> WebNode db
getWebNodeById callerTag i wv = 
  case [ wn | (vid, wn) <- getWebNodesAndViewIds True wv, vid == i ] of
    [b] -> b
    []  -> error $ "internal error: getWebNodeById (called by "++callerTag++"): no webnode with id " ++ show i
    _   -> error $ "internal error: getWebNodeById (called by "++callerTag++"): multiple webnode with id " ++ show i

-- TODO: workaround for problem with events that arrive after the target widget has been removed. (especially on iPad/iPhone)
mGetAnyWidgetById :: IsView db v => ViewId -> WebView db v -> Maybe (AnyWidget db)
mGetAnyWidgetById i wv = 
  case mGetWebNodeById "getAnyWidgetById" i wv of
    Just (WidgetNode _ _ _ wd) -> Just wd
    _                          -> Nothing

mGetWebNodeById :: IsView db v => String -> ViewId -> WebView db v -> Maybe (WebNode db)
mGetWebNodeById callerTag i wv = 
  case [ wn | (vid, wn) <- getWebNodesAndViewIds True wv, vid == i ] of
    [b] -> Just b
    []  -> Nothing
    _   -> Nothing

getWebNodesAndViewIds :: forall db v . MapWebView db v => Bool -> v -> [(ViewId, WebNode db)]
getWebNodesAndViewIds recursive v = snd $ runMapWebView v (getWebNodesAndViewIdsWV, getWebNodesAndViewIdsWd, noWidgetUpdates, recursive) []
 where getWebNodesAndViewIdsWV :: IsView db v' =>
                                  WebView db v' -> [(ViewId, WebNode db)] -> (WebView db v', [(ViewId, WebNode db)])
       getWebNodesAndViewIdsWV wv@(WebView vi _ _ _ _) state = (wv, (vi, WebViewNode $ UntypedWebView wv):state)

       getWebNodesAndViewIdsWd :: MapWebView db (w db) => Widget (w db) -> [(ViewId, WebNode db)] -> (Widget (w db), [(ViewId, WebNode db)])
       getWebNodesAndViewIdsWd wd@(Widget sid id w) state = (wd, widgetNode ++ state) 
         where widgetNode :: [(ViewId, WebNode db)]
               widgetNode = case widgetToAnyWidget w of
                              Nothing       -> error "Generics.getWebNodesAndViewIds Widget with non-widget child."
                              Just (vid,a)  -> [(vid, WidgetNode vid sid id a)]

widgetToAnyWidget :: MapWebView db w => w -> (Maybe (ViewId,AnyWidget db))
widgetToAnyWidget w = snd $ runMapWebView w (inert,inert,widgetUpdates,False {- has no effect -}) Nothing 
 where widgetUpdates :: WidgetUpdates db (Maybe (ViewId, AnyWidget db))
       widgetUpdates = WidgetUpdates labelViewUpd textViewUpd radioViewUpd selectViewUpd buttonUpd jsVarUpd editActionUpd
                                     
       labelViewUpd w s = (w, Just (getViewId w, LabelWidget w))
       textViewUpd w s  = (w, Just (getViewId w, TextWidget w))
       radioViewUpd w s  = (w, Just (getViewId w, RadioViewWidget w))
       selectViewUpd w s = (w, Just (getViewId w, SelectViewWidget w))
       buttonUpd w s     = (w, Just (getViewId w, ButtonWidget w))
       jsVarUpd w s      = (w, Just (getViewId w, JSVarWidget w))
       editActionUpd w s = (w, Just (getViewId w, EditActionWidget w))

updateViewById :: forall db v view . (IsView db v, Typeable view) => ViewId -> (view->view) -> WebView db v -> WebView db v
updateViewById vid viewUpdate rootView = fst $ runMapWebView rootView (updateViewByIdWV, updateViewByIdWd, noWidgetUpdates, True) ()
 where updateViewByIdWV :: IsView db v' => WebView db v' -> () -> (WebView db v', ())
       updateViewByIdWV wv@(WebView vi si i mkV v) state = 
         ( if vid == vi
           then case cast viewUpdate of
                  Just f  -> WebView vid si i mkV $ f v 
                  Nothing -> error "replaceWebViewById: type error"
           else wv
         , state )

       updateViewByIdWd wd state = (wd, state)

substituteIds :: forall db v . IsView db v => [(Id, Id)] -> WebView db v -> WebView db v
substituteIds subs rootView = fst $ runMapWebView rootView (substituteIdsWV, substituteIdsWd, noWidgetUpdates, True) ()
 where substituteIdsWV :: WebView db v' -> () -> (WebView db v', ())
       substituteIdsWV (WebView vi sid id mkF v) state = (WebView vi (substituteId sid) (substituteId id) mkF v, state)
       
       substituteIdsWd (Widget sid id w) state = (Widget (substituteId sid) (substituteId id) w, state)
       
       subsMap = IntMap.fromList $ map (\(Id i1, Id i2) -> (i1,i2)) subs
       
       substituteId id@(Id i) = case IntMap.lookup i subsMap of
                                  Nothing  -> id
                                  Just i' -> Id i'

type Updates = Map ViewId String  -- maps id's to the string representation of the new value


-- TODO: take widgets out of the map. doesn't really add anything as they won't appear without a wrapping Widget
-- TODO: add db to Widget type? might make things easier. (done in rolled-back r3053, didn't seem to make things simpler) 
-- TODO: use anywidget type in widget? There doesn't seem to be a need for different types.
--       In that case, probably need a phantom type in Widget, as well as db.

-- update the datastructure at the id's in Updates 
applyUpdates :: forall db v . IsView db v => Updates -> WebView db v -> WebView db v
applyUpdates updates rootView = fst $ runMapWebView rootView (applyUpdatesWV, applyUpdatesWd, widgetUpdates, True) ()
 where applyUpdatesWV :: WebView db v' -> () -> (WebView db v', ())
       applyUpdatesWV wd state = (wd, state)
       applyUpdatesWd wd state = (wd, state)
       widgetUpdates = WidgetUpdates labelViewUpd textViewUpd radioViewUpd selectViewUpd buttonUpd jsVarUpd editActionUpd
                                     
       labelViewUpd      = inert
       textViewUpd w     = mkWidgetUpdate w (\v -> w{getTextStrVal=v})        id
       radioViewUpd w    = mkWidgetUpdate w (\v -> w{getRadioSelection=v})  $ unsafeRead ("Generics.replace.radioViewUpd at "++(show $ getViewId w))
       selectViewUpd w   = mkWidgetUpdate w (\v -> w{getSelectSelection=v}) $ unsafeRead ("Generics.replace.selectViewUpd at "++(show $ getViewId w))
       buttonUpd         = inert 
       jsVarUpd w        = mkWidgetUpdate w (\v -> w{getJSVarValue_=v})       id
       editActionUpd     = inert 

       mkWidgetUpdate :: HasViewId w => w -> (a -> w) -> (String -> a) -> s -> (w,s) 
       mkWidgetUpdate w upd parse s = case Map.lookup (getViewId w) updates of
                                        Nothing  -> (w,s)
                                        Just str -> (upd (parse str),s)
                                    