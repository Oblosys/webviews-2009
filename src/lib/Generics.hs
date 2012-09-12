{-# LANGUAGE ExistentialQuantification, RankNTypes, DeriveDataTypeable, ScopedTypeVariables, FlexibleContexts #-}
module Generics where

import Types
import ObloUtils

import Data.Generics
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet 
import Debug.Trace

-- Generics


data Tree = Bin Tree Tree | SecondTree Tree2 | ThirdTree Tree3 | FourthTree Tree4 | Leaf Char |IdLeaf Id deriving (Show, Data, Typeable)

data Tree2 = FirstTree2 Tree  deriving (Show, Data, Typeable)
data Tree3 = FirstTree3 Tree  deriving (Show, Data, Typeable)
data Tree4 = FirstTree4 Tree  deriving (Show, Data, Typeable)

mytree = Bin (Bin (Leaf 'a') (Leaf 'b')) (Leaf 'c')

mkViewMap :: Data db => WebView db -> ViewMap db
mkViewMap wv = let wvs = getAll wv
                    in  Map.fromList [ (i, wv) | wv@(WebView i _ _ _ _) <- wvs]

webNodeQ :: (Typeable db, Typeable a) => a -> Maybe (WebNode db)
webNodeQ = (Nothing `mkQ`  (\w -> Just $ WebViewNode w) 
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (LabelWidget x))
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (TextWidget x))
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (RadioViewWidget x))
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (SelectViewWidget x))
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (ButtonWidget x))
                    `extQ` (\w@(Widget stbid id x) -> Just $ let vi = getViewId x in WidgetNode vi stbid id (JSVarWidget x))
           )              -- TODO can we do this bettter?
                        
                    
webNodeLstQ :: (Typeable db, Typeable a) => a -> Maybe [WebNode db]
webNodeLstQ = (Nothing `mkQ`  (\ws -> Just [ WebViewNode w | w <- ws ]) 
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (LabelWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (TextWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (RadioViewWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (SelectViewWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (ButtonWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget stbid id x) -> let vi = getViewId x in WidgetNode vi stbid id (JSVarWidget x)) ws)
           )              -- TODO can we do this bettter?


testQ :: Typeable a => a -> Maybe (Either () (Either Int Bool))
testQ = (Nothing `mkQ` (\x -> Just $ Left x)
                 `extQ` (\x -> Just $ Right (Left x))
                 `extQ` (\x -> Just $ Right (Right x))
        )

testLstQ :: Typeable a => a -> Maybe [(Either () (Either Int Bool))]
testLstQ = (Nothing `mkQ`  (\xs -> Just $ [ Left x | x <- xs ])
                    `extQ` (\xs -> Just $ [ Right (Left x) | x <- xs ])
                    `extQ` (\xs -> Just $ [ Right (Right x) | x <- xs ])
        )

mkWebNodeMap :: (Typeable db, Data d) => d -> WebNodeMap db
mkWebNodeMap x = mkWebNodeMapAlt x {- Map.fromList $ everything (++) 
  ([] `mkQ`  (\w@(WebView vid sid id _ _) -> [(vid, WebViewNode w)]) 
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ LabelWidget w)])
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ TextWidget w)])
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ RadioViewWidget w)])
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ SelectViewWidget w)])
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ ButtonWidget w)])
      `extQ` (\(Widget sid id w) -> let vid = getViewId w in [(vid, WidgetNode vid sid id $ JSVarWidget w)])
  ) x    
-}

-- TODO: maybe call these getChildWebNodes..?
getTopLevelWebNodesWebView :: Data db => WebView db -> [WebNode db]
getTopLevelWebNodesWebView wv@(WebView _ _ _ _ v) =
  getTopLevelWebNodesWebViewAlt v
  --everythingTopLevel webNodeQ v



-- make sure this one is not called on a WebView, but on its child view
-- TODO: rename this one, it is not called on a WebNode
getTopLevelWebNodesWebNode :: (Data db, Data x, MapWebView db x) => x -> [WebNode db]
getTopLevelWebNodesWebNode x = 
  getTopLevelWebNodesWebViewAlt x
  --everythingTopLevel webNodeQ x
                     
-- lookup the view id and if the associated view is of the desired type, return it. Otherwise return Nothing
lookupOldView :: (Initial v, Typeable v) => ViewId -> ViewMap db -> Maybe v
lookupOldView vid viewMap = 
  case Map.lookup vid viewMap of
    Nothing              -> Nothing
    Just (WebView _ _ _ _ aView) -> cast aView

getAll :: (Data a, Data b) => a -> [b]
getAll = listify (const True)

getTopLevelWebViews :: (Data db, Data a) => a -> [WebView db]
getTopLevelWebViews wv = everythingTopLevel (Nothing `mkQ` Just) wv
-- the type sig specifies the term type


-- is everythingBut, but not on the root (not used now)
everythingBelow :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingBelow f x = concat $ gmapQ (everythingTopLevel f) x

-- TODO: this name is wrong
-- get all non-nested descendents of a type
-- Eg. everythingTopLevel  (X T_1 .. (T_2 .. (T_3 ..) ..) .. (Just T_4)) = [T_1,T_2,T_4]
everythingTopLevel :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingTopLevel f x = case f x of
                      Just x  -> [x]
                      Nothing -> concat $ gmapQ (everythingTopLevel f) x


-- all webviews and Web nodes are returned as a list with their corresponding path
-- lists of webviews or widgets do not get indices for the elements. Moreover, only direct lists of 
-- webviews or widgets are recognized. nested lists should be detected somehow and give an error.
getTopLevelWebNodesWebViewWithPath :: Data db => WebView db -> [(WebNode db,[Int])]
getTopLevelWebNodesWebViewWithPath (WebView _ _ _ _ v) =
  everythingTopLevelWithPath [] webNodeLstQ webNodeQ v


everythingTopLevelWithPath :: Data r => [Int] -> GenericQ (Maybe [r]) -> GenericQ (Maybe r) -> GenericQ [(r,[Int])]
everythingTopLevelWithPath pth flst f x = 
  case flst x of
    Just lsts -> [ (lst,pth++[-2]) | lst <- lsts ]
    Nothing -> case f x of Just x  -> [(x,pth)]
                           Nothing -> concat $ gmapQIx (\i -> everythingTopLevelWithPath (pth++[i]) flst f) x
                               
 -- where isList = undefined :: Data a => [a] -> ()

--- tricky stuff:

-- | The type constructor used in definition of gmapQr
data Qr r a = Qr Int (r -> r)

gmapQIx :: Data a => (forall d. Data d => Int -> d -> r') -> a -> [(r')]
gmapQIx f x0 = case (gfoldl k (const (Qr 0 id)) x0) of Qr _ res -> res []
    where
      k (Qr i c) x = Qr  (i+1) (\r -> c (f i x : r))




getWebViewContainingId :: Data db => Id -> WebView db -> Maybe (WebView db)
getWebViewContainingId (Id i) wv = 
  case somethingAcc Nothing (Nothing `mkQ` Just) (Nothing `mkQ` isId) wv of
    Just (_, Just wv') -> Just wv'
    Just (_, Nothing)  -> Nothing -- not called on a webview
    Nothing -> Nothing -- id not found
  where isId (Id i') = if i == i' then Just i' else Nothing


-- return internal id's but don't descend past Widgets or WebViews
webViewGetInternalIds :: forall db . Data db => WebView db -> [Id]        
webViewGetInternalIds (WebView _ _ _ _ v) = 
  let --stopcond = (False `mkQ` (((\wv@(WebView _ _ _ _ _) -> True) :: WebView -> Bool))) 
      isId :: Id -> Bool
      isId _ = True
      stop :: GenericQ Bool
      stop = False `mkQ` isWebView `extQ` isWidget1 `extQ` isWidget2 `extQ` isWidget3 `extQ` isWidget4 `extQ` isWidget5 `extQ` isWidget6
      isWebView :: WebView  db -> Bool -- TODO: aargh! just one is tricky with the type var in widget
      isWebView _ = True
      isWidget1 :: Widget (LabelView db) -> Bool
      isWidget1 _ = True
      isWidget2 :: Widget (TextView db) -> Bool
      isWidget2 _ = True
      isWidget3 :: Widget (RadioView db) -> Bool
      isWidget3 _ = True
      isWidget4 :: Widget (SelectView db) -> Bool
      isWidget4 _ = True
      isWidget5 :: Widget (Button db) -> Bool
      isWidget5 _ = True
      isWidget6 :: Widget (JSVar db) -> Bool
      isWidget6 _ = True
  in  listifyBut isId stop v 

-- a GenericQ is easier in the callee code, but not as nice for the caller
listifyBut :: (Data a, Data x) => (a -> Bool) -> GenericQ Bool -> x -> [a]  
listifyBut stnh but x =
  case cast x of
    Just a  -> if stnh a then [a] else []
    Nothing -> if but x  
               then []
               else concat $ gmapQ (listifyBut stnh but) x

                    
somethingAcc :: (Data a, Data r) => a -> GenericQ (Maybe a) -> GenericQ (Maybe r) -> GenericQ (Maybe (r,a))
somethingAcc a fa f x = let a' = case fa x of 
                                   Nothing -> a
                                   Just a' -> a'
                        in  case f x of
                              Just x  -> Just (x,a')
                              Nothing -> foldl orElse Nothing (gmapQ (somethingAcc a' fa f) x)

{-
everythingBut :: Data a 
 => GenericQ Bool
 -> (forall a. Data a => a -> r) 
 -> a -> r 
everythingBut q f x = {- if q x then [x] else -} concat $ gmapQ (everythingBut q f) x
-}

-- number all Id's in the argument data structure uniquely
--assignIds :: Data d => d -> d

clearIds x = everywhere (mkT $ \(Id _) -> noId) x

-- clear all ids in webView and assign unique ones with respect to oldWebView
assignAllUniqueIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db -> WebView db
assignAllUniqueIds oldWebView webView =
  let clearedWebView = clearIds webView
      allIds = getAll (oldWebView,clearedWebView) :: [Id] -- clearedWebView is necessary because assignIdz uses
      assigned = assignIdzAlt allIds clearedWebView          -- nr of ids to compute list of free ids
  in trace (show $ filter (==Id (-1)) (getAll assigned :: [Id])) $
     assigned
  
-- assign unique ids to all noIds in x
assignIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db
assignIds x = let allIds = getAll x :: [Id]
                  assigned = assignIdzAlt allIds x
              in trace (show $ filter (==Id (-1)) (getAll assigned :: [Id])) $
                 assigned

                 
assignIdz :: (Data db, MapWebView db (WebView db)) => [Id] -> WebView db -> WebView db  
assignIdz allIds x = (snd $ everywhereAccum assignId freeIds x)
 where usedIds = IntSet.fromList $ map unId $ filter (/= noId) $ allIds 
       freeIds = (IntSet.fromList $ [0 .. length allIds - 1]) `IntSet.difference` usedIds
       
       
assignId :: Data d => IntSet -> d -> (IntSet,d)
assignId = mkAccT $ \ids (Id id) -> if (id == -1) 
                                    then if IntSet.null ids 
                                         then error "Internal error: assign Id, empty id list"
                                         else let (newId, ids') = IntSet.deleteFindMin ids 
                                              in  (ids', Id newId) 
                                    else (ids, Id id)


type Updates = Map ViewId String  -- maps id's to the string representation of the new value

-- update the datastructure at the id's in Updates 
-- TODO is dummy db arg necessary? with ScopedTypeVariables we can prevent it, but maybe that leads to big type sigs
replace :: forall db d . (Typeable db, Data d) => db -> Updates -> d -> d
replace _ updates v = (everywhere $  mkT    (replaceText updates :: TextView db -> TextView db)
                                     `extT` (replaceRadioView updates :: RadioView db -> RadioView db)
                                     `extT` (replaceSelectView updates :: SelectView db -> SelectView db)
                                     `extT` (replaceJSVar updates :: JSVar db -> JSVar db)) v

replaceJSVar :: Updates -> JSVar db -> JSVar db
replaceJSVar updates x@(JSVar i nm _) =
  case Map.lookup i updates of
    Just str -> (JSVar i nm str)
    Nothing -> x

replaceText :: Updates -> TextView db -> TextView db
replaceText updates x@(TextView i h _ st ba ea) =
  case Map.lookup i updates of
    Just str -> (TextView i h str st ba ea)
    Nothing -> x

replaceRadioView :: Updates -> (RadioView db) -> (RadioView db)
replaceRadioView updates x@(RadioView i is _ en st ch) =
  case Map.lookup i updates of
    Just str -> (RadioView i is (unsafeRead ("Generics.replaceRadioView on "++show i) str) en st ch)
    Nothing -> x

replaceSelectView :: Updates -> (SelectView db) -> (SelectView db)
replaceSelectView updates x@(SelectView i is _ en st ch) =
  case Map.lookup i updates of
    Just str -> (SelectView i is (unsafeRead ("Generics.replaceSelectView on "++show i) str) en st ch)
    Nothing -> x

substituteIds :: Data x => [(Id, Id)] -> x -> x 
substituteIds subs wv =
  (everywhere $ mkT replaceId) wv
 where replaceId id@(Id i) = case lookup id subs of
                               Nothing -> id
                               Just id' -> id'
       
replaceWebViewById :: Data db => (ViewId) -> WebView db -> WebView db -> WebView db
replaceWebViewById i wv rootView =
 (everywhere $ mkT replaceWebView) rootView
 where replaceWebView wv'@(WebView i' _ _ _ _) = if i == i' then wv else wv' 
--getWebViews x = listify (\(WebView i' :: WebView) -> True) x 

getButtonByViewId :: (Typeable db, Data d) => ViewId -> d -> Maybe (Button db)
getButtonByViewId i view = 
  case listify (\(Button i' _ _ _ _ _) -> i==i') view of
    [b] -> Just b
    []  -> error $ "internal error: no button with id "++show i
    _   -> error $ "internal error: multiple buttons with id "++show i

getLabelViewByViewId :: Typeable db => Data d => ViewId -> d -> LabelView db
getLabelViewByViewId i view = 
  case listify (\(LabelView i' _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no label with id "++show i
    _   -> error $ "internal error: multiple LabelViews with id "++show i

getMTextByViewId :: (Typeable db, Data d) => ViewId -> d -> Maybe (TextView db)
getMTextByViewId i view = 
  case listify (\(TextView i' _ _ _ _ _) -> i==i') view of
    [b] -> Just b
    []  -> Nothing
    _   -> error $ "internal error: multiple texts with id "++show i


getTextByViewId :: (Typeable db, Data d) => ViewId -> d -> TextView db
getTextByViewId i view = fromMaybe (error $ "internal error: no text with id "++show i) $ getMTextByViewId i view  

getJSVarByViewId :: (Typeable db, Data d) => ViewId -> d -> JSVar db
getJSVarByViewId i view = 
  case listify (\(JSVar i' _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no JSVar with id "++show i
    _   -> error $ "internal error: multiple JSVars with id "++show i

getEditActionByViewId :: (Typeable db, Data d) => ViewId -> d -> EditAction db
getEditActionByViewId i view = 
  case listify (\(EditAction i' _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no edit action with id "++show i
    _   -> error $ "internal error: multiple edit actions with id "++show i

--

everywhereAccum :: Data d => (forall b . Data b => a -> b -> (a,b)) -> a -> d -> (a,d)
everywhereAccum f acc a =
 let (acc'',r) = gfoldl k z a
 in  f acc'' r  
 where k (acc',a2b) a = let (acc'',a') = everywhereAccum f acc' a
                            b = a2b a'
                        in  (acc'', b)
       z e = (acc, e)

gReplace :: forall a d . (Typeable a, Data d) => [a] -> d -> ([a],d)
gReplace = mkAccT $ \(i:is) _ -> (is,i)

number :: Data d => Int -> d -> (Int,d)
number  = mkAccT $ \acc _ -> (acc+1, acc)

mkAccT :: forall a b acc . (Typeable acc, Typeable a, Data b) => (acc -> a -> (acc,a)) -> (acc -> b -> (acc,b))
mkAccT f = case cast f  of -- can we do this without requiring Typeable acc?
                  Just g  -> g 
                  Nothing -> \acc b -> (acc,b)


getWebViewById i view = 
  case listify (\(WebView i' _ _ _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: getWebViewById: no webview with id " ++ show i
    _   -> error $ "internal error: getWebViewById: multiple webviews with id " ++ show i

getAnyWidgetById :: forall db d . (Typeable db, Data d) => ViewId -> d -> AnyWidget db
getAnyWidgetById i x = 
  case everything (++) 
         ([] `mkQ`  (\(Widget sid id w) -> if i == getViewId w then [LabelWidget w] else [])
             `extQ` (\(Widget sid id w) -> if i == getViewId w then [TextWidget w] else [])
             `extQ` (\(Widget sid id w) -> if i == getViewId w then [RadioViewWidget w] else [])
             `extQ` (\(Widget sid id w) -> if i == getViewId w then [SelectViewWidget w] else [])
             `extQ` (\(Widget sid id w) -> if i == getViewId w then [ButtonWidget w] else [])
             `extQ` (\(Widget sid id w) -> if i == getViewId w then [JSVarWidget w] else [])
         ) x of
    [w] -> w
    []  -> error $ "internal error: getAnyWidgetById: no widget with id "
    _   -> error $ "internal error: getAnyWidgetById: multiple webviews with id "
     
     
  
getTextByViewIdRef :: forall db v . (Typeable db, Data v) => db -> ViewIdRef -> v -> String
getTextByViewIdRef _ (ViewIdRef i) view =
  let (TextView _ _ str _ _ _) :: TextView db = getTextByViewId (ViewId i) view
  in  str

 
assignIdzAlt :: forall db . (Data db) => [Id] -> WebView db -> WebView db
assignIdzAlt allIds rootView =
  let assigned = fst $ mapWebView (assignIdsWV, assignIdsW, True)
               rootView freeIds
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

getTopLevelWebNodesWebViewAlt :: forall db v . (Typeable db, MapWebView db v) => v -> [WebNode db]
getTopLevelWebNodesWebViewAlt v = map snd $ getWebNodesAndViewIds False v

mkWebNodeMapAlt :: (Typeable db) => WebView db -> WebNodeMap db
mkWebNodeMapAlt wv = Map.fromList $ getWebNodesAndViewIds True wv


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
      
