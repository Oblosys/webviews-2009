{-# OPTIONS -fglasgow-exts #-}
module Generics where

import Types

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet 

-- Generics


data Tree = Bin Tree Tree | SecondTree Tree2 | ThirdTree Tree3 | FourthTree Tree4 | Leaf Char |IdLeaf Id deriving (Show, Data, Typeable)

data Tree2 = FirstTree2 Tree  deriving (Show, Data, Typeable)
data Tree3 = FirstTree3 Tree  deriving (Show, Data, Typeable)
data Tree4 = FirstTree4 Tree  deriving (Show, Data, Typeable)

mytree = Bin (Bin (Leaf 'a') (Leaf 'b')) (Leaf 'c')

mkViewMap :: WebView -> ViewMap
mkViewMap wv = let wvs = getAll wv
                    in  Map.fromList [ (i, wv) | wv@(WebView i _ _ _ _) <- wvs]

-- webviews are to coarse for incrementality. We also need buttons, eints etc. as identifiable entities
-- making these into webviews does not seem to be okay. Maybe a separate class is needed

webNodeQ :: Typeable a => a -> Maybe WebNode
webNodeQ = (Nothing `mkQ`  (\w -> Just $ WebViewNode w) 
                    `extQ` (\w@(Widget vi stbid id x) -> Just $ WidgetNode vi stbid id (RadioViewWidget x))
                    `extQ` (\w@(Widget vi stbid id x) -> Just $ WidgetNode vi stbid id (TextWidget x))
                    `extQ` (\w@(Widget vi stbid id x) -> Just $ WidgetNode vi stbid id (ButtonWidget x))
           )              -- TODO can we do this bettter?
                        
                    
webNodeLstQ :: Typeable a => a -> Maybe [WebNode]
webNodeLstQ = (Nothing `mkQ`  (\ws -> Just [ WebViewNode w | w <- ws ]) 
 `extQ` (\ws -> Just $ map (\w@(Widget vi stbid id x) -> WidgetNode vi stbid id (RadioViewWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget vi stbid id x) -> WidgetNode vi stbid id (TextWidget x)) ws)
 `extQ` (\ws -> Just $ map (\w@(Widget vi stbid id x) -> WidgetNode vi stbid id (ButtonWidget x)) ws)
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

mkWebNodeMap :: Data d => d -> WebNodeMap
mkWebNodeMap x = Map.fromList $ everything (++) 
  ([] `mkQ`  (\w@(WebView vid sid id _ _) -> [(vid, WebViewNode w)]) 
      `extQ` (\(Widget vid sid id w) -> [(vid, WidgetNode vid sid id $ RadioViewWidget w)])
      `extQ` (\(Widget vid sid id w) -> [(vid, WidgetNode vid sid id $ TextWidget w)])
      `extQ` (\(Widget vid sid id w) -> [(vid, WidgetNode vid sid id $ ButtonWidget w)])
      `extQ` (\(Widget vid sid id w) -> [(vid, WidgetNode vid sid id $ EditActionWidget w)])
  ) x    

getTopLevelWebNodesWebView :: WebView -> [WebNode]
getTopLevelWebNodesWebView (WebView _ _ _ _ v) =
  everythingTopLevel webNodeQ v

-- make sure this one is not called on a WebView, but on its child view
-- TODO: rename this one, it is not called on a WebNode
getTopLevelWebNodesWebNode :: Data x => x -> [WebNode]
getTopLevelWebNodesWebNode x = everythingTopLevel 
                     webNodeQ x
                     
-- lookup the view id and if the associated view is of the desired type, return it. Otherwise return Nothing
lookupOldView :: (Initial v, Typeable v) => ViewId -> ViewMap -> Maybe v
lookupOldView vid viewMap = 
  case Map.lookup vid viewMap of
    Nothing              -> Nothing
    Just (WebView _ _ _ _ aView) -> cast aView

getAll :: (Data a, Data b) => a -> [b]
getAll = listify (const True)

getTopLevelWebViews :: Data a => a -> [WebView]
getTopLevelWebViews wv = everythingTopLevel (Nothing `mkQ` Just) wv
-- the type sig specifies the term type


-- is everythingBut, but not on the root (not used now)
everythingBelow :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingBelow f x = foldl (++) [] (gmapQ (everythingTopLevel f) x)

-- TODO: this name is wrong
-- get all non-nested descendents of a type
-- Eg. everythingBelow () (X (T_1 .. (T_2 .. (T_3 ..) ..)) (T_4)) = [T_1,T_2,T_4]
everythingTopLevel :: Data r => GenericQ (Maybe r) -> GenericQ [r]
everythingTopLevel f x = case f x of
                      Just x  -> [x]
                      Nothing -> foldl (++) [] (gmapQ (everythingTopLevel f) x)


-- all webviews and Web nodes are returned as a list with their corresponding path
-- lists of webviews or widgets do not get indices for the elements. Moreover, only direct lists of 
-- webviews or widgets are recognized. nested lists should be detected somehow and give an error.
getTopLevelWebNodesWebViewWithPath :: WebView -> [(WebNode,[Int])]
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




getWebViewContainingId :: Id -> WebView -> Maybe WebView
getWebViewContainingId (Id i) wv = 
  case somethingAcc Nothing (Nothing `mkQ` Just) (Nothing `mkQ` isId) wv of
    Just (_, Just wv') -> Just wv'
    Just (_, Nothing)  -> Nothing -- not called on a webview
    Nothing -> Nothing -- id not found
  where isId (Id i') = if i == i' then Just i' else Nothing


-- return internal id's but don't descend past Widgets or WebViews
webViewGetInternalIds :: WebView -> [Id]        
webViewGetInternalIds (WebView _ _ _ _ v) = 
  let --stopcond = (False `mkQ` (((\wv@(WebView _ _ _ _ _) -> True) :: WebView -> Bool))) 
      isId :: Id -> Bool
      isId _ = True
      stop :: GenericQ Bool
      stop = False `mkQ` isWebView `extQ` isWidget1 `extQ` isWidget2 `extQ` isWidget3
      isWebView :: WebView -> Bool -- TODO: aargh! just one is tricky with the type var in widget
      isWebView _ = True
      isWidget1 :: Widget RadioView -> Bool
      isWidget1 _ = True
      isWidget2 :: Widget Text -> Bool
      isWidget2 _ = True
      isWidget3 :: Widget Button -> Bool
      isWidget3 _ = True

  in  listifyBut isId stop v 

-- a GenericQ is easier in the callee code, but not as nice for the caller
listifyBut :: (Data a, Data x) => (a -> Bool) -> GenericQ Bool -> x -> [a]  
listifyBut stnh but x =
  case cast x of
    Just a  -> if stnh a then [a] else []
    Nothing -> if but x  
               then []
               else foldl (++) [] (gmapQ (listifyBut stnh but) x)

                    
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
everythingBut q f x = {- if q x then [x] else -} foldl (++) [] (gmapQ (everythingBut q f) x)
-}

-- number all Id's in the argument data structure uniquely
--assignIds :: Data d => d -> d

clearIds x = everywhere (mkT $ \(Id _) -> noId) x
assignIds x = assignIdz x
                  
assignIdz x = (snd $ everywhereAccum assignId freeIds x)
 where allIds = getAll x :: [Id]
       usedIds = IntSet.fromList $ map unId $ filter (/= noId) $ allIds 
       freeIds = (IntSet.fromList $ [0 .. length allIds - 1]) `IntSet.difference` usedIds
       
       
assignId :: Data d => IntSet -> d -> (IntSet,d)
assignId = mkAccT $ \ids (Id id) -> if (id == -1) 
                                    then if IntSet.null ids 
                                         then error "Internal error: assign Id, empty id list"
                                         else let (newId, ids') = IntSet.deleteFindMin ids 
                                              in  (ids', Id newId) 
                                    else (ids, Id id)


type Updates = Map Id String  -- maps id's to the string representation of the new value

-- update the datastructure at the id's in Updates 
replace :: Data d => Updates -> d -> d
replace updates v = (everywhere $ extT (mkT (replaceText updates))  (replaceRadioView updates)) v

replaceText :: Updates -> Text -> Text
replaceText updates x@(Text i h _ ea) =
  case Map.lookup i updates of
    Just str -> (Text i h str ea)
    Nothing -> x

replaceRadioView :: Updates -> RadioView -> RadioView
replaceRadioView updates x@(RadioView i is _ en) =
  case Map.lookup i updates of
    Just str -> (RadioView i is (read str) en)
    Nothing -> x

substituteIds :: Data x => [(Id, Id)] -> x -> x 
substituteIds subs wv =
  (everywhere $ mkT replaceId) wv
 where replaceId id@(Id i) = case lookup id subs of
                               Nothing -> id
                               Just id' -> id'
       
replaceWebViewById :: (ViewId) -> WebView -> WebView -> WebView
replaceWebViewById i wv rootView =
 (everywhere $ mkT replaceWebView) rootView
 where replaceWebView wv'@(WebView i' _ _ _ _) = if i == i' then wv else wv' 
--getWebViews x = listify (\(WebView i' :: WebView) -> True) x 

getButtonById :: Data d => Id -> d -> Button
getButtonById i view = 
  case listify (\(Button i' _ _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no button with id "++show i
    _   -> error $ "internal error: multiple buttons with id "++show i

getTextById :: Data d => Id -> d -> Text
getTextById i view = 
  case listify (\(Text i' _ _ _) -> i==i') view of
    [b] -> b
    []  -> error $ "internal error: no text with id "++show i
    _   -> error $ "internal error: multiple texts with id "++show i

getEditActionById :: Data d => Id -> d -> EditAction
getEditActionById i view = 
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
    []  -> error $ "internal error: no button with id "
    _   -> error $ "internal error: multiple buttons with id "

getTextByViewId :: Data v => ViewIdRef -> v -> Maybe String
getTextByViewId (ViewIdRef i) view =
  something (Nothing `mkQ` (\(Widget (ViewId i') _ _ (Text _ _ str _)) -> 
                             if i == i' then Just str else Nothing)) view
  



