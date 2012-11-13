{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
module Generics (module Generics, module GenericsMap) where

import GenericsMap (
                  getAllIds
                , clearIds
                , assignIdsFromList
                , getTopLevelWebNodes
                , getTopLevelWebViews
                , mkWebNodeMap
                , mkViewMap
  
                , getWebViewById
                , getAnyWidgetById  
                , mGetAnyWidgetById

                , getLabelViewByViewId
                , getTextViewByViewId
                , getRadioViewByViewId
                , getSelectViewByViewId
                , getButtonByViewId
                , getJSVarByViewId
                , getEditActionByViewId

                , updateViewById
                , substituteIds
                , applyUpdates
                , Updates
                )

import Data.Generics

import Types
import ObloUtils
import Debug.Trace
import qualified Data.Map as Map

-- clear all ids in webView and assign unique ones with respect to oldWebView
assignAllUniqueIds :: (IsView db v1, IsView db v2) => WebView db v1 -> WebView db v2 -> WebView db v2
assignAllUniqueIds oldWebView webView =
  let clearedWebView = clearIds webView
      allIds = getAllIds oldWebView ++ getAllIds clearedWebView -- clearedWebView is necessary because assignIdz uses
      assigned = assignIdsFromList allIds clearedWebView          -- nr of ids to compute list of free ids
  in --trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
     assigned
  
-- assign unique ids to all noIds in x
assignIds :: (MapWebView db (WebView db v), IsView db v) => WebView db v -> WebView db v
assignIds x = let allIds = getAllIds x
                  assigned = assignIdsFromList allIds x
              in --trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
                 assigned

lookupOldView :: (Initial v, Typeable v) => ViewId -> ViewMap db -> Maybe v
lookupOldView vid viewMap = 
  case Map.lookup vid viewMap of
    Nothing                                       -> Nothing
    Just (UntypedWebView (WebView _ _ _ _ aView)) -> cast aView


-- return al list of all WebNodes in rootView            
getBreadthFirstWebNodes :: IsView db v => WebView db v -> [WebNode db]
getBreadthFirstWebNodes rootView = -- todo: why not do getWebNodes with recursion? Do we need breadth-first here?
  concat $ takeWhile (not . null) $ iterate (concatMap getTopLevelWebNodesWebNode) 
                                       [WebViewNode $ UntypedWebView rootView]
 where getTopLevelWebNodesWebNode (WebViewNode (UntypedWebView wv)) = getTopLevelWebNodes wv
       getTopLevelWebNodesWebNode _ = []


getTextViewStrByViewIdRef :: forall db v . IsView db v => ViewIdRef -> WebView db v -> String
getTextViewStrByViewIdRef (ViewIdRef i) wv = getTextStrVal $ (getTextViewByViewId (ViewId i) wv :: TextView db)

getLabelStrByViewIdRef :: forall db v . IsView db v => ViewIdRef -> WebView db v -> String
getLabelStrByViewIdRef (ViewIdRef i) view =
  let (LabelView _ str _) :: LabelView db = getLabelViewByViewId (ViewId i) view
  in  str
    
getJSVarValueByViewIdRef :: forall db v . IsView db v => ViewIdRef -> WebView db v -> String
getJSVarValueByViewIdRef (ViewIdRef i) view =
  let (JSVar _ _ value) :: JSVar db = getJSVarByViewId (ViewId i) view
  in  value

                        
