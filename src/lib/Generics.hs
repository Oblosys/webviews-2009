{-# LANGUAGE FlexibleContexts #-}
module Generics (module Generics, module GenericsSYB, module GenericsMap) where

import GenericsSYB ( replace
                , replaceWebViewById
                
--                ,  getAllIds
--                , clearIds

                , substituteIds

--                , assignIdsFromList
--                , getTopLevelWebNodes
--                , getTopLevelWebViews
--                , mkWebNodeMap
--                , mkViewMap
                

--                , getWebViewById
--                , getAnyWidgetById
                
                , getButtonByViewId
                , getTextViewByViewId
                , getLabelViewByViewId
                , getJSVarByViewId

                , getEditActionByViewId
                , lookupOldView
                )
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
{-
                , getLabelViewByViewId
                , getTextViewByViewId
                , getRadioViewByViewId
                , getSelectViewByViewId
                , getButtonByViewId
                , getJSVarByViewId
-}
--                , getEditActionByViewId
                )

import Data.Generics

import Types
import ObloUtils
import Debug.Trace

-- TODO: the Data constraints can probably be relaxed to typeable when only Map is used.

-- clear all ids in webView and assign unique ones with respect to oldWebView
assignAllUniqueIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db -> WebView db
assignAllUniqueIds oldWebView webView =
  let clearedWebView = clearIds webView
      allIds = getAllIds oldWebView ++ getAllIds clearedWebView -- clearedWebView is necessary because assignIdz uses
      assigned = assignIdsFromList allIds clearedWebView          -- nr of ids to compute list of free ids
  in --trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
     assigned
  
-- assign unique ids to all noIds in x
assignIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db
assignIds x = let allIds = getAllIds x
                  assigned = assignIdsFromList allIds x
              in --trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
                 assigned

-- return al list of all WebNodes in rootView            
getBreadthFirstWebNodes :: Data db => WebView db -> [WebNode db]
getBreadthFirstWebNodes rootView = -- todo: why not do getWebNodes with recursion? Do we need breadth-first here?
  concat $ takeWhile (not . null) $ iterate (concatMap getTopLevelWebNodesWebNode) 
                                       [WebViewNode rootView]
 where getTopLevelWebNodesWebNode (WebViewNode wv) = getTopLevelWebNodes wv
       getTopLevelWebNodesWebNode _ = []

getTextViewStrByViewIdRef :: forall db . Data db => ViewIdRef -> WebView db -> String
getTextViewStrByViewIdRef (ViewIdRef i) wv = getStrVal' $ (getTextViewByViewId (ViewId i) wv :: TextView db)
