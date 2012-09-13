{-# LANGUAGE FlexibleContexts #-}
module Generics (module Generics, module GenericsSYB, module GenericsMap) where

import GenericsSYB ( 
                  getAllIds
                , clearIds
                , assignIdsFromList
                , getTopLevelWebNodesWebView
                , mkWebNodeMap


                , substituteIds
                , getTopLevelWebViews
                , getTopLevelWebNodesWebNode
                , mkViewMap
                , getAnyWidgetById
                , getButtonByViewId
                , getWebViewById
                , replaceWebViewById
                , getTextByViewIdRef
                , getTextByViewId
                , getLabelViewByViewId
                , getJSVarByViewId
                , getEditActionByViewId
                , lookupOldView
                , replace
                )
import GenericsMap ( 
--                  getAllIds
--                , clearIds
--                , assignIdsFromList
--               , getTopLevelWebNodesWebView
--                , mkWebNodeMap
                )
import Data.Generics

import Types
import ObloUtils
import Debug.Trace

-- clear all ids in webView and assign unique ones with respect to oldWebView
assignAllUniqueIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db -> WebView db
assignAllUniqueIds oldWebView webView =
  let clearedWebView = clearIds webView
      allIds = getAllIds oldWebView ++ getAllIds clearedWebView -- clearedWebView is necessary because assignIdz uses
      assigned = assignIdsFromList allIds clearedWebView          -- nr of ids to compute list of free ids
  in trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
     assigned
  
-- assign unique ids to all noIds in x
assignIds :: (Data db, MapWebView db (WebView db)) => WebView db -> WebView db
assignIds x = let allIds = getAllIds x
                  assigned = assignIdsFromList allIds x
              in trace (show $ filter (==Id (-1)) (getAllIds assigned)) $
                 assigned

