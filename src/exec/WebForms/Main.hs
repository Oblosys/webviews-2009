{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TupleSections, FlexibleInstances, ScopedTypeVariables, DoRec #-}
module Main where

import Data.List
import BlazeHtml
import Data.Generics
import Data.Char hiding (Space)
import Data.Function (on)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.Time
import Types
import ObloUtils
import Generics
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State hiding (get)
import qualified Control.Monad.State
import Server
import TemplateHaskell
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels

import Database
import WebFormUtils

type WebForm = [FormElt]

data FormElt = HtmlElt String
             | RadioAnswerElt RadioAnswer
             | ButtonAnswerElt ButtonAnswer
             | TextAnswerElt TextAnswer
             | TableElt [[FormElt]]
   deriving (Eq, Show, Typeable, Data)

data RadioAnswer = RadioAnswer [String]
   deriving (Eq, Show, Typeable, Data)

data ButtonAnswer = ButtonAnswer [String]
   deriving (Eq, Show, Typeable, Data)

data TextAnswer = TextAnswer
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormElt
instance MapWebView Database FormElt

deriveInitial ''RadioAnswer
instance MapWebView Database RadioAnswer

deriveInitial ''ButtonAnswer
instance MapWebView Database ButtonAnswer

deriveInitial ''TextAnswer
instance MapWebView Database TextAnswer

testForm = [ TableElt 
              [ [ HtmlElt "Leeftijd", TextAnswerElt TextAnswer]
              , [ HtmlElt "Geslacht", ButtonAnswerElt $ ButtonAnswer ["Man", "Vrouw"]]
              , [ HtmlElt "App is leuk",       RadioAnswerElt $ RadioAnswer ["Ja","Nee"]]
              ]
           , HtmlElt "<p>yo</p>"
           , RadioAnswerElt $ RadioAnswer ["Veel","Weinig"]
           ]


------- WebViews

data SelectableView = SelectableView Bool String (Widget (EditAction Database)) deriving (Eq, Show, Typeable, Data)

deriveInitial ''SelectableView
deriveMapWebViewDb ''Database ''SelectableView


mkSelectableView :: [ViewId] -> String -> WebViewM Database (WebView Database)
mkSelectableView allSelectableVids str = mkWebView $
  \vid (SelectableView selected _ _) ->
    do { clickAction <- mkEditAction $ Edit $ sequence_ [ viewEdit v $ \(SelectableView _ str ca) ->
                                                                           SelectableView (vid == v) str ca
                                                        | v <- allSelectableVids
                                                        ]
                                                  
       ; return $ SelectableView selected str clickAction
       }

instance Presentable SelectableView where
  present (SelectableView selected str clickAction) =
    let color = if selected then "#0f3" else "white"
    in  withEditAction clickAction $ with [thestyle $ "background-color: "++color] $ boxed $ toHtml str


instance Storeable Database SelectableView
  
mkSelectionViews :: [String] -> WebViewM Database [WebView Database]
mkSelectionViews strs =
 do { rec { wvs <- mapM (mkSelectableView vids) strs
          ; let vids = map getViewId wvs
          }
    ; return wvs
    }
     
data RadioAnswerView = RadioAnswerView RadioAnswer (Widget (RadioView Database))
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''RadioAnswerView
deriveMapWebViewDb ''Database ''RadioAnswerView

mkRadioAnswerView :: RadioAnswer -> WebViewM Database (WebView Database)
mkRadioAnswerView r@(RadioAnswer answers) = mkWebView $
  \vid (RadioAnswerView _ radioOld) ->
    do { radio <-  mkRadioView answers (getSelection radioOld) True 
    
       ; return $ RadioAnswerView r radio
       }

instance Presentable RadioAnswerView where
  present (RadioAnswerView _ radio) =
      present radio

instance Storeable Database RadioAnswerView

data ButtonAnswerView = ButtonAnswerView ButtonAnswer [WebView Database]
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''ButtonAnswerView
deriveMapWebViewDb ''Database ''ButtonAnswerView

mkButtonAnswerView :: ButtonAnswer -> WebViewM Database (WebView Database)
mkButtonAnswerView b@(ButtonAnswer answers) = mkWebView $
  \vid (ButtonAnswerView _ buttonsold) ->
    do { buttons <- mkSelectionViews answers  
    
       ; return $ ButtonAnswerView b buttons
       }

instance Presentable ButtonAnswerView where
  present (ButtonAnswerView _ buttons) =
      hList $ map present buttons

instance Storeable Database ButtonAnswerView


data TextAnswerView = TextAnswerView TextAnswer (Widget (TextView Database))
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''TextAnswerView
deriveMapWebViewDb ''Database ''TextAnswerView

mkTextAnswerView :: TextAnswer -> WebViewM Database (WebView Database)
mkTextAnswerView t@(TextAnswer) = mkWebView $
  \vid (TextAnswerView _ _) ->
    do { text <-  mkTextField "" 
    
       ; return $ TextAnswerView t text
       }

instance Presentable TextAnswerView where
  present (TextAnswerView _ radio) =
      present radio

instance Storeable Database TextAnswerView


data TableView = TableView [[WebView Database]]
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''TableView
deriveMapWebViewDb ''Database ''TableView

mkTableView :: [[WebView Database]] -> WebViewM Database (WebView Database)
mkTableView rows = mkWebView $
  \vid _ ->
    do { return $ TableView rows
       }

instance Presentable TableView where
  present (TableView rows) =
      mkTable [] [] [] $ map (map present) rows

instance Storeable Database TableView


data FormView = 
  FormView [WebView Database]
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormView

deriveMapWebViewDb ''Database ''FormView

mkFormView :: WebForm -> WebViewM Database (WebView Database)
mkFormView form = mkWebView $
  \vid _ ->
    do { selViews <- mkSelectionViews ["Ja","Nee", "Misschien"] 
       ; formElts <- mapM mkView form
       ; return $ FormView $ selViews ++ formElts
       }

instance Presentable FormView where
  present (FormView wvs) =
    mkPage [thestyle "background: url('img/noise.png') repeat scroll center top transparent; min-height: 100%;"] $
      with [thestyle "background: white; width:1000px; margin: 10px"] $
        vList $ map present wvs

instance Storeable Database FormView



mkView :: FormElt -> WebViewM Database (WebView Database)
mkView (RadioAnswerElt r) = mkRadioAnswerView r
mkView (ButtonAnswerElt r) = mkButtonAnswerView r
mkView (TextAnswerElt r) = mkTextAnswerView r
mkView (HtmlElt html) = mkHtmlView html
mkView (TableElt rows) = do { wvs <- mapM (mapM mkView) rows
                            ; mkTableView wvs
                            }

---- Main (needs to be below all webviews that use deriveInitial)

main :: IO ()
main = server rootViews "WebFormDB.txt" mkInitialDatabase $ Map.empty

rootViews :: RootViews Database
rootViews = [ ("",  mkFormView testForm)
            ] 
