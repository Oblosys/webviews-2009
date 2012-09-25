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

data RadioAnswer = RadioAnswer QuestionTag [String]
   deriving (Eq, Show, Typeable, Data)

data ButtonAnswer = ButtonAnswer QuestionTag [String]
   deriving (Eq, Show, Typeable, Data)

data TextAnswer = TextAnswer QuestionTag
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormElt
instance MapWebView Database FormElt

deriveInitial ''RadioAnswer
instance MapWebView Database RadioAnswer

deriveInitial ''ButtonAnswer
instance MapWebView Database ButtonAnswer

deriveInitial ''TextAnswer
instance MapWebView Database TextAnswer

testForm = [ TableElt $
              [ [ HtmlElt "Leeftijd", TextAnswerElt $ TextAnswer "age"]
              , [ HtmlElt "Geslacht", ButtonAnswerElt $ ButtonAnswer "gender" ["Man", "Vrouw"]]
              , [ HtmlElt "App is leuk",       RadioAnswerElt $ RadioAnswer "nice" ["Ja","Nee"]]
              ] ] ++
           mkVignette 1 "Een app waarmee u rapporten mondeling kunt inspreken, die achteraf door andere medewerkers schriftelijk kunnen worden vastgelegd"
                                "Een app waarmee u snel kunt zien welke medicijnen met elkaar interacteren"
                                "Mogelijk" "Niet mogelijk" "Onvoldoende" "Goed" "Goed" "Onvoldoende" 
                                "Sceptisch" "Enthousiast"
                                "Tijdbesparing" "Minder kans op fouten" ++ 
           [ HtmlElt "<br/><br/>" ] ++ 
           mkVignette 2 "Een app waarmee u snel kunt zien welke medicijnen met elkaar interacteren"
                                "Een app waarmee u pati&euml;ntgegevens als een zakkaartje op uw iPhone meedraagt"
                                "Niet mogelijk" "Mogelijk" "Onvoldoende" "Onvoldoende" "Goed" "Onvoldoende" 
                                "Sceptisch" "Sceptisch"
                                "Minder kans op fouten" "Niet meer zeulen met dossiers" 


mkVignette nr omschr1 omschr2 uitproberen1 uitproberen2 klaar1 klaar2 succes1 succes2 collegas1 collegas2 beloning1 beloning2 = 
  [ TableElt $
    [ [ HtmlElt "Vignette 1", HtmlElt $ greyBg "Situatie 1", HtmlElt $ greyBg "Situatie 2"]
    , [ HtmlElt $ greyBg "<div style='width:300px; height:100px'>Omschrijving van de app</div>", HtmlElt $ "<div style='width:300px'>"++omschr1++"</div>"
                                         , HtmlElt $ "<div style='width:300px'>"++omschr2++"</div>"]
    , [ HtmlElt $ greyBg "Uitproberen", HtmlElt uitproberen1, HtmlElt uitproberen2]
    , [ HtmlElt $ greyBg "De mate waarin de organisatie technisch klaar is om de app in te voeren", HtmlElt klaar1, HtmlElt klaar2]
    , [ HtmlElt $ greyBg "De mate waarin de organisatie in het verleden succesvol technische innovaties heeft ingevoerd", HtmlElt succes1, HtmlElt succes2]
    , [ HtmlElt $ greyBg "Mening van uw collega's", HtmlElt collegas1, HtmlElt collegas2]
    , [ HtmlElt $ greyBg "Beloning", HtmlElt beloning1, HtmlElt beloning2] ]
    , HtmlElt $ "<br/><em>Vragen (kruis de situatie aan die het beste bij u past):</em><br/><br/>"
  , TableElt $
    [ [ HtmlElt $ greyBg "De app die het meest gemakkelijk te gebruiken voor mij als persoon is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show nr++".gemak") ["App 1", "App 2"]]
    , [ HtmlElt $ greyBg "De app die het meest nuttig ter ondersteuning van mijn dagelijkse werkzaamheden"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show nr++".nut") ["App 1", "App 2"]]
    , [ HtmlElt $ greyBg "De app die ik zou gebruiken is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show nr++".voorkeur") ["App 1", "App 2"]]
    , [HtmlElt $ "<br/><em>Omcirkel het getal dat aangeeft in hoeverre u het eens bent met onderstaande stelling:</em><br/><br/>" ]
    , [ HtmlElt $ greyBg "Ik vond het moeilijk om te kiezen"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show nr++".moeilijkKiezen") $ map show [1..10]]
    ]
  ]
  where greyBg str = "<div style='background-color: #EEE'>"++str++"</div>"  

------- WebViews

data SelectableView = SelectableView Bool String (Widget (EditAction Database)) deriving (Eq, Show, Typeable, Data)

deriveInitial ''SelectableView
deriveMapWebViewDb ''Database ''SelectableView


mkSelectableView :: [ViewId] -> String -> EditM Database () -> WebViewM Database (WebView Database)
mkSelectableView allSelectableVids str clickCommand = mkWebView $
  \vid (SelectableView selected _ _) ->
    do { clickAction <- mkEditAction $ Edit $ do { sequence_ [ viewEdit v $ \(SelectableView _ str ca) ->
                                                                              SelectableView (vid == v) str ca
                                                             | v <- allSelectableVids
                                                             ]
                                                 ; clickCommand
                                                 }
                                             
                                                  
       ; return $ SelectableView selected str clickAction
       }

instance Presentable SelectableView where
  present (SelectableView selected str clickAction) =
    let color = if selected then "#0f3" else "white"
    in  withEditAction clickAction $ with [thestyle $ "background-color: "++color] $ boxed $ toHtml str


instance Storeable Database SelectableView
  
-- TODO: can make this more general by providing a list of (EditM db ()) for each button
mkSelectionViews :: [String] -> ((Int,String) -> EditM Database ()) -> WebViewM Database [WebView Database]
mkSelectionViews strs clickActionF =
 do { rec { wvs <- sequence [ mkSelectableView vids str $ clickActionF (i,str)  
                            | (i,str) <- zip [0..] strs
                            ]
          ; let vids = map getViewId wvs
          }
    ; return wvs
    }
     
data RadioAnswerView = RadioAnswerView RadioAnswer (Widget (RadioView Database))
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''RadioAnswerView
deriveMapWebViewDb ''Database ''RadioAnswerView

mkRadioAnswerView :: RadioAnswer -> WebViewM Database (WebView Database)
mkRadioAnswerView r@(RadioAnswer _ answers) = mkWebView $
  \vid (RadioAnswerView _ radioOld) ->
    do { radio <-  mkRadioView answers (getSelection radioOld) True 
    
       ; return $ RadioAnswerView r radio
       }

instance Presentable RadioAnswerView where
  present (RadioAnswerView _ radio) =
      present radio

instance Storeable Database RadioAnswerView where
  save (RadioAnswerView (RadioAnswer questionTag answers) radio) =
    setAnswer questionTag $ answers !! getSelection radio -- todo: unsafe


data ButtonAnswerView = ButtonAnswerView ButtonAnswer [WebView Database]
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''ButtonAnswerView
deriveMapWebViewDb ''Database ''ButtonAnswerView

mkButtonAnswerView :: ButtonAnswer -> WebViewM Database (WebView Database)
mkButtonAnswerView b@(ButtonAnswer questionTag answers) = mkWebView $
  \vid (ButtonAnswerView _ buttonsold) ->
    do { buttons <- mkSelectionViews answers $ \(_,str) -> modifyDb $ setAnswer questionTag str
       -- because we cannot access webview fields like widget values (because of existentials) we cannot
       -- query the webview in Storeable and put the setAnswer in an edit command instead.
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
mkTextAnswerView t = mkWebView $
  \vid (TextAnswerView (TextAnswer questionTag) textfield) ->
    do { db <- getDb
                                 
       ; str <- case Map.lookup questionTag db of
                  Nothing  -> do { modifyDb $ Map.insert questionTag Nothing
                                 ; return ""
                                 }
                  Just Nothing    -> return ""
                  Just (Just str) -> return str  
       ; --if getStrVal textfield == "" then Nothing else Just $ getStrVal textfield
       ; text <-  mkTextField str 
       ; return $ TextAnswerView t text
       }

instance Presentable TextAnswerView where
  present (TextAnswerView _ radio) =
      present radio

instance Storeable Database TextAnswerView where
  save (TextAnswerView (TextAnswer questionTag) text) =
    let str = getStrVal text
    in  if str == "" then clearAnswer questionTag else setAnswer questionTag str


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
      mkTable [border "1", cellpadding "5"] [] [] $ map (map present) rows

instance Storeable Database TableView


data FormView = 
  FormView Bool (Widget (Button Database)) [WebView Database]
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormView

deriveMapWebViewDb ''Database ''FormView

mkFormView :: WebForm -> WebViewM Database (WebView Database)
mkFormView form = mkWebView $
  \vid _ ->
    do { formElts <- mapM mkView form
       ; db <- getDb
       ; let isComplete = all isJust $ Map.elems db
       ; sendButton <- mkButton "Opsturen" isComplete $ Edit $
          do { db <- getDb
             ; liftIO $ putStrLn $ "Sending answers:\n"++show db
             }
       ; return $ FormView isComplete sendButton formElts
       }

instance Presentable FormView where
  present (FormView isComplete sendButton wvs) =
    mkPage [thestyle "background: url('img/noise.png') repeat scroll center top transparent; min-height: 100%; font-family: Geneva"] $
      with [thestyle "background: white; width:1000px; margin: 10px; padding: 10px"] $
        vList $ map present wvs ++ [present sendButton]

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
