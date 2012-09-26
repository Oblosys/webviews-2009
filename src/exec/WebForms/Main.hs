{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TupleSections, FlexibleInstances, ScopedTypeVariables, DoRec #-}
module Main where

import Data.List
import BlazeHtml
import Data.Generics
import Data.Char hiding (Space)
import Data.Function (on)
import Data.Maybe
import Data.List
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


-- WebForm data types

type WebForm = [FormElt]

data FormElt = HtmlElt String
             | RadioAnswerElt  RadioAnswer
             | ButtonAnswerElt ButtonAnswer
             | TextAnswerElt   TextAnswer
             | TableElt [[FormElt]]
   deriving (Eq, Show, Typeable, Data)

data RadioAnswer = RadioAnswer { getRadioQuestionTag :: QuestionTag, getRadioAnswers :: [String] }
   deriving (Eq, Show, Typeable, Data)

data ButtonAnswer = ButtonAnswer { getButtonQuestionTag :: QuestionTag, getButtonAnswers :: [String] }
   deriving (Eq, Show, Typeable, Data)

data TextAnswer = TextAnswer { getTextQuestionTag :: QuestionTag }
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormElt
instance MapWebView Database FormElt

deriveInitial ''RadioAnswer
instance MapWebView Database RadioAnswer

deriveInitial ''ButtonAnswer
instance MapWebView Database ButtonAnswer

deriveInitial ''TextAnswer
instance MapWebView Database TextAnswer



-- Form instance declaration

testForm = [ TableElt $
              [ [ HtmlElt "Leeftijd", TextAnswerElt $ TextAnswer "age"]
              , [ HtmlElt "Geslacht", ButtonAnswerElt $ ButtonAnswer "gender" ["Man", "Vrouw"]]
              , [ HtmlElt "App is leuk",       RadioAnswerElt $ RadioAnswer "nice" ["Ja","Nee"]]
              ] ] ++
           mkVignette Vignette { nummer = 1
                               , omschr1 = "Een app waarmee u rapporten mondeling kunt inspreken, die achteraf door andere medewerkers schriftelijk kunnen worden vastgelegd"
                               , omschr2 = "Een app waarmee u snel kunt zien welke medicijnen met elkaar interacteren"
                               , uitproberen1 = "Mogelijk"
                               , uitproberen2 = "Niet mogelijk"
                               , klaar1 = "Onvoldoende"
                               , klaar2 = "Goed"
                               , succes1 = "Goed"
                               , succes2 = "Onvoldoende"
                               , collegas1 = "Sceptisch"
                               , collegas2 = "Enthousiast"
                               , beloning1 = "Tijdbesparing"
                               , beloning2 = "Minder kans op fouten"
                               } {- ++ 
           [ HtmlElt "<br/><br/>" ] ++ 
           mkVignette Vignette { nummer = 2
                               , omschr1 = "Een app waarmee u snel kunt zien welke medicijnen met elkaar interacteren"
                               , omschr2 = "Een app waarmee u pati&euml;ntgegevens als een zakkaartje op uw iPhone meedraagt"
                               , uitproberen1 = "Niet mogelijk"
                               , uitproberen2 = "Mogelijk"
                               , klaar1 = "Onvoldoende"
                               , klaar2 = "Onvoldoende"
                               , succes1 = "Goed"
                               , succes2 = "Onvoldoende"
                               , collegas1 = "Sceptisch"
                               , collegas2 = "Sceptisch"
                               , beloning1 = "Minder kans op fouten"
                               , beloning2 = "Niet meer zeulen met dossiers"
                               }
-}

data Vignette = Vignette { nummer :: Int
                         , omschr1, omschr2, uitproberen1, uitproberen2, klaar1, klaar2, succes1, succes2
                         , collegas1, collegas2, beloning1, beloning2 :: String
                         }

mkVignette vt = 
  [ TableElt $
    [ [ HtmlElt $ "Vignette "++show (nummer vt), HtmlElt $ greyBg "Situatie 1", HtmlElt $ greyBg "Situatie 2"]
    , [ HtmlElt $ greyBg "<div style='width:300px; height:100px'>Omschrijving van de app</div>", HtmlElt $ "<div style='width:300px'>"++omschr1 vt++"</div>"
                                         , HtmlElt $ "<div style='width:300px'>"++omschr2 vt++"</div>"]
    , [ HtmlElt $ greyBg "Uitproberen", HtmlElt $ uitproberen1 vt, HtmlElt $ uitproberen2 vt]
    , [ HtmlElt $ greyBg "De mate waarin de organisatie technisch klaar is om de app in te voeren", HtmlElt $ klaar1 vt, HtmlElt $ klaar2 vt]
    , [ HtmlElt $ greyBg "De mate waarin de organisatie in het verleden succesvol technische innovaties heeft ingevoerd", HtmlElt $ succes1 vt, HtmlElt $ succes2 vt]
    , [ HtmlElt $ greyBg "Mening van uw collega's", HtmlElt $ collegas1 vt, HtmlElt $ collegas2 vt]
    , [ HtmlElt $ greyBg "Beloning", HtmlElt $ beloning1 vt, HtmlElt $ beloning2 vt] ]
    , HtmlElt $ "<br/><em>Vragen (kruis de situatie aan die het beste bij u past):</em><br/><br/>"
  , TableElt $
    [ [ HtmlElt $ greyBg "De app die het meest gemakkelijk te gebruiken voor mij als persoon is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".gemak") ["App 1", "App 2"]]
    , [ HtmlElt $ greyBg "De app die het meest nuttig ter ondersteuning van mijn dagelijkse werkzaamheden"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".nut") ["App 1", "App 2"]]
    , [ HtmlElt $ greyBg "De app die ik zou gebruiken is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".voorkeur") ["App 1", "App 2"]]
    , [HtmlElt $ "<br/><em>Omcirkel het getal dat aangeeft in hoeverre u het eens bent met onderstaande stelling:</em><br/><br/>" ]
    , [ HtmlElt $ greyBg "Ik vond het moeilijk om te kiezen"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".moeilijkKiezen") $ map show [1..10]]
    ]
  ]
  where greyBg str = "<div style='background-color: #EEE'>"++str++"</div>"  




------- WebViews

data SelectableView = SelectableView Bool String (Widget (EditAction Database)) deriving (Eq, Show, Typeable, Data)

deriveInitial ''SelectableView
deriveMapWebViewDb ''Database ''SelectableView


mkSelectableView :: [ViewId] -> String -> Bool -> EditM Database () -> WebViewM Database (WebView Database)
mkSelectableView allSelectableVids str selected clickCommand = mkWebView $
  \vid (SelectableView _ _ _) ->
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
mkSelectionViews :: [String] -> [String] -> ((Int,String) -> EditM Database ()) -> WebViewM Database [WebView Database]
mkSelectionViews strs selectedStrs clickActionF =
 do { rec { wvs <- sequence [ mkSelectableView vids str (str `elem` selectedStrs) $ clickActionF (i,str)  
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
mkRadioAnswerView r@(RadioAnswer questionTag answers) = mkWebView $
  \vid (RadioAnswerView _ radioOld) ->
    do { db <- getDb
       ; selection <- case unsafeLookup "mkRadioAnswerView" questionTag db of
                        Nothing    -> return $ -1
                        (Just str) -> return $ fromMaybe (-1) $ elemIndex str answers

       ; radio <-  mkRadioView answers selection True 
    
       ; return $ RadioAnswerView r radio
       }

instance Presentable RadioAnswerView where
  present (RadioAnswerView _ radio) =
      present radio

instance Storeable Database RadioAnswerView where
  save (RadioAnswerView (RadioAnswer questionTag answers) radio) =
    if getSelection radio == -1
    then clearAnswer questionTag 
    else setAnswer questionTag $ answers !! getSelection radio -- todo: unsafe


data ButtonAnswerView = ButtonAnswerView ButtonAnswer [WebView Database]
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''ButtonAnswerView
deriveMapWebViewDb ''Database ''ButtonAnswerView

mkButtonAnswerView :: ButtonAnswer -> WebViewM Database (WebView Database)
mkButtonAnswerView b@(ButtonAnswer questionTag answers) = mkWebView $
  \vid (ButtonAnswerView _ buttonsold) ->
    do { db <- getDb
       ; selectedStrs <- case unsafeLookup "mkButtonAnswerView" questionTag db of
                  Nothing    -> return []
                  (Just str) -> return [str]
       
       ; buttons <- mkSelectionViews answers selectedStrs $ \(_,str) -> modifyDb $ setAnswer questionTag str
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
mkTextAnswerView t@(TextAnswer questionTag) = mkWebView $
  \vid (TextAnswerView _ textfield) ->
    do { db <- getDb
       ; str <- case unsafeLookup "mkTextAnswerView" questionTag db of
                  Nothing    -> return ""
                  (Just str) -> return str  
       ; db <- getDb
       ; liftIO $ putStrLn $ "Db: "++show db
       
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
  FormView Bool (Widget (Button Database)) (Widget (Button Database)) [WebView Database]
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormView

deriveMapWebViewDb ''Database ''FormView

mkFormView :: WebForm -> WebViewM Database (WebView Database)
mkFormView form = mkWebView $
  \vid _ ->
    do { modifyDb $ initializeDb form
       ; formElts <- mapM mkView form
       ; db <- getDb
       ; liftIO $ putStrLn $ "Db is "++show db
       ; let isComplete = all isJust $ Map.elems db
       ; sendButton <- mkButton "Opsturen" isComplete $ Edit $
          do { db <- getDb
             ; liftIO $ putStrLn $ "Sending answers:\n"++show db
             }
       ; clearButton <- mkButton "Opnieuw" True $ Edit $ modifyDb $ \db -> Map.empty
       ; return $ FormView isComplete sendButton clearButton formElts
       }

instance Presentable FormView where
  present (FormView isComplete sendButton clearButton wvs) =
    mkPage [thestyle "background: url('img/noise.png') repeat scroll center top transparent; min-height: 100%; font-family: Geneva"] $
      with [thestyle "background: white; width:1000px; margin: 10px; padding: 10px"] $
        vList $ map present wvs ++ [present sendButton, present clearButton]

instance Storeable Database FormView

-- Initialize the database by putting a Nothing for each answer. (so completeness == absence of Nothings)  
initializeDb :: WebForm -> Database -> Database
initializeDb webForm = compose $ map initializeDbFormElt webForm

initializeDbFormElt (RadioAnswerElt r)  = initializeDbQuestion $ getRadioQuestionTag r
initializeDbFormElt (ButtonAnswerElt b) = initializeDbQuestion $ getButtonQuestionTag b
initializeDbFormElt (TextAnswerElt t)   = initializeDbQuestion $ getTextQuestionTag t
initializeDbFormElt (HtmlElt html) = id
initializeDbFormElt (TableElt rows) = compose $ concatMap (map initializeDbFormElt) rows

compose :: [(a->a)] -> a -> a
compose = foldl (.) id

initializeDbQuestion :: QuestionTag -> Database -> Database
initializeDbQuestion questionTag db = case Map.lookup questionTag db of
                                        Nothing  -> Map.insert questionTag Nothing db
                                        Just _   -> db
                            
mkView :: FormElt -> WebViewM Database (WebView Database)
mkView (RadioAnswerElt r) = mkRadioAnswerView r
mkView (ButtonAnswerElt b) = mkButtonAnswerView b
mkView (TextAnswerElt t) = mkTextAnswerView t
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
