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

-- TODO: the question tags are more answer tags
-- TODO: how about using css for table decorations? (add classes for top row, left column, etc.)

data WebForm = Form [FormPage]

data FormPage = Page [FormElt]

data FormElt = HtmlElt String
             | HtmlFileElt String
             | RadioAnswerElt  RadioAnswer
             | RadioTextAnswerElt  RadioTextAnswer
             | ButtonAnswerElt ButtonAnswer
             | TextAnswerElt   TextAnswer
             | StyleElt String FormElt
             | TableElt String Bool Bool Bool [[FormElt]] -- because elts are in other web view, they cannot set the table cell's
                                                   -- background color, so we need to specify headers explicitly.
   deriving (Eq, Show, Typeable, Data)

data RadioAnswer = RadioAnswer { getRadioQuestionTag :: QuestionTag, getRadioAnswers :: [String] }
   deriving (Eq, Show, Typeable, Data)

data RadioTextAnswer = RadioTextAnswer { getRadioTextRadioQuestionTag :: QuestionTag, getRadioTextTextQuestionTag :: QuestionTag, getRadioTextAnswers :: [String] }
   deriving (Eq, Show, Typeable, Data)

data ButtonAnswer = ButtonAnswer { getButtonQuestionTag :: QuestionTag, getButtonAnswers :: [String] }
   deriving (Eq, Show, Typeable, Data)

data TextAnswer = TextAnswer { getTextQuestionTag :: QuestionTag }
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormElt
instance MapWebView Database FormElt

deriveInitial ''RadioAnswer
instance MapWebView Database RadioAnswer

deriveInitial ''RadioTextAnswer
instance MapWebView Database RadioTextAnswer

deriveInitial ''ButtonAnswer
instance MapWebView Database ButtonAnswer

deriveInitial ''TextAnswer
instance MapWebView Database TextAnswer



-- Form instance declaration

testForm = Form $ [ introductie, persoonsgegevens, stellingen, deelDrie ] ++ vignettes

introductie = Page [ HtmlFileElt "Introductie.html" ]

persoonsgegevens = Page
  [ HtmlElt "<em>Eerst wil ik u enkele algemene vragen stellen, klik op het antwoord dat voor u van toepassing is of vul de betreffende informatie in.</em>"
  , medSkip
  , TableElt "Algemeen" False False False $
      [ [ HtmlElt "Wat is uw leeftijd?", StyleElt "width: 50px" $ TextAnswerElt $ TextAnswer "age"]
      , [ medSkip ]
      , [ HtmlElt "Wat is uw geslacht?", StyleElt "width: 100px" $ ButtonAnswerElt $ ButtonAnswer "gender" ["Man", "Vrouw"]]
      , [ medSkip ]
      , [ HtmlElt "In welke functie bent u momenteel werkzaam?", RadioTextAnswerElt $ RadioTextAnswer "functie" "functieAnders" 
                                                                                    [ "Activiteitenbegeleider"
                                                                                    , "Groepsbegeleider"
                                                                                    , "Helpende gezondheidszorg"
                                                                                    , "Verpleeghulp"
                                                                                    , "Verpleegkundige"
                                                                                    , "Verzorgende"
                                                                                    , "Anders, nl. :" ] ]
      , [ medSkip ]
      , [ HtmlElt "Wat is uw hoogst afgeronde opleiding?", RadioTextAnswerElt $ RadioTextAnswer "opleiding" "opleidingAnders" 
                                                                                    [ "Lager algemeen onderwijs (basisonderwijs)"
                                                                                    , "Lager beroepsonderwijs (LTS, LEAO)"
                                                                                    , "Middelbaar algemeen onderwijs (MAVO, MULO, VMBO)"
                                                                                    , "Middelbaar beroepsonderwijs (MTS, MEAO, MBO)"
                                                                                    , "Voortgezet algemeen onderwijs (HAVO,VWO, Atheneum, Gymnasium)"
                                                                                    , "Hoger beroepsonderwijs (HBO, HEAO, HTS)"
                                                                                    , "Wetenschappelijk onderwijs"
                                                                                    , "Anders, nl. :" ] ]
      ]
       ]
       
stellingen = Page
  [ HtmlElt "<em>Het tweede deel van de vragenlijst bevat twaalf stellingen die betrekking hebben op uw persoonlijke situatie in uw huidige werk. Wilt u aangeven in hoeverre u het eens bent met de stellingen hieronder door het cijfer aan te klikken. Hoe hoger het cijfer, des te beter u zich kunt vinden in de stelling.</em>"
  , bigSkip
  , TableElt "Stellingen" False False False $
      [ [ HtmlElt "", HtmlElt "Mee&nbsp;eens<span style='margin-left:50px'></span>Mee&nbsponeens" ]
      , mkScaleQuestion 7 "presteren" "Ik vind het belangrijk om beter te presteren dan mijn collega's"
      , mkScaleQuestion 7 "angst" "Mijn angst om op mijn werk onder te presteren is vaak wat mij motiveert"
      , mkScaleQuestion 7 "vermijden" "Ik wil vooral vermijden dat onderpresteer op mijn werk"
      , mkScaleQuestion 7 "vergelijking" "Ik vind het belangrijk om goed te presteren, in vergelijking met mijn directe collega's"
      , mkScaleQuestion 7 "beheersen" "Het is belangrijk voor mij om de werkzaamheden die ik verricht zo goed als mogelijk te beheersen"
      , mkScaleQuestion 7 "zorgen" "Ik maak mij vaak zorgen dat ik niet alle kennis en vaardigheden op kan doen die deze baan mij te bieden heeft"
      , mkScaleQuestion 7 "ontwikkelen" "Ik wil mij zo goed als mogelijk ontwikkelen tijdens mijn werk"
      , mkScaleQuestion 7 "kennis" "Soms maak ik mij zorgen dat ik kennis en vaardigheden mis om mijn werk uit te voeren, zoals ik dat zou willen"
      , mkScaleQuestion 7 "verlangen" "Ik verlang er naar om de vereiste kennis en vaardigheden voor mijn werk compleet onder de knie te krijgen"
      , mkScaleQuestion 7 "beoordeling" "Mijn doel binnen het werk dat ik doe is om een betere beoordeling te krijgen dan mijn collega's"
      , mkScaleQuestion 7 "onderprestatie" "Mijn doel op mijn werk is om onderprestatie te vermijden"
      , mkScaleQuestion 7 "vermogens" "Ik maak mij zorgen dat ik mij niet naar mijn eigen vermogen(s) kan ontwikkelen in deze baan"
      ] 
  ]

deelDrie = Page [ HtmlFileElt "DeelDrie.html" ]

vignettes = 
  [ Page $
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
                          } 
  , Page $
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
  ]
  
medSkip = vSkip 10
bigSkip = vSkip 20

vSkip :: Int -> FormElt
vSkip height = HtmlElt $ "<div style='height: " ++ show height ++ "px'></div>"

mkScaleQuestion scaleMax tag question = [ HtmlElt question, ButtonAnswerElt $ ButtonAnswer tag $ map show [1..scaleMax]]
data Vignette = Vignette { nummer :: Int
                         , omschr1, omschr2, uitproberen1, uitproberen2, klaar1, klaar2, succes1, succes2
                         , collegas1, collegas2, beloning1, beloning2 :: String
                         }

mkVignette vt = 
  [ TableElt "VignetteOmschr" False False False $
    [ [ HtmlElt $ "Vignette "++show (nummer vt), HtmlElt "Situatie 1", HtmlElt "Situatie 2"]
    , [ HtmlElt "<div >Omschrijving van de app</div>", HtmlElt $ "<div >"++omschr1 vt++"</div>"
                                         , HtmlElt $ "<div >"++omschr2 vt++"</div>"]
    , [ HtmlElt "Uitproberen", HtmlElt $ uitproberen1 vt, HtmlElt $ uitproberen2 vt]
    , [ HtmlElt "De mate waarin de organisatie technisch klaar is om de app in te voeren", HtmlElt $ klaar1 vt, HtmlElt $ klaar2 vt]
    , [ HtmlElt "De mate waarin de organisatie in het verleden succesvol technische innovaties heeft ingevoerd", HtmlElt $ succes1 vt, HtmlElt $ succes2 vt]
    , [ HtmlElt "Mening van uw collega's", HtmlElt $ collegas1 vt, HtmlElt $ collegas2 vt]
    , [ HtmlElt "Beloning", HtmlElt $ beloning1 vt, HtmlElt $ beloning2 vt] ]
  , bigSkip
  , HtmlElt $ "<br/><em>Vragen (kruis de situatie aan die het beste bij u past):</em><br/><br/>"
  , TableElt "VignetteVragen" False False False $
    [ [ HtmlElt "De app die het meest gemakkelijk te gebruiken voor mij als persoon is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".gemak") ["App&nbsp;1", "App&nbsp;2"]]
    , [ HtmlElt "De app die het meest nuttig ter ondersteuning van mijn dagelijkse werkzaamheden"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".nut") ["App&nbsp;1", "App&nbsp;2"]]
    , [ HtmlElt "De app die ik zou gebruiken is"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".voorkeur") ["App&nbsp;1", "App&nbsp;2"]]
    ]
  , bigSkip
  , HtmlElt $ "<br/><em>Klik op het cijfer dat aangeeft in hoeverre u het eens bent met onderstaande stelling:</em><br/><br/>" 
  , TableElt "VignetteKiezen" False False False $
     [[ HtmlElt "Ik vond het moeilijk om te kiezen"
      , ButtonAnswerElt $ ButtonAnswer ("vignette"++show (nummer vt)++".moeilijkKiezen") $ map show [1..10]]
    ]
  ]




------- WebViews lib

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
    let (lineWidth,bgColor,fgColor) = if selected then (2, "#33f", "#fff") else (1, "#eee", "#000")
    in  withEditAction clickAction $ with [thestyle $ "background-color: "++bgColor] $
          boxedEx lineWidth 4 $ with [thestyle $ "color: "++fgColor] $ primHtml str


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

------- WebViews form
     
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


-- Similar to Radio, but has a text field that is enabled when the last answer is selected (which will be "other:")
data RadioTextAnswerView = RadioTextAnswerView RadioTextAnswer (Widget (RadioView Database)) (Widget (TextView Database))
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''RadioTextAnswerView
deriveMapWebViewDb ''Database ''RadioTextAnswerView


mkRadioTextAnswerView :: RadioTextAnswer -> WebViewM Database (WebView Database)
mkRadioTextAnswerView r@(RadioTextAnswer radioQuestionTag textQuestionTag answers) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; selection <- case unsafeLookup "mkRadioTextAnswerView.1" radioQuestionTag db of
                        Nothing    -> return $ -1
                        (Just str) -> return $ fromMaybe (-1) $ elemIndex str answers

       ; str <- case unsafeLookup "mkRadioTextAnswerView.2" textQuestionTag db of
                  Nothing    -> return ""
                  (Just str) -> return str  

       ; radio <-  mkRadioView answers selection True 
       ; let isTextAnswerSelected = selection == length answers - 1
       ; text <- mkTextFieldEx (if isTextAnswerSelected then str else "") isTextAnswerSelected "" Nothing Nothing
       ; return $ RadioTextAnswerView r radio text
       }

instance Presentable RadioTextAnswerView where
  present (RadioTextAnswerView _ radio text) =
    present radio >> present text

instance Storeable Database RadioTextAnswerView where
  save (RadioTextAnswerView (RadioTextAnswer radioQuestionTag textQuestionTag answers) radio text) =
    if getSelection radio == -1 
    then clearAnswer radioQuestionTag . clearAnswer textQuestionTag
    else (setAnswer radioQuestionTag $ answers !! getSelection radio) . -- todo: unsafe
         if getSelection radio < length answers - 1
         then (setAnswer textQuestionTag "") -- don't clear, because then it counts as not completed 
         else if getStrVal text == "" then clearAnswer textQuestionTag else setAnswer textQuestionTag $ getStrVal text 
-- TODO: maybe cache answer?

data ButtonAnswerView = ButtonAnswerView ButtonAnswer [WebView Database]
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''ButtonAnswerView
deriveMapWebViewDb ''Database ''ButtonAnswerView

mkButtonAnswerView :: ButtonAnswer -> WebViewM Database (WebView Database)
mkButtonAnswerView b@(ButtonAnswer questionTag answers) = mkWebView $
  \vid _ ->
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
      hStretchList $ intersperse space $ map (E . present) buttons

instance Storeable Database ButtonAnswerView

data TextAnswerView = TextAnswerView TextAnswer (Widget (TextView Database))
   deriving (Eq, Show, Typeable, Data)


deriveInitial ''TextAnswerView
deriveMapWebViewDb ''Database ''TextAnswerView

mkTextAnswerView :: TextAnswer -> WebViewM Database (WebView Database)
mkTextAnswerView t@(TextAnswer questionTag) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; str <- case unsafeLookup "mkTextAnswerView" questionTag db of
                  Nothing    -> return ""
                  (Just str) -> return str  
       
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


data StyleView = StyleView String (WebView Database)
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''StyleView
deriveMapWebViewDb ''Database ''StyleView

mkStyleView :: String -> WebView Database -> WebViewM Database (WebView Database)
mkStyleView styleStr wv = mkWebView $
  \vid _ ->
    do { return $ StyleView styleStr wv
       }

instance Presentable StyleView where
  present (StyleView styleStr wv) =
      with [thestyle styleStr] $ present wv

instance Storeable Database StyleView



data TableView = TableView String Bool Bool Bool [[WebView Database]]
   deriving (Eq, Show, Typeable, Data)

deriveInitial ''TableView
deriveMapWebViewDb ''Database ''TableView

mkTableView :: String -> Bool -> Bool -> Bool -> [[WebView Database]] -> WebViewM Database (WebView Database)
mkTableView classTag border topHeader leftHeader rows = mkWebView $
  \vid _ ->
    do { return $ TableView classTag border topHeader leftHeader rows
       }

instance Presentable TableView where
  present (TableView classTag hasBorder topHeader leftHeader rows) =
    mkTableEx [border $ if hasBorder then "1" else "0", cellpadding "5", theclass $ "FormTable Table"++classTag ] [] [] $ 
      [ [ ([ theclass $ "TableCell " ++ (if rowNr == 1 then " TopRow" else "") ++
                                        (if rowNr > 1 && rowNr < nrOfRows then " MiddleRow" else "") ++
                                        (if rowNr == nrOfRows then " BottomRow" else "") ++
                                        (if colNr == 1 then " LeftCol" else "") ++
                                        (if colNr > 1 && colNr < nrOfCols then " MiddleCol" else "") ++
                                        (if colNr == nrOfCols then " RightCol" else "")
           ] ++                              
           if rowNr == 1 && topHeader || colNr == 1 && leftHeader then [style "background-color: #EEE"] else []
          , present elt) 
        | (colNr, elt) <- zip [1..] row ]
      | (rowNr, row) <- zip [1..] rows ]
      where nrOfRows = length rows
            nrOfCols = case rows of
                         [] -> 0
                         (row:_) -> length row

instance Storeable Database TableView


mkViewFormElt :: FormElt -> WebViewM Database (WebView Database)
mkViewFormElt (RadioAnswerElt r)      = mkRadioAnswerView r
mkViewFormElt (RadioTextAnswerElt rt) = mkRadioTextAnswerView rt
mkViewFormElt (ButtonAnswerElt b)     = mkButtonAnswerView b
mkViewFormElt (TextAnswerElt t)       = mkTextAnswerView t
mkViewFormElt (HtmlElt html)          = mkHtmlView html
mkViewFormElt (HtmlFileElt path)      = mkHtmlTemplateView ("Webforms/"++path) []
mkViewFormElt (StyleElt styleStr elt) = 
 do { wv <- mkViewFormElt elt
    ; mkStyleView styleStr wv
    }
mkViewFormElt (TableElt classTag border topHeader leftHeader rows) = 
  do { wvs <- mapM (mapM mkViewFormElt) rows
     ; mkTableView classTag border topHeader leftHeader wvs
     }
-- TODO: recursion in mkView (Form & FormPage) or separate (TableEtl)
--       separate may be clearer, but has the weird situation that we get both FormPage and the WebViews
--       representing the page. Also maybe we don't want to generate all pages but only the current one.

data FormPageView = 
  FormPageView [WebView Database]
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormPageView

deriveMapWebViewDb ''Database ''FormPageView

mkFormPageView :: FormPage -> WebViewM Database (WebView Database)
mkFormPageView (Page elts) = mkWebView $
  \vid _ ->
    do { pageViews <- mapM mkViewFormElt elts
       ; return $ FormPageView pageViews
       }

instance Presentable FormPageView where
  present (FormPageView wvs) =
    vList $ map present wvs

instance Storeable Database FormPageView

data FormView = 
  FormView Bool Int Int (Widget (Button Database)) (Widget (Button Database)) (Widget (Button Database)) (Widget (Button Database)) (WebView Database)
    deriving (Eq, Show, Typeable, Data)

deriveInitial ''FormView

deriveMapWebViewDb ''Database ''FormView

mkFormView :: WebForm -> WebViewM Database (WebView Database)
mkFormView form@(Form pages) = mkWebView $
  \vid (FormView _ _ _ _ _ _ _ _) ->
    do { modifyDb $ initializeDb form
       ; args <- getHashArgs
       ; let currentPage = case args of
                             ("p", nrStr):_ | Just nr <- readMaybe nrStr  -> nr - 1
                             _                                            -> 0 
       ; pageView <- mkFormPageView $ pages!!currentPage
       ; db <- getDb
       ; liftIO $ putStrLn $ "Db is "++show db
       ; let isComplete = all isJust $ Map.elems db
       ; sendButton <- mkButton "Opsturen" isComplete $ Edit $
          do { db <- getDb
             ; liftIO $ putStrLn $ "Sending answers:\n"++show db
             }
       ; clearButton <- mkButton "Alles wissen" True $ ConfirmEdit "Weet u zeker dat u alle antwoorden wilt wissen?" 
                                                     $ Edit $ modifyDb $ \db -> Map.empty
       ; prevButton <- mkButtonWithClick "Vorige" (currentPage/=0) $ \_ -> jsNavigateTo $ "'#form&p="++show (1+ currentPage - 1)++"'"
       ; nextButton <- mkButtonWithClick "Volgende" (currentPage < length pages - 1) $ \_ -> jsNavigateTo $ "'#form&p="++show (1+ currentPage + 1)++"'" 
       ; return $ FormView isComplete currentPage (length pages) prevButton nextButton sendButton clearButton pageView
       }

instance Presentable FormView where
  present (FormView isComplete currentPage nrOfPages prevButton nextButton sendButton clearButton wv) =
    mkPage [thestyle "background: url('img/noise.png') repeat scroll center top transparent; min-height: 100%; font-family: Geneva"] $
      with [theclass "FormPage", thestyle "background: white;", align "left"] $
                                 -- dimensions are specified in css to allow iPad specific style                                                              
        mkPageHeader +++
        vList [ present wv
              , vSpace 40
              , hStretchList [ E $ present clearButton, space, E $ present sendButton ]
              ]
   where mkPageHeader = with [ align "right", style "margin-bottom:40px"] $
                          hList [ present prevButton
                                , with [style "font-size: 80%"] $ nbsp >> (toHtml $ "Pagina "++show (currentPage+1) ++"/"++show nrOfPages) >> nbsp
                                , present nextButton ] 
      --  +++
          

instance Storeable Database FormView


-- Initialize the database by putting a Nothing for each answer. (so completeness == absence of Nothings)  
initializeDb :: WebForm -> Database -> Database
initializeDb (Form pages) = compose $ map initializeDbFormPage pages

initializeDbFormPage :: FormPage -> Database -> Database
initializeDbFormPage (Page elts) = compose $ map initializeDbFormElt elts

initializeDbFormElt (RadioAnswerElt r)      = initializeDbQuestion $ getRadioQuestionTag r
initializeDbFormElt (RadioTextAnswerElt r)  = (initializeDbQuestion $ getRadioTextRadioQuestionTag r) . 
                                              (initializeDbQuestion $ getRadioTextTextQuestionTag r)
initializeDbFormElt (ButtonAnswerElt b)     = initializeDbQuestion $ getButtonQuestionTag b
initializeDbFormElt (TextAnswerElt t)       = initializeDbQuestion $ getTextQuestionTag t
initializeDbFormElt (HtmlElt html)          = id
initializeDbFormElt (HtmlFileElt path)      = id
initializeDbFormElt (StyleElt _ elt)        = initializeDbFormElt elt
initializeDbFormElt (TableElt _ _ _ _ rows) = compose $ concatMap (map initializeDbFormElt) rows

compose :: [(a->a)] -> a -> a
compose = foldl (.) id

initializeDbQuestion :: QuestionTag -> Database -> Database
initializeDbQuestion questionTag db = case Map.lookup questionTag db of
                                        Nothing  -> Map.insert questionTag Nothing db
                                        Just _   -> db


---- Main (needs to be below all webviews that use deriveInitial)

main :: IO ()
main = server "Blij van IT" rootViews "BlijVanIT.css" "WebFormDB.txt" mkInitialDatabase $ Map.empty

rootViews :: RootViews Database
rootViews = [ ("",  mkFormView testForm), ("form",  mkFormView testForm)
            ] 
