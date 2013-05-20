{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE TypeOperators, TupleSections, FlexibleInstances, ScopedTypeVariables, DoRec #-}
module Main where

import Data.List
import BlazeHtml hiding (form)
import Data.Generics
import Data.Char hiding (Space, isNumber)
import Data.Function (on)
import Data.Maybe
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.Time
import System.Directory
import Types
import ObloUtils
import Utils
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
-- TODO: figure out how to execute js without problem with incrementality (we now show state in a comment to force evaluation)
--       Related: somehow queue stuff, since now we do a initProgressMarker for every element on the page
-- TODO: the question tags are more answer tags
-- TODO: update script on edit (tricky, needs event model on all widgets and also on ButtonAnswerVIew)

-- TODO: dynamic updates on progress markers and button disenable. Requires validation in both Haskell and Javascript.
-- TODO: show reason for validation fail.

resultsFilepath = "scr/BlijVanIT.csv"

data WebForm = Form [FormPage]

data FormPage = Page [FormElt]

data FormElt = HtmlElt String
             | HtmlFileElt String
             | RadioAnswerElt  RadioAnswer
             | RadioTextAnswerElt  RadioTextAnswer
             | ButtonAnswerElt ButtonAnswer
             | TextAnswerElt   TextAnswer
             | StyleElt String FormElt
             | TableElt String Bool Bool Bool [TableRow] -- because elts are in other web view, they cannot set the table cell's
                                                   -- background color, so we need to specify headers explicitly.
   deriving Show

type TableRow = [FormElt]

data RadioAnswer = RadioAnswer { getRadioQuestionTag :: QuestionTag, getRadioAnswers :: [String] }
   deriving (Eq, Show)

data RadioTextAnswer = RadioTextAnswer { getRadioTextRadioQuestionTag :: QuestionTag, getRadioTextTextQuestionTag :: QuestionTag, getRadioTextAnswers :: [String] 
                                       , getRadioTextValidate :: String -> Bool -- TODO: maybe use data Correct / Incorrect String to provide hints/error messages 
                                       }
   deriving Show

data ButtonAnswer = ButtonAnswer { getButtonQuestionTag :: QuestionTag, getButtonAnswers :: [String] }
   deriving (Eq, Show)

data TextAnswer = TextAnswer { getTextQuestionTag :: QuestionTag
                             , getTextValidate :: String -> Bool -- TODO: maybe use data Correct / Incorrect String to provide hints/error messages 
                             } 
   deriving Show

deriveInitial ''FormElt
instance MapWebView Database FormElt

deriveInitial ''RadioAnswer
instance MapWebView Database RadioAnswer

deriveInitial ''ButtonAnswer
instance MapWebView Database ButtonAnswer

form pages = Form pages

page elts = Page elts

htmlElt str = HtmlElt str

htmlFileElt str = HtmlFileElt str

styleElt style elt = StyleElt style elt

tableElt tag rows = TableElt tag False False False rows
 
textAnswerValidateElt tag validate = TextAnswerElt $ TextAnswer tag validate

textAnswerElt tag = TextAnswerElt $ TextAnswer tag $ const True -- any input is correct

buttonAnswerElt tag answers = ButtonAnswerElt $ ButtonAnswer tag answers

radioAnswerElt tag answers = RadioAnswerElt $ RadioAnswer tag answers

radioTextAnswerElt rtag ttag answers = RadioTextAnswerElt $ RadioTextAnswer rtag ttag answers $ const True -- any input is correct

radioTextAnswerValidateElt rtag ttag validate answers = RadioTextAnswerElt $ RadioTextAnswer rtag ttag validate answers

medSkip = vSkip 30
bigSkip = vSkip 45

vSkip :: Int -> FormElt
vSkip height = htmlElt $ "<div style='height: " ++ show height ++ "px'></div>"

isNumber = all isDigit



-- Form instance declaration

testPages = [ page [ htmlElt "Wat is uw leeftijd?", styleElt "width: 50px" $ textAnswerValidateElt "testAge" isNumber ]
            , page [ htmlElt "Bla?", styleElt "width: 50px" $ buttonAnswerElt "bla" ["Ja", "Nee"]]
            ]

--testForm = form $ testPages ++ [ introductie, persoonsgegevens ]

-- Blij van IT onderzoek

mainForm = form $ [ introductie, persoonsgegevens, stellingen, deelDrie ] ++ vignettes 

introductie = page [ htmlFileElt "Introductie.html" ]

persoonsgegevens = page
  [ htmlElt "<em>Eerst wil ik u enkele algemene vragen stellen, klik op het antwoord dat voor u van toepassing is of vul de betreffende informatie in.</em>"
  , medSkip
  , tableElt "Algemeen"
      [ [ htmlElt "Wat is uw leeftijd?", styleElt "width: 50px" $ textAnswerValidateElt "leeftijd" isNumber ]
      , [ medSkip ]
      , [ htmlElt "Wat is uw geslacht?", styleElt "width: 100px" $ buttonAnswerElt "geslacht" ["Man", "Vrouw"]]
      , [ medSkip ]
      , [ htmlElt "In welke functie bent u momenteel werkzaam?", radioTextAnswerElt "functie" "functieAnders" 
                                                                   [ "Activiteitenbegeleider"
                                                                   , "Groepsbegeleider"
                                                                   , "Helpende gezondheidszorg"
                                                                   , "Verpleeghulp"
                                                                   , "Verpleegkundige"
                                                                   , "Verzorgende"
                                                                   , "Anders, nl. :" ] ]
      , [ medSkip ]
      , [ htmlElt "Wat is uw hoogst afgeronde opleiding?", radioTextAnswerElt "opleiding" "opleidingAnders" 
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
       
stellingen = page
  [ htmlElt "<em>Het tweede deel van de vragenlijst bevat twaalf stellingen die betrekking hebben op uw persoonlijke situatie in uw huidige werk. Wilt u aangeven in hoeverre u het eens bent met de stellingen hieronder door het cijfer aan te klikken. Hoe hoger het cijfer, des te beter u zich kunt vinden in de stelling.</em>"
  , bigSkip
  , tableElt "Stellingen"
      [ [ htmlElt "", htmlElt "Mee&nbsp;eens<span style='margin-left:50px'></span>Mee&nbsp;oneens" ]
      , mkScaleQuestion 7 "presteren" "Ik vind het belangrijk om beter te presteren dan mijn collega's"
      , mkScaleQuestion 7 "angst" "Mijn angst om op mijn werk onder te presteren is vaak wat mij motiveert"
      , mkScaleQuestion 7 "vermijden" "Ik wil vooral vermijden dat ik onderpresteer op mijn werk"
      , mkScaleQuestion 7 "vergelijking" "Ik vind het belangrijk om goed te presteren, in vergelijking met mijn directe collega's"
{-      , mkScaleQuestion 7 "beheersen" "Het is belangrijk voor mij om de werkzaamheden die ik verricht zo goed als mogelijk te beheersen"
      , mkScaleQuestion 7 "zorgen" "Ik maak mij vaak zorgen dat ik niet alle kennis en vaardigheden op kan doen die deze baan mij te bieden heeft"
      , mkScaleQuestion 7 "ontwikkelen" "Ik wil mij zo goed als mogelijk ontwikkelen tijdens mijn werk"
      , mkScaleQuestion 7 "kennis" "Soms maak ik mij zorgen dat ik kennis en vaardigheden mis om mijn werk uit te voeren, zoals ik dat zou willen"
      , mkScaleQuestion 7 "verlangen" "Ik verlang er naar om de vereiste kennis en vaardigheden voor mijn werk compleet onder de knie te krijgen"
      , mkScaleQuestion 7 "beoordeling" "Mijn doel binnen het werk dat ik doe is om een betere beoordeling te krijgen dan mijn collega's"
      , mkScaleQuestion 7 "onderprestatie" "Mijn doel op mijn werk is om onderprestatie te vermijden"
      , mkScaleQuestion 7 "vermogens" "Ik maak mij zorgen dat ik mij niet naar mijn eigen vermogen(s) kan ontwikkelen in deze baan"
-}      ] 
  ]

deelDrie = page [ htmlFileElt "DeelDrie.html" ]

vignettes = 
  [ mkVignettePage Vignette { nummer = 1
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
  , mkVignettePage Vignette { nummer = 2
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


-- Create a table row containing a question that has n numbered answer buttons.
mkScaleQuestion :: Int -> String -> String -> TableRow
mkScaleQuestion n tag question = [ htmlElt question, buttonAnswerElt tag $ map show [1..n] ]

data Vignette = Vignette { nummer :: Int
                         , omschr1, omschr2, uitproberen1, uitproberen2, klaar1, klaar2, succes1, succes2
                         , collegas1, collegas2, beloning1, beloning2 :: String
                         }
                      
-- Create a form page for vignette vt.    
mkVignettePage :: Vignette -> FormPage
mkVignettePage vt = page $ 
  [ tableElt "VignetteOmschr"
    [ [ htmlElt $ "Vignette "++show (nummer vt), htmlElt "Situatie 1", htmlElt "Situatie 2"]
    , [ htmlElt "<div >Omschrijving van de app</div>", htmlElt $ "<div >"++omschr1 vt++"</div>"
                                                     , htmlElt $ "<div >"++omschr2 vt++"</div>"]
    , [ htmlElt "Uitproberen", htmlElt $ uitproberen1 vt, htmlElt $ uitproberen2 vt]
    , [ htmlElt "De mate waarin de organisatie technisch klaar is om de app in te voeren", htmlElt $ klaar1 vt, htmlElt $ klaar2 vt]
    , [ htmlElt "De mate waarin de organisatie in het verleden succesvol technische innovaties heeft ingevoerd", htmlElt $ succes1 vt, htmlElt $ succes2 vt]
    , [ htmlElt "Mening van uw collega's", htmlElt $ collegas1 vt, htmlElt $ collegas2 vt]
    , [ htmlElt "Beloning", htmlElt $ beloning1 vt, htmlElt $ beloning2 vt] ]
  , medSkip
  , htmlElt $ "<br/><em>Vragen (kruis de situatie aan die het beste bij u past):</em><br/><br/>"
  , tableElt "VignetteVragen"
    [ [ htmlElt "De app die het meest gemakkelijk te gebruiken voor mij als persoon is"
      , buttonAnswerElt ("vignette"++show (nummer vt)++".gemak") ["App 1", "App 2"]]
    , [ htmlElt "De app die het meest nuttig ter ondersteuning van mijn dagelijkse werkzaamheden"
      , buttonAnswerElt ("vignette"++show (nummer vt)++".nut") ["App 1", "App 2"]]
    , [ htmlElt "De app die ik zou gebruiken is"
      , buttonAnswerElt ("vignette"++show (nummer vt)++".voorkeur") ["App 1", "App 2"]]
    ]
  , medSkip
  , htmlElt $ "<br/><em>Klik op het cijfer dat aangeeft in hoeverre u het eens bent met onderstaande stelling:</em><br/><br/>" 
  , tableElt "VignetteKiezen"
     [ mkScaleQuestion 10 ("vignette"++show (nummer vt)++".moeilijkKiezen") "Ik vond het moeilijk om te kiezen" ]
  ]


------- WebViews lib


newtype NoPresent a = NoPresent a
{- Wraps a type to provide Eq,Show, Initial, MapWebView instances (which should not be evaluated).
   We can use it to put state in webviews.
    
-}

instance Eq (NoPresent a) where
  x == y = True

instance Show (NoPresent a) where
  show _ = "(NoPresent a)" -- TODO: why do we need show? For seq'ing?

instance Initial (NoPresent a) where
  initial = error "no initial for Wrapped"

instance MapWebView db (NoPresent a)


------- WebViews form

data Answered = Unanswered | Invalid | Answered deriving (Eq, Show)

instance Initial Answered where
  initial = Unanswered

instance MapWebView db Answered
 
withAnswerClass :: String -> Answered -> Html -> Html
withAnswerClass answerType answered = with [strAttr "AnswerType" answerType, strAttr "Answer" $ attrVal answered ]
 where attrVal Unanswered = "Unanswered"
       attrVal Invalid    = "Invalid"
       attrVal Answered   = "Answered"
       
--------- RadioAnswerView ------------------------------------------------------------

data RadioAnswerView = RadioAnswerView RadioAnswer Answered (Widget (RadioView Database)) String
   deriving (Eq, Show, Typeable)

deriveInitial ''RadioAnswerView
deriveMapWebViewDb ''Database ''RadioAnswerView

mkRadioAnswerView :: RadioAnswer -> WebViewM Database (WV RadioAnswerView)
mkRadioAnswerView r@(RadioAnswer questionTag answers) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; selection <- case getStringAnswer questionTag db of
                        Nothing    -> return $ -1
                        Just str -> return $ fromMaybe (-1) $ elemIndex str answers

       ; radio <-  mkRadioView answers selection True 
    
       ; return $ RadioAnswerView r (if selection /= -1 then Answered else Unanswered) radio $ jsScript 
           [ "// selection is "++show selection
           , "initProgressMarkers()"
           ]
       }

instance Presentable RadioAnswerView where
  present (RadioAnswerView _ answered radio script) = (withAnswerClass "RadioAnswer" answered $ present radio) +++ mkScript script

instance Storeable Database RadioAnswerView where
  save (RadioAnswerView (RadioAnswer questionTag answers) answered radio _) =
    if getSelection radio == -1
    then clearAnswer questionTag 
    else setAnswer questionTag Nothing $ answers !! getSelection radio -- todo: unsafe


--------- RadioTextAnswerView ------------------------------------------------------------

-- Similar to Radio, but has a text field that is enabled when the last answer is selected (which will be "other:")
data RadioTextAnswerView = 
       RadioTextAnswerView String String [String] Answered (Widget (RadioView Database)) (Widget (TextView Database))
                           (NoPresent (String -> Bool)) String         
   deriving (Eq, Show, Typeable)

deriveInitial ''RadioTextAnswerView
deriveMapWebViewDb ''Database ''RadioTextAnswerView


mkRadioTextAnswerView :: RadioTextAnswer -> WebViewM Database (WV RadioTextAnswerView)
mkRadioTextAnswerView r@(RadioTextAnswer radioQuestionTag textQuestionTag answers validate) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; selection <- case getStringAnswer radioQuestionTag db of
                        Nothing  -> return $ -1
                        Just str -> return $ fromMaybe (-1) $ elemIndex str answers

       ; str <- case getStringAnswer textQuestionTag db of
                  Nothing    -> return ""
                  Just str -> return str  

       ; radio <-  mkRadioView answers selection True 
       ; let isTextAnswerSelected = selection == length answers - 1
       ; textField <- mkTextFieldEx (if isTextAnswerSelected then str else "") isTextAnswerSelected "" Nothing Nothing
       ; return $ RadioTextAnswerView radioQuestionTag textQuestionTag answers
                                      (answered selection str)
                                      radio textField
                                      (NoPresent validate) $ jsScript 
           [ "// selection is "++show selection ++", str is "++show str -- TODO: hack: prevent incrementality from not executing script
           , "initProgressMarkers()"
           ]
       }
 where answered (-1) _                              = Unanswered
       answered sel str | sel < length answers - 1  = Answered
                        | str /= "" && validate str = Answered -- last answer
                        | otherwise                 = Invalid  -- is selected
                    
                    --(if not $ selection == -1 || isTextAnswerSelected && str == "" then Answered else Unanswered)
instance Presentable RadioTextAnswerView where
  present (RadioTextAnswerView _ _ _ answered radio textField _ script) =
    (withAnswerClass "RadioTextAnswer" answered $ present radio >> present textField) +++ mkScript script

instance Storeable Database RadioTextAnswerView where
  save (RadioTextAnswerView radioQuestionTag textQuestionTag answers valid radio textField (NoPresent validate) _) =
    if getSelection radio == -1 
    then clearAnswer radioQuestionTag . clearAnswer textQuestionTag
    else (setAnswer radioQuestionTag (Just validate) $ answers !! getSelection radio) . -- todo: unsafe
         if getSelection radio < length answers - 1
         then (setAnswer textQuestionTag (Just validate) "") -- don't clear, because then it counts as not completed 
         else if getStrVal textField == "" then clearAnswer textQuestionTag else setAnswer textQuestionTag (Just validate) $ getStrVal textField 
-- TODO: maybe cache answer?


--------- ButtonAnswerView ------------------------------------------------------------

data ButtonAnswerView = ButtonAnswerView ButtonAnswer Answered [WV (SelectableView Database)] String
   deriving (Eq, Show, Typeable)


deriveInitial ''ButtonAnswerView
deriveMapWebViewDb ''Database ''ButtonAnswerView

mkButtonAnswerView :: ButtonAnswer -> WebViewM Database (WV ButtonAnswerView)
mkButtonAnswerView b@(ButtonAnswer questionTag answers) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; let mSelectedStr :: Maybe String = getStringAnswer questionTag db
       
       ; buttons <- mkSelectableViews answers mSelectedStr $ \(_,str) -> modifyDb $ setAnswer questionTag Nothing str
       -- because we cannot access webview fields like widget values (because of existentials) we cannot
       -- query the webview in Storeable and put the setAnswer in an edit command instead.
       ; return $ ButtonAnswerView b (if isJust mSelectedStr then Answered else Unanswered) buttons $ jsScript 
           [ "// selection is "++show mSelectedStr -- TODO: hack: prevent incrementality from not executing script
           , "initProgressMarkers()"
           ]
       }

instance Presentable ButtonAnswerView where
  present (ButtonAnswerView _ answered buttons script) =
    (withAnswerClass "ButtonAnswer" answered $ hStretchList $ intercalate [E $ mkClassDiv "ButtonAnswerSep" $ noHtml, space] $ [[E $ present b] | b <-  buttons]) +++
    mkScript script

instance Storeable Database ButtonAnswerView


--------- TextAnswerView ------------------------------------------------------------

data TextAnswerView = TextAnswerView String Answered (Widget (TextView Database)) (NoPresent (String -> Bool)) String
   deriving (Eq, Show, Typeable)


deriveInitial ''TextAnswerView
deriveMapWebViewDb ''Database ''TextAnswerView

mkTextAnswerView :: TextAnswer -> WebViewM Database (WV TextAnswerView)
mkTextAnswerView t@(TextAnswer questionTag validate) = mkWebView $
  \vid _ ->
    do { db <- getDb
       ; str <- case getStringAnswer questionTag db of
                  Nothing    -> return ""
                  (Just str) -> return str  
       
       ; textField <-  mkTextField str 

       ; return $ TextAnswerView questionTag (answered str) textField (NoPresent validate) $ jsScript 
           [ "// str is "++show str -- TODO: hack: prevent incrementality from not executing script
           , "initProgressMarkers()"
           ]
       }
 where answered ""                 = Unanswered
       answered str | validate str = Answered
                    | otherwise    = Invalid
                    
instance Presentable TextAnswerView where
  present (TextAnswerView _ answered textField _ script) = (withAnswerClass "TextAnswer" answered $ present textField) +++ mkScript script

instance Storeable Database TextAnswerView where
  save (TextAnswerView questionTag _ textField (NoPresent validate) _) =
    let str = getStrVal textField
    in  if str == "" then clearAnswer questionTag else setAnswer questionTag (Just validate) str




--------- Non-answer views ------------------------------------------------------------


data StyleView = StyleView String (UntypedWebView Database)
   deriving (Eq, Show, Typeable)

mkStyleView :: String -> UntypedWebView Database -> WebViewM Database (WV StyleView)
mkStyleView styleStr wv = mkWebView $
  \vid _ ->
    do { return $ StyleView styleStr wv
       }

instance Presentable StyleView where
  present (StyleView styleStr wv) =
      withStyle styleStr $ present wv

instance Storeable Database StyleView



data TableView = TableView String Bool Bool Bool [[UntypedWebView Database]]
   deriving (Eq, Show, Typeable)


mkTableView :: String -> Bool -> Bool -> Bool -> [[UntypedWebView Database]] -> WebViewM Database (WV TableView)
mkTableView classTag border topHeader leftHeader rows = mkWebView $
  \vid _ ->
    do { return $ TableView classTag border topHeader leftHeader rows
       }

instance Presentable TableView where
  present (TableView classTag hasBorder topHeader leftHeader rows) =
    mkTableEx [border $ if hasBorder then "1" else "0", class_ $ "FormTable Table"++classTag ] [] [] $ 
      [ [ ([ class_ $ "TableCell " ++ (if rowNr == 1 then " TopRow" else "") ++
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




-- put these here due to Template-Haskell scope restrictions
deriveInitial ''StyleView
deriveMapWebViewDb ''Database ''StyleView

deriveInitial ''TableView
deriveMapWebViewDb ''Database ''TableView


mkViewFormElt :: FormElt -> WebViewM Database (UntypedWebView Database)
mkViewFormElt (RadioAnswerElt r)      = mkUntypedWebView $ mkRadioAnswerView r
mkViewFormElt (RadioTextAnswerElt rt) = mkUntypedWebView $ mkRadioTextAnswerView rt
mkViewFormElt (ButtonAnswerElt b)     = mkUntypedWebView $ mkButtonAnswerView b
mkViewFormElt (TextAnswerElt t)       = mkUntypedWebView $ mkTextAnswerView t
mkViewFormElt (HtmlElt html)          = mkUntypedWebView $ mkHtmlView html
mkViewFormElt (HtmlFileElt path)      = mkUntypedWebView $ mkHtmlTemplateView ("Webforms/"++path) []
mkViewFormElt (StyleElt styleStr elt) = 
 do { wv <- mkViewFormElt elt
    ; mkUntypedWebView $ mkStyleView styleStr wv
    }
mkViewFormElt (TableElt classTag border topHeader leftHeader rows) = 
  do { wvs <- mapM (mapM mkViewFormElt) rows
     ; mkUntypedWebView $ mkTableView classTag border topHeader leftHeader wvs
     }


data FormPageView = 
  FormPageView [UntypedWebView Database] String
    deriving (Eq, Show, Typeable)

deriveInitial ''FormPageView

deriveMapWebViewDb ''Database ''FormPageView

mkFormPageView :: Int -> FormPage -> WebViewM Database (WV FormPageView)
mkFormPageView nr (Page elts) = mkWebView $
  \vid _ ->
    do { pageViews <- mapM mkViewFormElt elts
       ; return $ FormPageView pageViews $ jsScript
           [ "initProgressMarkers();"
           , "dlog('"++show nr++"');" ] -- todo: need to put the nr in the script because otherwise
                                        -- the incrementality prevents the script from being executed.
                                        -- The current script model is not okay! 
       }

instance Presentable FormPageView where
  present (FormPageView wvs script) =
    (vList $ map present wvs) +++ mkScript script

instance Storeable Database FormPageView

data FormView = 
  FormView Bool Int Int (Widget (Button Database)) (Widget (Button Database)) (Widget (Button Database)) 
           (Widget (Button Database)) (Widget (Button Database)) (WV FormPageView)
    deriving (Eq, Show, Typeable)

deriveInitial ''FormView

deriveMapWebViewDb ''Database ''FormView

mkFormView :: WebForm -> WebViewM Database (WV FormView)
mkFormView form@(Form pages) = mkWebView $
  \vid FormView{} ->
    do { modifyDb $ initializeDb form
       ; args <- getHashArgs
       ; let currentPageNr = case args of
                             ("p", nrStr):_ | Just nr <- readMaybe nrStr  -> if nr < length pages then nr - 1 else length pages - 1
                             _                                            -> 0 
             currentPage = pages!!currentPageNr
             
       ; pageView <- mkFormPageView currentPageNr currentPage
       ; db <- getDb
       --; liftIO $ putStrLn $ "Db is "++show db
       ; let isComplete = all isQuestionAnswered $ Map.elems db
       ; clearButton <- mkButton "Alles wissen" True $ confirmEdit "Weet u zeker dat u alle antwoorden wilt wissen?" 
                                                       clearForm
       ; let mkPrevButton = mkButtonWithClick "Vorige" (currentPageNr/=0) $ \_ -> gotoPageNr (currentPageNr - 1)
             isLastPage = currentPageNr == length pages - 1
             mkNextButton = if isLastPage
                            then mkButton "Opsturen" isComplete $ confirmEdit "Weet u zeker dat u de antwoorden wilt versturen?"
                                                                  sendForm
                            else mkButtonWithClick "Volgende" (getQuestionsAnsweredFormPage currentPage db) $
                                   \_ -> gotoPageNr (currentPageNr + 1) 
       ; prevButton1 <- mkPrevButton
       ; prevButton2 <- mkPrevButton
       ; nextButton1 <- mkNextButton
       ; nextButton2 <- mkNextButton
       
       ; return $ FormView isComplete currentPageNr (length pages) prevButton1 prevButton2 nextButton1 nextButton2 clearButton pageView
       }
 where gotoPageNr nr = "setHashArg('p', '"++show (1+ nr)++"');" -- nr is 0-based

       clearForm :: EditM Database ()
       clearForm = 
        do { modifyDb $ \db -> Map.empty
           ; evalJSEdit [gotoPageNr 0]
           }
                             
       sendForm :: EditM Database ()
       sendForm = -- very very basic save to csv (no check for column validity, column order based on sort, etc.)
        do { db <- getDb
           ; fileExists <- io $ doesFileExist resultsFilepath
           
           ; when (not fileExists) $
                  io $ writeFile resultsFilepath $ intercalate "," $ map show $ Map.keys db
           ; io $ appendFile resultsFilepath $ "\n" ++
                    intercalate "," [ show answer | Just (_, answer) <- Map.elems db ]
           
           ; clearForm
           }
       
instance Presentable FormView where
  present (FormView isComplete currentPageNr nrOfPages prevButton1 prevButton2 nextButton1 nextButton2 clearButton wv) =
      with [class_ $ "FormPage", style "background: white;", align "left"] $ -- dimensions are specified in css to allow iPad specific style
        with [ align "right", style "margin-bottom:40px"] (mkPageHeader prevButton1 nextButton1) +++
        vList [ present wv
              , vSpace 40
              , hStretchList [ E $ present clearButton, space
                             , E $ with [ onclick (toValue $ jsNavigateTo $ "'"++resultsFilepath++"'") -- use onclick to avoid problem with getting <a> underlined.
                                        , style "font-size: 80%; color: blue; text-decoration: underline; cursor: pointer"] $  "Resultaten downloaden"
                             , space
                             , E $ mkPageHeader prevButton2 nextButton2 ]
              -- Decrease FormPage bottom-padding to 20px when enabling this.
              --, vSpace 70
              --, hStretchList [ space, E $ with [style "font-size: 11px; font-style: italic"] "Powered by WebViews" ]
              ]
   where mkPageHeader prevButton nextButton =
                          hListCenter 
                                [ present prevButton
                                , with [style "font-size: 80%"] $ nbsp >> (primHtml $ "Pagina&nbsp;"++show (currentPageNr+1) ++"/"++show nrOfPages) >> nbsp
                                , with [class_ "NextButton"] $ present nextButton ] 
      --  +++
          

instance Storeable Database FormView

data MainView = MainView (WV FormView) deriving (Eq, Show, Typeable)
  
instance Initial MainView where                 
  initial = MainView initial

mkMainView form = mkWebView $
 \vid (MainView _) ->
  do { formView <- mkFormView form
     ; return $ MainView formView
     }

instance Presentable MainView where
  present (MainView fv) = mkPage [style "background: url('/img/background_linen.png') repeat scroll center top transparent; min-height: 100%"] $
                            with [class_ $ "MainPage"] $
                            present fv 

instance Storeable Database MainView where
     

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

getQuestionsAnsweredFormPage :: FormPage -> Database -> Bool
getQuestionsAnsweredFormPage (Page elts) db = and [ getQuestionsAnsweredFormElt e db | e <- elts ]

getQuestionsAnsweredFormElt :: FormElt -> Database -> Bool
getQuestionsAnsweredFormElt (RadioAnswerElt r)      db = isQuestionTagAnswered (getRadioQuestionTag r) db
getQuestionsAnsweredFormElt (RadioTextAnswerElt r)  db = isQuestionTagAnswered (getRadioTextRadioQuestionTag r) db && 
                                                         isQuestionTagAnswered (getRadioTextTextQuestionTag r) db
getQuestionsAnsweredFormElt (ButtonAnswerElt b)     db = isQuestionTagAnswered (getButtonQuestionTag b) db
getQuestionsAnsweredFormElt (TextAnswerElt t)       db = isQuestionTagAnswered (getTextQuestionTag t) db
getQuestionsAnsweredFormElt (HtmlElt html)          db = True
getQuestionsAnsweredFormElt (HtmlFileElt path)      db = True
getQuestionsAnsweredFormElt (StyleElt _ elt)        db = getQuestionsAnsweredFormElt elt db
getQuestionsAnsweredFormElt (TableElt _ _ _ _ rows) db = and [ getQuestionsAnsweredFormElt elt db 
                                                             | row <- rows, elt <- row ]
                       --compose $ concatMap (map getQuestionsAnsweredFormElt) rows


---- Main (needs to be below all webviews that use deriveInitial)

deriveMapWebViewDb ''Database ''MainView

main :: IO ()
main = server 8100 "Blij van IT" rootViews ["WebForms.js", "WebForms.css", "BlijVanIT.css"] "WebFormDB.txt" mkInitialDatabase $ Map.empty

rootViews :: RootViews Database
rootViews = [ mkRootView "" $ mkMainView mainForm
            , mkRootView "form" $ mkMainView mainForm
            , mkRootView "embedded" $ mkFormView mainForm
            ] 
