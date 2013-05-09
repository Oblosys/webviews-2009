{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
module ClientWebView where

import Data.List
import BlazeHtml hiding (time)
import Data.Generics
import Data.Char
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap 
import Debug.Trace
import System.Time
import Data.Time.Calendar
import Types
import Utils
import Generics
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server
import System.IO.Unsafe (unsafePerformIO) -- just for calendar stuff
import TemplateHaskell

import Database
import ReservationUtils

-- Extra indirection, so we can style client views that are not part of the combined view or embedded in an iFrame.
data ClientWrapperView = ClientWrapperView (WV ClientView) deriving (Eq, Show, Typeable)
  
instance Initial ClientWrapperView where                 
  initial = ClientWrapperView initial

mkClientWrapperView = mkWebView $
 \vid (ClientWrapperView _) ->
  do { clientView <- mkClientView
     ; return $ ClientWrapperView clientView
     }

instance Presentable ClientWrapperView where
  present (ClientWrapperView fv) = mkPage [] $
                            with [class_ $ "ClientWrapperView"] $
                            present fv 

instance Storeable Database ClientWrapperView where


data ClientView = 
  ClientView Int (Maybe Date) (Maybe Time) [Widget (Button Database)] (Widget (TextView Database)) (Widget (TextView Database)) (Widget (Button Database)) (Widget (Button Database)) [Widget (Button Database)] [[Widget (Button Database)]] (Widget (Button Database)) 
  (Widget (LabelView Database)) (Widget (LabelView Database)) (Widget (LabelView Database)) (Widget (EditAction Database))
  String
    deriving (Eq, Show, Typeable)
    
setClientViewNrOfPeople np (ClientView _ b c d e f g h i j k l m n o p) = (ClientView np b c d e f g h i j k l m n o p) 
setClientViewDate md (ClientView a _ c d e f g h i j k l m n o p) = (ClientView a md c d e f g h i j k l m n o p) 
setClientViewTime mt (ClientView a b _ d e f g h i j k l m n o p) = (ClientView a b mt d e f g h i j k l m n o p)
 
instance Initial ClientView where                 
  initial = ClientView initial initial initial initial initial initial initial initial initial initial initial initial initial initial initial initial

maxNrOfPeople = 10

mkClientView = mkWebView $
 \vid (ClientView oldNrOfP oldMDate oldMTime _ oldNameText oldCommentText _ _ _ _ _ _ _ _ _ _) ->
  do { clockTime <-  liftIO getClockTime -- this stuff is duplicated
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let today@(currentDay, currentMonth, currentYear) = dateFromCalendarTime ct
           now   = (ctHour ct, ctMin ct)
           
     --; let (initialDate, initialTime) = (Nothing, Nothing)
     ; let (initialDate, initialTime) = (Just today, Just (19,30))
     
     ; let nrOfP = if oldNrOfP == 0 then 2 else oldNrOfP -- todo init is not nice like this
     ; let mDate = maybe initialDate Just oldMDate -- todo init is not nice like this
     
     ; reservations <- fmap Map.elems $ withDb $ \db -> allReservations db
     ; let timeAvailable tm = case mDate of
                                   Nothing -> True
                                   Just dt ->
                                     let reservationsAtTimeAndDay = filter (\r-> dt == date r && tm == time r) reservations
                                     in  sum (map nrOfPeople reservationsAtTimeAndDay) + nrOfP <= maxNrOfPeople
     ; let availableAtDateAndTime dt tm = let reservationsAtTimeAndDay = filter (\r-> dt == date r && tm == time r) reservations
                                          in  maxNrOfPeople - sum (map nrOfPeople reservationsAtTimeAndDay) + nrOfP

     ; let mTime = case oldMTime of
                     Nothing -> initialTime
                     Just oldTime -> if timeAvailable oldTime then oldMTime else Nothing
     
     ; nrButtons <- sequence [ mkButtonWithClick (show nr) True $ \bvid -> jsCallFunction vid "setNr" [show nr]
                                  {- $ Edit $ viewEdit vid $ setClientViewNrOfPeople nr -} | nr <-[1..8]]
     
       -- TODO hacky
     ; nameText  <- mkTextField $ getStrVal (oldNameText) -- needed, even though in browser text is reused without it
     ; commentText  <- mkTextArea $ getStrVal (oldCommentText) -- at server side it is not
     
     ; todayButton <- mkButtonWithStyleClick ("Today ("++show currentDay++" "++showShortMonth currentMonth++")") True "width:100%" $ const ""  {-Edit $ viewEdit vid $ setClientViewDate (Just today) -} 
     ; tomorrowButton <- mkButtonWithStyleClick "Tomorrow" True "width:100%" $ const "" {- Edit $ viewEdit vid $ setClientViewDate (Just $ addToDate today 1) -}
     ; dayButtons <- sequence [ mkButtonWithClick (showShortDay . weekdayForDate $ dt) True $ const "" {- Edit $ viewEdit vid $ setClientViewDate (Just dt) -}
                              | day <-[2..7], let dt = addToDate today day ]
     
     -- todo: cleanup once script stuff is in monad (becomes  sequence [ sequence [ mkButton; script ..] ]

     ; timeButtonssTimeEditss <- 
         sequence [ sequence [ do { b <- mkButtonWithStyleClick (showTime tm) (timeAvailable tm) "width:100%" $ const "" 
                                                {- Edit $ viewEdit vid $ setClientViewTime (Just tm) -}
                                  ; return (b, onClick b $ jsCallFunction vid "setTime" [ "{hour:"++show hr ++",min:"++show mn++",index:"++show ix++"}"])
                                  }     
                             | (mn,j) <-zip [0,30] [0..], let tm = (hr,mn), let ix = 2*i+j ]
                  | (hr,i) <-  zip [18..23] [0..] ] 
     ; let (timeButtonss, timeEditss) = unzip . map unzip $ timeButtonssTimeEditss
     ; let timeEdits = concat timeEditss  -- TODO, we want some way to do getStr on both labels and Texts 
     ; nrOfPeopleLabel <- mkLabelView "nrOfPeopleLabel" 
     ; dateLabel <- mkLabelView "dateLabel" 
     ; timeLabel <- mkLabelView "timeLabel" 
      
     ; confirmButton <- mkButtonEx "Confirm" True {- (isJust mDate && isJust mTime)-} "width: 100%" (const "") $ return ()
     ; submitAction <- mkEditActionEx $ \args@[nameStr, nrOfPeopleStr, dateIndexStr, timeStr, commentStr] ->
        do { debugLn $ "submit action executed "++show args
           ;
           -- todo constructing date/time from indices duplicates work
           ; debugLn $ "Values are "++nameStr++" "++dateIndexStr++" "++timeStr
           ; let nrOfPeople = read nrOfPeopleStr
           ; debugLn $ "nrOfPeople "++show nrOfPeople
           ; let date = addToDate today (read dateIndexStr)
           ; debugLn $ "date "++show date

           ; let time = read timeStr
           ; debugLn $ "time "++show time
           
           ; modifyDb $ \db -> 
                let (Reservation rid _ _ _ _ _, db') = newReservation db
                    db'' = updateReservation rid (const $ Reservation rid date time nameStr nrOfPeople commentStr) db
                in  db''
           }
     ; let datesAndAvailabilityArray = 
             "["++ intercalate "," [ "{date: \""++showDay (weekdayForDate  d)++", "++showShortDate d++"\","++
                                     " availables: ["++intercalate "," [ show $ availableAtDateAndTime d (h,m)
                                                                       | h<-[18..23], m<-[0,30]]++"]}" | i <-[0..7], let d = addToDate today i ] ++ 
             "];"
     
     ; return $ ClientView nrOfP mDate mTime nrButtons nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton 
                           nrOfPeopleLabel dateLabel timeLabel submitAction
                  $ jsScript --"/*"++show (ctSec ct)++"*/" ++
                  [ "availability = "++datesAndAvailabilityArray -- forces script to run when it changes
                    -- maybe combine these with button declarations to prevent multiple list comprehensions
                    -- maybe put script in monad and collect at the end, so we don't have to separate them so far (script :: String -> WebViewM ())
                  , concat [ onClick nrButton $ jsCallFunction vid "setNr" [show nr]| (nr,nrButton) <- zip [1..] nrButtons]
                  , concat [ onClick button $ jsCallFunction vid "setDate" [ show dateIndex ] 
                           | (dateIndex,button) <- zip [0..] $ [todayButton, tomorrowButton]++dayButtons ]
                  , concat timeEdits
                  , inertTextView nameText
                  , inertTextView commentText
                  , onClick confirmButton $ intercalate ";" 
                                          [ callServerEditAction submitAction ["escape("++jsGetElementByIdRef (widgetGetViewRef nameText)++".value)", jsVar vid "selectedNr"
                                                                              , jsVar vid "selectedDate", "'('+"++jsVar vid "selectedTime"++".hour+','+"++jsVar vid "selectedTime"++".min+')'"
                                                                              ,"escape("++jsGetElementByIdRef (widgetGetViewRef commentText)++".value)"]
                                          , jsCallFunction vid "reset" []
                                          , jsCallFunction vid "disenable" []
                                          , "alertDialog('Your reservation has been confirmed.')"]
                  , jsFunction vid "reset" []
                                          [ jsCallFunction vid "setNr" ["null"]
                                          , jsCallFunction vid "setDate" ["null"]
                                          , jsCallFunction vid "setTime" ["null"]
                                          , jsGetElementByIdRef (widgetGetViewRef nameText)++".value = \"\""
                                          , jsGetElementByIdRef (widgetGetViewRef commentText)++".value = \"\""
                                          ] 
                  , jsFunction vid "inputValid" [] [ "console.log(\"inputValid: \"+"++jsGetElementByIdRef (widgetGetViewRef nameText)++".value)"
                                                   , "return "++jsVar vid "selectedNr"++"!=null && "++jsVar vid "selectedDate"++"!=null && "++jsVar vid "selectedTime"++"!=null && "++
                                                                jsGetElementByIdRef (widgetGetViewRef nameText)++".value != \"\""
                                                   ] 
                  , jsFunction vid "setNr" ["nr"] [ "console.log(\"setNr (\"+nr+\") old val: \", "++jsVar vid "selectedNr"++")"
                                                  , jsAssignVar vid "selectedNr" "nr"
                                                  , "nrStr = nr==null ? \"Please select nr of people:\" : 'Nr of people: '+nr"
                                                  , jsGetElementByIdRef (widgetGetViewRef nrOfPeopleLabel)++".innerHTML = nrStr"
                                                  , "$('.nrButtons button').attr('selected',false)"
                                                  , "if (nr!=null) $('.nrButtons button').eq(nr-1).attr('selected',true)"
                                                  , jsCallFunction vid "disenable" [] ]
                  , jsFunction vid "setTime" ["time"] [ "console.log(\"setTime \"+time, "++jsVar vid "selectedTime"++")"
                                                      , jsAssignVar vid "selectedTime" "time"
                                                      , "timeStr = time==null ? \"Please select a time:\" : 'Time: ' + time.hour +\":\"+ (time.min<10?\"0\":\"\") + time.min"
                                                      , jsAssignVar vid "selectedTimeIndex" "time == null ? null : time.index"
                                                      , jsGetElementByIdRef (widgetGetViewRef timeLabel)++".innerHTML = timeStr"
                                                      , "$('.timeButtons button').attr('selected',false)"
                                                      , "if (time!=null) $('.timeButtons button').eq(time.index).attr('selected',true)"
                                                      , jsCallFunction vid "disenable" [] ] 
                  , jsFunction vid "setDate" ["dateIx"] [ "console.log(\"setDate \"+dateIx, "++jsVar vid "selectedDate"++")"
                                                      , jsAssignVar vid "selectedDate" "dateIx"
                                                      , "dateStr = dateIx==null ? \"Please select a date:\" : availability[dateIx].date"
                                                      , jsGetElementByIdRef (widgetGetViewRef dateLabel)++".innerHTML = dateStr"
                                                      , "$('.dateButtons button').attr('selected',false)"
                                                      , "if (dateIx!=null) $('.dateButtons button').eq(dateIx).attr('selected',true)"
                                                      , jsCallFunction vid "disenable" [] ] 
                  , jsFunction vid "disenable" [] [ "console.log(\"disenable: \","++jsVar vid "selectedNr"++","++jsVar vid "selectedDate"++","++jsVar vid "selectedTime" ++" )"
                                                  , "var availables = "++jsVar vid "selectedDate"++" == null ? null : availability["++jsVar vid "selectedDate"++"].availables"
                                                  , "var buttonIds = [\""++intercalate "\",\"" (map (show . getViewId) $ concat timeButtonss)++"\"]"
                                                  , jsFor "i=0;i<buttonIds.length;i++" $ 
                                                      [ "document.getElementById(buttonIds[i]).disabled = availables == null ? true : availables[i]<"++jsVar vid "selectedNr"
                                                      ]
                                                  , jsIf (jsVar vid "selectedTimeIndex"++" && availables && availables["++jsVar vid "selectedTimeIndex"++"] <"++jsVar vid "selectedNr") 
                                                      [ jsCallFunction vid "setTime" ["null"] ] -- if the selected time becomes unavailable, deselect
                                                      -- TODO this deselect is implemented horribly, with the extra index field in time. Can this be done more elegantly?
                                                  , jsGetElementByIdRef (widgetGetViewRef confirmButton)++".disabled = !"++jsCallFunction vid "inputValid" []
                                                  ]
                    -- todo: handle when time selection is disabled because of availability (on changing nr of persons or date)
                  , onKeyUp nameText $ jsCallFunction vid "disenable" []
                  , jsDeclareVar vid "selectedNr" "null"
                  , jsDeclareVar vid "selectedTime" "null"
                  , jsDeclareVar vid "selectedTimeIndex" "null"
                  , jsDeclareVar vid "selectedDate" "null"
                  , "if ("++jsVar vid "selectedNr"++"==null) "++jsCallFunction vid "reset" []
                  -- only reset (and initialize) if selectedNumber hasn't been set yet.
                  -- TODO: we're doing far too much on each refresh. Attaching handlers etc. should also be done only once
                  , jsCallFunction vid "disenable" []
                  ]   
     }


-- todo comment has hard-coded width. make constant for this
instance Presentable ClientView where
  present (ClientView nrOfP mDate mTime nrButtons nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton
                      nrOfPeopleLabel dateLabel timeLabel _ script) =
    with [ style "font-family:arial", class_ "ClientView" ] $  
    vList [ hListEx [width "100%"] [ "Name:", present nameText] -- todo: buggy on iPhone, space is added between Name and text, but not if "Name:" is replaced by "x"
          , vSpace 7
          , present nrOfPeopleLabel
          , hListEx [class_ "nrButtons", width "100%"] $ map present nrButtons
          , vSpace 7
          --, stringToHtml $ maybe "Please choose a date:" (\d -> (showDay . weekdayForDate $ d) ++ ", " ++ showShortDate d) mDate
          , present dateLabel
          , hListEx [class_ "dateButtons", width "100%"] [ present todayButton, present tomorrowButton]
          , hListEx [class_ "dateButtons", width "100%"] $ map present dayButtons
          , vSpace 7
          , present timeLabel
          --, stringToHtml $ maybe "Please select a time:" (\d -> showTime d) mTime
          , simpleTable [class_ "timeButtons", width "100%",cellpadding "0", cellspacing "0"] [] $ map (map present) timeButtonss
          , vSpace 7
          , "Comments:"
          , present commentText
          , vSpace 7
          , present confirmButton ] +++   
          mkScript script
 
instance Storeable Database ClientView where
  save _ = id

--- Utils

showMonth m = show (toEnum (m-1) :: System.Time.Month)
showDate (d,m,y) = show d ++ " " ++ showMonth m ++ " " ++ show y
showShortDate (d,m,y) = show d ++ " " ++ showShortMonth m ++ " " ++ show y
showTime (h,m) = (if h<10 then " " else "") ++ show h ++ ":" ++ (if m<10 then "0" else "") ++ show m

daysToWeeks days = if length days < 7 then [days]
                   else take 7 days : daysToWeeks (drop 7 days)

-- TODO: before using seriously, check unsafePerformIO (or remove)
calendarTimeForDate (day, month, year) =
 do { clockTime <-  getClockTime -- first need to get a calendar time in this time zone (only time zone is used, so ok to unsafePerformIO)
    ; today <- toCalendarTime clockTime
    ; return $ today {ctDay= day, ctMonth = toEnum $ month-1, ctYear = year, ctHour = 12, ctMin = 0, ctPicosec = 0}
    }
    
dateFromCalendarTime ct = (ctDay ct, 1+fromEnum (ctMonth ct), ctYear ct)
    
weekdayForDate date = unsafePerformIO $ -- unsafePerformIO is okay, since we don't use the current time/date anyway
 do { ct <- calendarTimeForDate date
    ; ctWithWeekday <- toCalendarTime $ toClockTime ct
    ; return $ (fromEnum (ctWDay ctWithWeekday) -1) `mod` 7 + 1 -- in enum, Sunday is 0 and Monday is 1, we want Monday = 1 and Sunday = 7
    } 

addToDate date days = unsafePerformIO $ -- unsafePerformIO is okay, since we don't use the current time/date anyway
 do { ct <- calendarTimeForDate date
    ; ct' <- toCalendarTime $ addToClockTime (noTimeDiff {tdDay = days}) $ toClockTime ct 
    ; return $ dateFromCalendarTime ct' 
    }
    
showDay :: Int -> String 
showDay d = ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]!!(d-1) 

showShortDay :: Int -> String 
showShortDay d = ["Mo","Tu","We","Th","Fr","Sa","Su"]!!(d-1) 

showShortMonth :: Database.Month -> String
showShortMonth m = ["Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.","Sept.", "Oct.", "Nov.", "Dec."]!!(m-1)


deriveMapWebViewDb ''Database ''ClientView
deriveMapWebViewDb ''Database ''ClientWrapperView