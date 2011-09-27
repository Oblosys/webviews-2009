{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses #-}
module Main where

import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
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
import Generics
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server
import System.IO.Unsafe (unsafePerformIO) -- just for calendar stuff
import Reservations.Database

main :: IO ()
main = server rootViews "ReservationsDB.txt" theDatabase users


rootViews :: [ (String, Int -> WebViewM Database (WebView Database))]
rootViews = [ ("", mkMainRootView), ("client", \sessionId -> mkClientView), ("restaurant", \sessionId -> mkRestaurantView)] 
  -- TODO: id's here?
  -- TODO: fix the sessionId stuff
  -- TODO: find good names for root, main, etc.

 -- TODO: sessionId? put this in an environment? or maybe the WebViewM monad?
    
-- Main ----------------------------------------------------------------------  

data MainView = 
  MainView (WebView Database) (WebView Database)
    deriving (Eq, Show, Typeable, Data)
  
instance Initial MainView where                 
  initial = MainView initial initial


mkMainRootView sessionId = mkWebView $
 \vid (MainView _ _) ->
  do { clientView <- mkClientView
     ; restaurantView <- mkRestaurantView                                            
     ; return $ MainView clientView restaurantView
     }

appBgColor = Rgb 0xf8 0xf8 0xf8
instance Presentable MainView where
  present (MainView cv rv) = 
     hListEx [] [ with [thestyle "font-family:arial"] $ roundedBoxed (Just $ appBgColor) $ present cv
                , hSpace 30
                , with [thestyle "font-family:arial"] $ roundedBoxed (Just $ appBgColor) $ present rv ] 

instance Storeable Database MainView where
  save _ = id
     

-- Main ----------------------------------------------------------------------  

data RestaurantView = 
  RestaurantView (Maybe Date) (Maybe Int) (Maybe Reservation) [[(WebView Database,EditAction Database)]] (WebView Database) (WebView Database) (WebView Database)
    deriving (Eq, Show, Typeable, Data)

instance Initial RestaurantView where                 
  initial = RestaurantView initial initial initial [] initial initial initial

mkRestaurantView = mkWebView $
 \vid (RestaurantView mSelectedDate mSelectedHour mSelectedReservation _ _ _ _) ->
  do { clockTime <-  liftIO getClockTime
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let today@(currentDay, currentMonth, currentYear) = dateFromCalendarTime ct
           now   = (ctHour ct, ctMin ct)
           (lastMonth, lastMonthYear) = if currentMonth == 1 then (12,currentYear-1) else (currentMonth-1,currentYear)
           (nextMonth, nextMonthYear) = if currentMonth == 12 then (1,currentYear+1) else (currentMonth+1,currentYear)
           nrOfDaysInLastMonth = gregorianMonthLength (fromIntegral lastMonthYear) lastMonth 
           nrOfDaysInThisMonth = gregorianMonthLength (fromIntegral currentYear) currentMonth
           firstDayOfMonth = weekdayForDate (1, 1+fromEnum (ctMonth ct), ctYear ct)
     
     ; let daysOfLastMonth = reverse $ take (firstDayOfMonth-1) [nrOfDaysInLastMonth,nrOfDaysInLastMonth-1..]
     ; let daysOfThisMonth = [1..nrOfDaysInThisMonth]
     ; let daysOfNextMonth = take (7 - ((length daysOfLastMonth + length daysOfThisMonth) `mod` 7)) [1..]
     ; let calendarDaysOfLastMonth = [(d,lastMonth, lastMonthYear) | d <- daysOfLastMonth] 
     ; let calendarDaysOfThisMonth = [(d,currentMonth, currentYear) | d <- daysOfThisMonth]
     ; let calendarDaysOfNextMonth = [(d,nextMonth, nextMonthYear) | d <- daysOfNextMonth]
     ; let calendarDays = calendarDaysOfLastMonth ++ calendarDaysOfThisMonth ++ calendarDaysOfNextMonth 
     ; selects <- mapM (selectDateEdit vid) calendarDays
     
     ; reservations <- fmap Map.elems $ withDb $ \db -> allReservations db
     
     ; let calendarDayViewForDate date@(_,m,_) = mkCalendarDayView date (Just date==mSelectedDate) (date==today) (m==currentMonth) $
                                           [r | r@(Reservation _ d _ _ _ _) <- reservations, date==d]  
     
     ; calendarDayViews <- mapM calendarDayViewForDate calendarDays
     
     ; let reservationsSelectedDay = filter ((==mSelectedDate). Just . date) reservations
     ; let reservationsSelectedHour = filter ((==mSelectedHour). Just . fst . time) reservationsSelectedDay
      
     ; let weeks = daysToWeeks $ zip calendarDayViews selects
     
     ; -- hack
     ; let (mSelectedDate', mSelectedHour') = 
             case mSelectedDate of
                Just selectedDate -> (mSelectedDate, mSelectedHour)
                Nothing           -> let resHours = sort $ map (fst . time) $  filter ((==today) . date) reservations
                                         initHr = if null resHours then Just 19 else Just $ head resHours 
                                     in  (Just today, initHr)

     -- if the current selection is in the selected hour, keep it, otherwise select the first reservation of the hour (if any)
     ; let mSelectedReservation' = case reservationsSelectedHour of
                                          fstRes:_  -> case mSelectedReservation of
                                                              Just r | r `elem` reservationsSelectedHour -> Just r
                                                              _                                          -> Just fstRes
                                          _      -> Nothing

     ; dayView <- mkDayView vid mSelectedHour' reservationsSelectedDay
     ; hourView <- mkHourView vid mSelectedReservation' mSelectedHour' reservationsSelectedHour
     ; reservationView <- mkReservationView mSelectedReservation'

     
     ; return $ RestaurantView mSelectedDate' mSelectedHour' mSelectedReservation' weeks dayView hourView reservationView
     }
 where selectDateEdit vid d = mkEditAction . Edit $
        do { (_,_,db,_,_) <- get
           ; viewEdit vid $ 
              \(RestaurantView _ h r weeks dayView hourView reservationView) ->
                let resHours = sort $ map (fst . time) $ filter ((==d). date) $ Map.elems $ allReservations db
                    newHr = if null resHours then h else Just $ head resHours 
                in  RestaurantView (Just d) newHr r weeks dayView hourView reservationView
           }

getWebViewId (WebView vid _ _ _ _) = vid

{- day: mark today
mark different months, mark appointments
 -}
instance Presentable RestaurantView where
  present (RestaurantView mSelectedDate mSelectedHour mSelectedReservation weeks dayView hourView reservationView) = 
    vList $ 
      [ with [thestyle "text-align:center; font-weight:bold"] $ stringToHtml $ maybe "no selection" (\(_,m,y)->showMonth m ++ " "++show y) mSelectedDate
      , vSpace 5
      , mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; text-align:center"] [] [thestyle "border: 1px solid #909090"]  
                  (header :
                   [ [ ([withEditActionAttr selectionAction], present dayView) 
                     | (dayView, selectionAction) <- week] 
                   | week <- weeks ]
                   )
      ] ++
      [ present dayView
      , vSpace 15
      , with [thestyle "font-size:80%"] $
          case (mSelectedDate, mSelectedHour) of
            (Just (d,m,_), Just selectedHour) -> stringToHtml $ "Reservations on "++show d++" "++showMonth m ++
                                                                     " between "++show selectedHour ++ "h and "++show (selectedHour+1)++"h"
            _                                      -> nbsp
      , vSpace 6
      , present hourView
      , vSpace 15
      , present reservationView 
      ]
   where header = [ ([], stringToHtml $ map toLower $ showShortDay d) | d <- [1..7] ] 
instance Storeable Database RestaurantView where
  save _ = id
   
-- CalendarDay ----------------------------------------------------------------------  
     
     
data CalendarDayView = 
  CalendarDayView Date Bool Bool Bool [Reservation]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial CalendarDayView where                 
  initial = CalendarDayView (initial, initial, initial) initial initial initial initial

mkCalendarDayView date isSelected isToday isThisMonth reservations = mkWebView $
 \vid (CalendarDayView _ _ _ _ _) ->
  do { return $ CalendarDayView date isSelected isToday isThisMonth reservations
     }
     
selectedDayColor = Rgb 0x80 0xb0 0xff

instance Presentable CalendarDayView where
  present (CalendarDayView date@(day, month, year) isSelected isToday isThisMonth reservations) = 
    withBgColor (if isToday then Rgb 0x90 0x90 0x90 else if isSelected then selectedDayColor else Rgb 240 240 240) $
    -- we use a margin of 3 together with the varying cell background to show today
    -- doing this with borders is awkward as they resize the table
      mkTableEx [ width "40px", height 40, cellpadding 0, cellspacing 0 
                , thestyle $ "margin:2px; background-color:" ++ if isSelected then htmlColor selectedDayColor else htmlColor (Rgb 240 240 240) ] [] [] 
        [[ ([valign "top"] ++ if isThisMonth then [] else [thestyle "color: #808080"], stringToHtml (show day)) ]
        ,[ ([thestyle "font-size:80%; color:#0000ff"], if not $ null reservations then stringToHtml $ show (length reservations) ++ " (" ++ show (sum $ map nrOfPeople reservations)++")" else nbsp) ] 
        ]
    -- TODO: make Rgb for standard html colors, make rgbH for (rgbH 0xffffff)
    
    -- check slow down after running for a while

instance Storeable Database CalendarDayView where
  save _ = id

-----------------------------------------------------------------------------

data DayView = 
  DayView (Maybe Int) [EditAction Database] [Reservation]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial DayView where                 
  initial = DayView initial initial initial

mkDayView :: ViewId -> Maybe Int -> [Reservation] -> WebViewM Database (WebView Database)
mkDayView restaurantViewId mSelectedHour dayReservations = mkWebView $
 \vid (DayView _ _ _) ->
  do { selectHourActions <- mapM (selectHourEdit restaurantViewId) [18..24]
     ; return $ DayView mSelectedHour selectHourActions dayReservations 
     }
 where selectHourEdit vid h = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView d _ r weeks dayView hourView reservationView) -> RestaurantView d (Just h) r weeks dayView hourView reservationView
 
instance Presentable DayView where
  present (DayView mSelectedHour selectHourActions dayReservations) =
    mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-size:80%"] [] [] 
      [[ ([withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: "++
                                             if Just hr==mSelectedHour then htmlColor selectedDayColor else "#d0d0d0"]
         , presentHour hr) | (sa,hr) <- zip selectHourActions [18..24] ]]

--    mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; text-align:center"] [] [thestyle "border: 1px solid #909090"]  
   where presentHour hr = --withBgColor (Rgb 255 255 0) $
           vListEx [width "40px"] -- needs to be smaller than alotted hour cell, but larger than width of "xx(xx)"
                 [ stringToHtml $ show hr++"h"
                 , with [thestyle "font-size:80%; text-align:center; color:#0000ff"] $ 
                     let ressAtHr = filter ((==hr) . fst . time) dayReservations
                     in  if not $ null ressAtHr then stringToHtml $ show (length ressAtHr) ++ " (" ++ show (sum $ map nrOfPeople ressAtHr)++")" else nbsp 
                 ]
instance Storeable Database DayView where
  save _ = id

-----------------------------------------------------------------------------

data HourView = 
  HourView (Maybe Reservation) (Maybe Int) [EditAction Database] [Reservation]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial HourView where                 
  initial = HourView initial initial initial initial

mkHourView :: ViewId -> Maybe Reservation -> Maybe Int -> [Reservation] -> WebViewM Database (WebView Database)
mkHourView restaurantViewId mSelectedReservation mSelectedHour hourReservations = mkWebView $
 \vid (HourView _ _ _ _) ->
  do { selectReservationActions <- mapM (selectReservationEdit restaurantViewId) hourReservations
     ; return $ HourView mSelectedReservation mSelectedHour selectReservationActions hourReservations 
     }
 where selectReservationEdit vid r = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView d h _ weeks dayView hourView reservationView) -> RestaurantView d h (Just r) weeks dayView hourView reservationView
 
instance Presentable HourView where
  present (HourView mSelectedReservation mSelectedHour selectReservationActions hourReservations) = 
    boxedEx 0 $ with [thestyle "height:90px;overflow:auto"] $
      mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse"] [] [] $ 
        [ [ ([withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: "++
                                                if Just r==mSelectedReservation then htmlColor selectedDayColor else "#f8f8f8"]
            , hList [hSpace 2, stringToHtml $ showTime tm ++ " -   "++nm++" ("++show nr++")"]) ]
        | (sa,r@(Reservation _ _ tm nm nr _)) <- zip selectReservationActions hourReservations 
        ]
instance Storeable Database HourView where
  save _ = id

-----------------------------------------------------------------------------

data ReservationView = 
  ReservationView (Maybe Reservation) (Widget (Button Database))
    deriving (Eq, Show, Typeable, Data)
  
instance Initial ReservationView where                 
  initial = ReservationView initial initial

mkReservationView mReservation = mkWebView $
 \vid (ReservationView _ _) ->
  do { removeButton <- mkButton "x" (isJust mReservation) $ 
         Edit $ docEdit $ maybe id (\res -> removeReservation $ reservationId res ) mReservation
     ; return $ ReservationView mReservation removeButton
     }
 
-- todo comment has hard-coded width. make constant for this
instance Presentable ReservationView where
  present (ReservationView mReservation removeButton) = with [thestyle "background-color:#f0f0f0"] $ boxed $ 
    vListEx [] 
      [ hListEx [width "100%"] [ stringToHtml "Reservation date: ",nbsp
                               , with [colorAttr reservationColor] $ stringToHtml date
                               , with [align "right"] $ present removeButton]
      , hList [stringToHtml "Time:",nbsp, with [colorAttr reservationColor] $ stringToHtml time]
      , hList [stringToHtml "Name:",nbsp, with [colorAttr reservationColor] $ stringToHtml name]
      , hList [stringToHtml "Nr. of people:",nbsp, with [colorAttr reservationColor] $ stringToHtml $ nrOfPeople ]
      , stringToHtml "Comment:" 
      , boxedEx 0 $ with [thestyle $ "padding-left:4px;height:70px; width:300px; overflow:auto; color:" ++ htmlColor reservationColor] $ 
          stringToHtml $ if comment == "" then "<no comment>" else  comment
      ]  
   where (date, time, name, nrOfPeople, comment) =
           case mReservation of
             Just (Reservation _ date time name nrOfPeople comment) -> (showDate date, showTime time, name, show nrOfPeople, comment)
             Nothing                                        -> ("", "", "", "", "") 
         reservationColor = Rgb 0x00 0x00 0xff
 
instance Storeable Database ReservationView where
  save _ = id





data ClientView = 
  ClientView Int (Maybe Date) (Maybe Time) [Widget (Button Database)] (Widget (Text Database)) (Widget (Text Database)) (Widget (Button Database)) (Widget (Button Database)) [Widget (Button Database)] [[Widget (Button Database)]] (Widget (Button Database)) (Widget LabelView)
    String
    deriving (Eq, Show, Typeable, Data)
    
setClientViewNrOfPeople p (ClientView _ b c d e f g h i j k l m) = (ClientView p b c d e f g h i j k l m) 
setClientViewDate md (ClientView a _ c d e f g h i j k l m) = (ClientView a md c d e f g h i j k l m) 
setClientViewTime mt (ClientView a b _ d e f g h i j k l m) = (ClientView a b mt d e f g h i j k l m)
 
instance Initial ClientView where                 
  initial = ClientView initial initial initial initial initial initial initial initial initial initial initial initial initial

maxNrOfPeople = 10

mkClientView = mkWebView $
 \vid (ClientView oldNrOfP oldMDate oldMTime _ oldNameText oldCommentText _ _ _ _ _ _ _) ->
  do { clockTime <-  liftIO getClockTime -- this stuff is duplicated
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let today@(currentDay, currentMonth, currentYear) = dateFromCalendarTime ct
           now   = (ctHour ct, ctMin ct)
           
     ; let nrOfP = if oldNrOfP == 0 then 2 else oldNrOfP -- todo init is not nice like this
     --; let mDate = Just $ maybe today id oldMDate -- todo init is not nice like this
     ; let mDate = oldMDate -- no init
     
     ; reservations <- fmap Map.elems $ withDb $ \db -> allReservations db
     ; let timeAvailable tm = case mDate of
                                   Nothing -> True
                                   Just dt ->
                                     let reservationsAtTimeAndDay = filter (\r-> dt == date r && tm == time r) reservations
                                     in  sum (map nrOfPeople reservationsAtTimeAndDay) + nrOfP <= maxNrOfPeople

     ; let mTime = case oldMTime of
                     Nothing -> Nothing
                     Just oldTime -> if timeAvailable oldTime then oldMTime else Nothing
     
     ; nrButtons <- sequence [ mkButton (show nr) True $ Edit $ viewEdit vid $ setClientViewNrOfPeople nr | nr <-[1..8]]
     
       -- TODO hacky
     ; nameText  <- mkTextField $ getStrVal (oldNameText) -- needed, even though in browser text is reused without it
     ; commentText  <- mkTextArea $ getStrVal (oldCommentText) -- at server side it is not
     
     ; todayButton <- mkButtonWithStyle ("Today ("++show currentDay++" "++showShortMonth currentMonth++")") True "width:100%" $ Edit $ viewEdit vid $ setClientViewDate (Just today)
     ; tomorrowButton <- mkButtonWithStyle "Tomorrow" True "width:100%" $ Edit $ viewEdit vid $ setClientViewDate (Just $ addToDate today 1)
     ; dayButtons <- sequence [ mkButton (showShortDay . weekdayForDate $ dt) True $ Edit $ viewEdit vid $ setClientViewDate (Just dt)
                              | day <-[2..7], let dt = addToDate today day ]
     
                                  
                              
     ; timeButtonss <- sequence [ sequence [ mkButtonWithStyle (showTime tm) (timeAvailable tm) "width:100%" $ Edit $ viewEdit vid $ setClientViewTime (Just tm)
                                           | mn <-[0,30], let tm = (hr,mn) ]
                                | hr <- [18..23] ] 
     
     ; confirmButton <- mkButtonWithStyle "Confirm" (isJust mDate && isJust mTime) "width: 100%" $
         Edit $ do { name <- getTextContents nameText
                   ; comment <- getTextContents commentText
                   ; docEdit $ \db -> 
                        let (Reservation rid _ _ _ _ _, db') = newReservation db
                            db'' = updateReservation rid (const $ Reservation rid (fromMaybe (error "no date") mDate) (fromMaybe (error "no time") mTime) name nrOfP comment) db
                        in  db''
                   }
     ; statusLabel <- mkLabelView "all ok" 
     ; return $ ClientView nrOfP mDate mTime nrButtons nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton statusLabel
                  $ declareFunction "disenable" vid $ "console.log(\"dynamic function executed "++ show (widgetGetViewRef nameText)++"\");" ++
                                                       --concat [getElementByIdRef (widgetGetViewRef button)++".disabled = true;"| buttons<-timeButtonss, button<-buttons]
                                                       getElementByIdRef (widgetGetViewRef statusLabel)++".innerHTML =\\'Pressed\\';"
     }

-- todo comment has hard-coded width. make constant for this
instance Presentable ClientView where
  present (ClientView nrOfP mDate mTime nrButtons nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton statusLabel script) = 
    vList [ hList [ stringToHtml "Name:", hSpace 4, present nameText]
          , stringToHtml $ "Nr of people: "++show nrOfP
          , hListEx [width "100%"] $ map present nrButtons
          , stringToHtml $ maybe "Please choose a date" (\d -> (showDay . weekdayForDate $ d) ++ ", " ++ showShortDate d) mDate
          , hListEx [width "100%"] [ present todayButton, present tomorrowButton]
          , hListEx [width "100%"] $ map present dayButtons
          , stringToHtml $ maybe "Please select a time" (\d -> showTime d) mTime
          , simpleTable [width "100%",cellpadding 0, cellspacing 0] [] $ map (map present) timeButtonss
          , stringToHtml "Comments:"
          , present commentText
          , present confirmButton
          , present statusLabel] +++           
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

showShortMonth :: Reservations.Database.Month -> String
showShortMonth m = ["Jan.", "Feb.", "Mar.", "Apr.", "May", "June", "July", "Aug.","Sept.", "Oct.", "Nov.", "Dec."]!!(m-1)

 -- HACK
getTextContents :: Widget (Text Database) -> EditM Database String
getTextContents text =
 do { (sessionId, user, db, rootView, pendingEdit) <- get
    ; return $ getTextByViewIdRef (undefined :: Database{-dummy arg-}) (widgetGetViewRef text) rootView
    } 
 
 
 {-
Bug that seems to add views with number 23 all the time (probably the date)

Redirect webviews.oblomov.com doesn't work in iframe, figure out why not.

 Make sure reservations are sorted on time
 Fix overflows when many reservations on one day are made

 Please enter your name
 Please enter number of people
 Please select a date
 Please select a time (red is not available)
 Enter a comment or (confirm reservation)
 
 add time that reservation was made
 
 
 -- nice way to access db in EditM (now done with (_,_,db,_,_) <- get)
 
-- TODO accessing text fields and getting a reuse using getStrVal and getTextContents) is hacky and error prone   
-- Figure out a general way to handle init and reuse
-- e.g. for a name field, if we have an initial name, when do we set it? checking for "" is not possible, as the user may
-- clear the name, which should not cause init    

-- Init and reuse on non-widgets is also hacky, see mSelectedDate in clientView
 
-- Button style cannot be changed by parent element, so we need a way to specify the style at the button
Right now, this is hacked in by adding a style element to button, which causes part of the presentation to be
specified outside the present instance.
Find a good way for this, and see what other widgets have the problem. Maybe a combination of styles is possible?
Or maybe just have buttons always be max size and require them to be made minimal on presentation?


-- todo: check setting selections (date on today is now a hack, whole calendar should be based on selected rather than today)
-- currently res selection is done on present and hour selection is done on init and change of selected Day
-- find a good mechanism to do these selections
-- todo: split restaurant view, check where selections should live

 Ideas:
 
 Maybe change background when nothing is selected (or foreground for reservation fields)
 
 Hover!
 
 Standard tables that have css stuff for alignment etc. so no need to specify styles for each td
 -}