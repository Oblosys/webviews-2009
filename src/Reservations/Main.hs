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
     
     ; let mSelectedDate' = Just $ maybe today id mSelectedDate
     ; let mSelectedReservation' = case mSelectedReservation of
                                     Just reservation -> if reservation `elem` reservationsSelectedHour then Just reservation else Nothing
                                     Nothing          -> Nothing
     
     -- todo: set selections (date on today is now a hack, whole calendar should be based on selected rather than today)
     -- todo: split these, check where selected date should live

     ; dayView <- mkDayView vid mSelectedHour reservationsSelectedDay
     ; hourView <- mkHourView vid mSelectedReservation mSelectedHour reservationsSelectedHour
     ; reservationView <- mkReservationView mSelectedReservation'

     
     ; return $ RestaurantView mSelectedDate' mSelectedHour mSelectedReservation' weeks dayView hourView reservationView
     }
 where selectDateEdit vid d = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView _ h r weeks dayView hourView reservationView) -> RestaurantView (Just d) h r weeks dayView hourView reservationView

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
   where header = [ ([], stringToHtml d) | d <- ["ma", "di", "wo", "do", "vr", "za", "zo"] ] 
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
  ClientView (Maybe Date) (Maybe Time) (Widget (Text Database)) (Widget (Text Database)) (Widget (Button Database)) (Widget (Button Database)) [Widget (Button Database)] [[Widget (Button Database)]] (Widget (Button Database)) String
    deriving (Eq, Show, Typeable, Data)
setClientViewDate md (ClientView _ mt n c b1 b2 b3 bs bss s) = (ClientView md mt n c b1 b2 b3 bs bss s)
setClientViewTime mt (ClientView md _ n c b1 b2 b3 bs bss s) = (ClientView md mt n c b1 b2 b3 bs bss s)
 
instance Initial ClientView where                 
  initial = ClientView initial initial initial initial initial initial initial initial initial initial

mkClientView = mkWebView $
 \vid (ClientView oldMDate mTime oldNameText oldCommentText _ _ _ _ _ _) ->
  do { clockTime <-  liftIO getClockTime -- this stuff is duplicated
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let today@(currentDay, currentMonth, currentYear) = dateFromCalendarTime ct
           now   = (ctHour ct, ctMin ct)
           
     ; let mDate = Just $ maybe today id oldMDate -- todo

       -- TODO hacky
     ; nameText  <- mkTextField $ getStrVal (oldNameText) -- needed, even though in browser text is reused without it
     ; commentText  <- mkTextArea $ getStrVal (oldCommentText) -- at server side it is not
           
     ; todayButton <- mkButton ("Today ("++show currentDay++" "++showShortMonth currentMonth++")") True $ Edit $ viewEdit vid $ setClientViewDate (Just today)
     ; tomorrowButton <- mkButton "Tomorrow" True $ Edit $ viewEdit vid $ setClientViewDate (Just $ addToDate today 1)
     ; dayButtons <- sequence [ mkButton (showShortDay . weekdayForDate $ dt) True $ Edit $ viewEdit vid $ setClientViewDate (Just dt)
                              | day <-[2..7], let dt = addToDate today day ]
                              
     ; timeButtonss <- sequence [ sequence [ mkButton (showTime tm) True $ Edit $ viewEdit vid $ setClientViewTime (Just tm)
                                           | mn <-[0,30], let tm = (hr,mn) ]
                                | hr <- [18..23] ] 
     
     ; confirmButton <- mkButton "Confirm" (isJust mDate && isJust mTime)  $
         Edit $ do { name <- getTextContents nameText
                   ; comment <- getTextContents commentText
                   ; docEdit $ \db -> 
                        let (Reservation rid _ _ _ _ _, db') = newReservation db
                            db'' = updateReservation rid (const $ Reservation rid (fromMaybe (error "no date") mDate) (fromMaybe (error "no time") mTime) name 8 comment) db
                        in  db''
                   }
     ; let status = "all ok" 
     ; return $ ClientView mDate mTime nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton status
     }
     
                    
-- todo comment has hard-coded width. make constant for this
instance Presentable ClientView where
  present (ClientView mDate mTime nameText commentText todayButton tomorrowButton dayButtons timeButtonss confirmButton status) = 
    vList [ stringToHtml $ maybe "No date chosen" (\d -> (showDay . weekdayForDate $ d) ++ ", " ++ showShortDate d) mDate
          , hList [ stringToHtml "Name:", hSpace 4, present nameText]
          , hList [ present todayButton, present tomorrowButton]
          , hList $ map present dayButtons
          , stringToHtml $ maybe "" (\d -> showTime d) mTime
          , simpleTable [width "100%"] [] $ map (map (\b -> with [width "100"] $ present b)) timeButtonss
          , stringToHtml "Comments:"
          , present commentText
          , present confirmButton
          , stringToHtml status]
 
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
if there are 1 or more reservations in the hour view, it is weird if none is selected and we see no comment (especially if there
is just one reservation as is seems that the no comment applies to that one) So add default selection of first elt.
 

add day name with date 
 
 Fix overflows when many reservations on one day are made

 Please enter your name
 Please enter number of people
 Please select a date
 Please select a time (red is not available)
 Enter a comment or (confirm reservation)
 
 add time that reservation was made
 
-- TODO accessing text fields and getting a reuse using getStrVal and getTextContents) is hacky and error prone   
-- Figure out a general way to handle init and reuse
-- e.g. for a name field, if we have an initial name, when do we set it? checking for "" is not possible, as the user may
-- clear the name, which should not cause init    
 
 Ideas:
 
 Maybe change background when nothing is selected (or foreground for reservation fields)
 
 Hover!
 
 Standard tables that have css stuff for alignment etc. so no need to specify styles for each td
 -}