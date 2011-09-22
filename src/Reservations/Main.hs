{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses #-}
module Main where

import Data.List
import Text.Html hiding (image)
import qualified Text.Html as Html
import Data.Generics
import Data.Char
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

import Reservations.Database

main :: IO ()
main = server mkRootView "ReservationsDB.txt" theDatabase users

mkRootView :: User -> Database -> Int -> ViewMap Database -> IO (WebView Database)
mkRootView user db sessionId viewMap =
  fmap assignIds $ runWebView user db viewMap [] 0 $ mkMainView sessionId
  -- TODO: id's here?

    
-- Main ----------------------------------------------------------------------  

data MainView = 
  MainView (WebView Database)
    deriving (Eq, Show, Typeable, Data)
  
instance Initial MainView where                 
  initial = MainView initial


mkMainView sessionId = mkWebView $
 \vid (MainView _) ->
  do { restaurantView <- mkRestaurantView                                            
     ; return $ MainView restaurantView
     }
 
instance Presentable MainView where
  present (MainView v) = present v 

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
     ; let currentDay = ctDay ct
           currentMonth = 1+fromEnum (ctMonth ct)
           currentYear = ctYear ct
           today = (currentDay, currentMonth, currentYear)
           now   = (ctHour ct, ctMin ct)
           (lastMonth, lastMonthYear) = if currentMonth == 1 then (12,currentYear-1) else (currentMonth-1,currentYear)
           (nextMonth, nextMonthYear) = if currentMonth == 12 then (1,currentYear+1) else (currentMonth+1,currentYear)
           nrOfDaysInLastMonth = gregorianMonthLength (fromIntegral lastMonthYear) lastMonth 
           nrOfDaysInThisMonth = gregorianMonthLength (fromIntegral currentYear) currentMonth
     ; firstDayOfMonth <- weekdayForDate (1, 1+fromEnum (ctMonth ct), ctYear ct)
     
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
     
     ; let reservationsToday = filter ((==mSelectedDate). Just . date) reservations
     ; let reservationsSelectedHour = filter ((==mSelectedHour). Just . fst . time) reservationsToday
      
     ; let weeks = daysToWeeks $ zip calendarDayViews selects
     
     -- todo: split these, check where selected date should live
     ; dayView <- mkDayView vid mSelectedHour reservationsToday
     ; hourView <- mkHourView vid mSelectedReservation mSelectedHour reservationsSelectedHour
     ; reservationView <- mkReservationView mSelectedReservation

     
     ; return $ RestaurantView mSelectedDate mSelectedHour mSelectedReservation weeks dayView hourView reservationView
     }
 where selectDateEdit vid d = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView _ h r weeks dayView hourView reservationView) -> RestaurantView (Just d) h r weeks dayView hourView reservationView

getWebViewId (WebView vid _ _ _ _) = vid

{- day: mark today
mark different months, mark appointments
 -}
instance Presentable RestaurantView where
  present (RestaurantView selectedDate selectedHour selectedReservation weeks dayView hourView reservationView) = 
    vList $ 
      [ mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-family:Arial; text-align:center"] [] [thestyle "border: 1px solid #909090"]  
                  (header :
                   [ [ ([withEditActionAttr selectionAction], present dayView) 
                     | (dayView, selectionAction) <- week] 
                   | week <- weeks ]
                   )
      ] ++
      [ present dayView
      , stringToHtml (show selectedDate)
      , present hourView
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
        ,[ ([],             stringToHtml $ if null reservations then "" else "o") ]
        ]
--      p << stringToHtml (if null reservations then "." else "o")
    -- TODO: make Rgb for standard html colors, make rgbH for (rgbH 0xffffff)
    
    -- check slow down after running for a while
 {-
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with_ [thestyle "font-family: arial"] $
      mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],
-}
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
    mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-size:80%; font-family:Arial"] [] [] 
      [[ ([withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: "++
                                             if Just hr==mSelectedHour then htmlColor selectedDayColor else "#d0d0d0"]
         , stringToHtml $ show hr++"h") | (sa,hr) <- zip selectHourActions [18..24] ]] 

--    mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-family:Arial; text-align:center"] [] [thestyle "border: 1px solid #909090"]  

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
    mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-family:Arial"] [] [] 
      [[ ([withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: "++
                                             if Just r==mSelectedReservation then htmlColor selectedDayColor else "#f8f8f8"]
         , stringToHtml $ showTime tm ++ " -   "++nm++" ("++show nr++")") ]
      | (sa,r@(Reservation _ _ tm nm nr _)) <- zip selectReservationActions hourReservations 
      ]
instance Storeable Database HourView where
  save _ = id

-----------------------------------------------------------------------------

data ReservationView = 
  ReservationView String Int String
    deriving (Eq, Show, Typeable, Data)
  
instance Initial ReservationView where                 
  initial = ReservationView initial initial initial


mkReservationView mReservation = mkWebView $
 \vid (ReservationView _ _ _) ->
  do { case mReservation of
         Just (Reservation _ _ _ name nrOfPeople comment) ->
           return $ ReservationView name nrOfPeople comment
         Nothing -> 
           return $ ReservationView "no reservation" 0 ""
     }
 
instance Presentable ReservationView where
  present (ReservationView name nrOfPeople comment) = 
    simpleTable [] [] 
      [ [stringToHtml "name: ", stringToHtml name]
      , [stringToHtml "nrOfPeople: ", stringToHtml $ show nrOfPeople ]
      , [stringToHtml comment ]
      ]  

instance Storeable Database ReservationView where
  save _ = id


--- Utils
showTime (h,m) = (if h<10 then " " else "") ++ show h ++ ":" ++ (if m<10 then "0" else "") ++ show m

daysToWeeks days = if length days < 7 then [days]
                   else take 7 days : daysToWeeks (drop 7 days)

weekdayForDate (day, month, year) = liftIO $
 do { clockTime <-  getClockTime -- first need to get a calendar time in this time zone
    ; today <- toCalendarTime clockTime
    ; let ct = today {ctDay= day, ctMonth = toEnum $ month-1, ctYear = year, ctHour = 12, ctMin = 0, ctPicosec = 0}
    ; ctWithWeekday <- toCalendarTime $ toClockTime ct
    ; return $ (fromEnum (ctWDay ctWithWeekday) -1) `mod` 7 + 1 -- in enum, Sunday is 0 and Monday is 1, we want Monday = 1 and Sunday = 7
    } 
 
 
 
 {-
 Ideas:
 
 Standard tables that have css stuff for alignment etc. so no need to specify styles for each td
 -}