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
main = server mkRootView theDatabase users

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
  RestaurantView Date String [[(WebView Database,EditAction Database)]]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial RestaurantView where                 
  initial = RestaurantView (initial, initial, initial) initial []

mkRestaurantView = mkWebView $
 \vid (RestaurantView selectedDate _ _) ->
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
     
     ; let calendarDayViewForDate date@(_,m,_) = mkCalendarDayView date (date==selectedDate) (date==today) (m==currentMonth) $
                                           [r | r@(Reservation _ d _ _ _) <- reservations, date==d]  
     
     ; calendarDayViews <- mapM calendarDayViewForDate calendarDays
     ; let reservationsToday = concat [ show h++"."++show m++": "++nm++" "
                                      | Reservation _ d (h,m) nm nr <- reservations, d==selectedDate ]
     
     ; let weeks = daysToWeeks $ zip calendarDayViews selects
     
     ; return $ RestaurantView selectedDate reservationsToday weeks
     }
 where selectDateEdit vid d = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView _ r weeks) -> RestaurantView d r weeks
 
{- day: mark today
mark different months, mark appointments
 -}
instance Presentable RestaurantView where
  present (RestaurantView selectedDate@(selDay, selMonth, selYear) reservationsToday weeks) = 
    mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-family:Arial; text-align:center"] [] [thestyle "border: 1px solid #909090"]  
              (header :
               [ [ ([withEditActionAttr selectionAction], present dayView) 
                 | (dayView, selectionAction) <- week] 
               | week <- weeks ]) +++
    p << stringToHtml (show selectedDate) +++
    p << stringToHtml reservationsToday
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
 {-
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with_ [thestyle "font-family: arial"] $
      mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],
-}
instance Storeable Database CalendarDayView where
  save _ = id

--- Utils

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