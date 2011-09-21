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
  RestaurantView Date [[(Date,EditAction Database)]]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial RestaurantView where                 
  initial = RestaurantView (initial, initial, initial) []

mkRestaurantView = mkWebView $
 \vid (RestaurantView selectedDate _) ->
  do { clockTime <-  liftIO getClockTime
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let currentDay = ctDay ct
           currentMonth = 1+fromEnum (ctMonth ct)
           currentYear = ctYear ct
           now   = (ctHour ct, ctMin ct)
           (lastMonth, lastMonthYear) = if currentMonth == 1 then (12,currentYear-1) else (currentMonth-1,currentYear)
           (nextMonth, nextMonthYear) = if currentMonth == 12 then (1,currentYear+1) else (currentMonth+1,currentYear)
           nrOfDaysInLastMonth = gregorianMonthLength (fromIntegral lastMonthYear) lastMonth 
           nrOfDaysInThisMonth = gregorianMonthLength (fromIntegral currentYear) currentMonth
     ; firstDayOfMonth <- weekdayForDate (1, 1+fromEnum (ctMonth ct), ctYear ct)
     
     ; let daysOfLastMonth = reverse $ take (firstDayOfMonth-1) [nrOfDaysInLastMonth,nrOfDaysInLastMonth-1..]
     ; let daysOfThisMonth = map (\d->(d, currentMonth,currentYear)) $ [1..nrOfDaysInThisMonth]
     ; let daysOfNextMonth = map (\d->(d, currentMonth+1,currentYear)) $ take (7 - ((length daysOfLastMonth + length daysOfThisMonth) `mod` 7)) [1..]
     ; let calendarDaysOfLastMonth = [(d,lastMonth, lastMonthYear) | d <- daysOfLastMonth] 
     ; let calendarDaysOfThisMonth = [(d,currentMonth, currentYear) | d <- daysOfLastMonth]
     ; let calendarDaysOfNextMonth = [(d,nextMonth, nextMonthYear) | d <- daysOfLastMonth]
     ; let calendarDays = calendarDaysOfLastMonth ++ calendarDaysOfThisMonth ++ calendarDaysOfNextMonth 
     ; selects <- mapM (selectDateEdit vid) calendarDays
     
     ; let weeks = daysToWeeks $ zip calendarDays selects
     ; return $ RestaurantView selectedDate weeks
     }
 where selectDateEdit vid d = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView _ weeks) -> RestaurantView d weeks
 
instance Presentable RestaurantView where
  present (RestaurantView selectedDate@(selDay, selMonth, selYear) weeks) = 
    mkTableEx [] [] [] [ [ ([ bgColorAttr $ if date == selectedDate then (Rgb 150 150 150) else (Rgb 240 240 240)
                            , withEditActionAttr selectionAction], stringToHtml (show day)) 
                         | (date@(day, month, year), selectionAction) <- week] 
                     | week <- weeks ] +++
    p << stringToHtml (show selectedDate)
  {-
    withBgColor (Rgb 235 235 235) $ withPad 5 0 5 0 $    
    with_ [thestyle "font-family: arial"] $
      mkTableEx [width "100%"] [] [valign "top"]
       [[ ([],
           (h2 << "Piglet 2.0 different version")  +++
           ("List of all visits     (session# "++show sessionId++")") +++         
      p << (hList [ withBgColor (Rgb 250 250 250) $ roundedBoxed Nothing $ withSize 230 100 $ 
             (let rowAttrss = [] :
                              [ [withEditActionAttr selectionAction] ++
                                if i == viewedVisit then [ fgbgColorAttr (Rgb 255 255 255) (Rgb 0 0 255)
                                                           ] else [] 
                              | (i,selectionAction) <- zip [0..] selectionActions 
                              ]
                  rows = [ stringToHtml "Nr.    ", stringToHtml "Zip"+++nbspaces 3
                         , (stringToHtml "Date"+++nbspaces 10) ]  :
                         [ [stringToHtml $ show i, stringToHtml zipCode, stringToHtml date] 
                         | (i, (zipCode, date)) <- zip [1..] visits
                         ]
              in  mkTable [strAttr "width" "100%", strAttr "cellPadding" "2", thestyle "border-collapse: collapse"] 
                     rowAttrss [] rows
                 )])  +++
      p << (present add +++ present remove) 
      )
      ,([align "right"],
     hList[
      case user of
         Nothing -> present loginoutView 
         Just (_,name) -> stringToHtml ("Hello "++name++".") +++ br +++ br +++ present loginoutView
      ] )]
      ] +++
      p << ((if null visits then "There are no visits. " else "Viewing visit nr. "++ show (viewedVisit+1) ++ ".") +++ 
             "    " +++ present prev +++ present next) +++ 
      --vList (map present tabbedVisits)
      present tabbedVisits
{-
          boxed (case mv of
               [] -> stringToHtml "No visits."
               visitVs -> concatHtml $ map present visitVs) -} +++
      h2 << "Comments" +++
      vList (map present commentViews) +++ 
      nbsp +++ (case mAddCommentButton of 
                  Nothing -> stringToHtml "Please log in to add a comment"
                  Just b  -> present b)
      -}
instance Storeable Database RestaurantView where
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
 