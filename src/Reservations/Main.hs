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
  RestaurantView Int [[(Int,EditAction Database)]]
    deriving (Eq, Show, Typeable, Data)
  
instance Initial RestaurantView where                 
  initial = RestaurantView initial []

mkRestaurantView = mkWebView $
 \vid (RestaurantView viewedDay _) ->
  do { clockTime <-  liftIO getClockTime
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let currentDay = ctDay ct
           currentMonth = 1+fromEnum (ctMonth ct)
           currentYear = ctYear ct
           now   = (ctHour ct, ctMin ct)
           nrOfDaysInThisMonth = gregorianMonthLength (fromIntegral currentYear) currentMonth
           nrOfDaysInLastMonth = if currentMonth == 1 then 31 else gregorianMonthLength (fromIntegral currentYear) (currentMonth-1) 
     ; firstDayOfMonth <- weekdayForDate (1, 1+fromEnum (ctMonth ct), ctYear ct)
     
     ; let daysOfLastMonth = reverse $ take (firstDayOfMonth-1) [nrOfDaysInLastMonth,nrOfDaysInLastMonth-1..]
     ; let daysOfThisMonth = [1..nrOfDaysInThisMonth]
     ; let daysOfNextMonth = take (7 - ((length daysOfLastMonth + length daysOfThisMonth) `mod` 7)) [1..]
     ; let calendarDays = daysOfLastMonth ++ daysOfThisMonth ++ daysOfNextMonth 
     ; selects <- mapM (selectDayEdit vid) calendarDays
     
     ; let weeks = daysToWeeks $ zip calendarDays selects
     ;
     ; return $ RestaurantView viewedDay weeks
     }
 where selectDayEdit vid newDay = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView _ weeks) -> RestaurantView newDay weeks
 
instance Presentable RestaurantView where
  present (RestaurantView viewedDay weeks) = 
    mkTableEx [] [] [] [ [ ([ bgColorAttr $ if day == viewedDay then (Rgb 150 150 150) else (Rgb 240 240 240)
                            , withEditActionAttr selectionAction], stringToHtml (show day)) 
                         | (day, selectionAction) <- week] 
                     | week <- weeks ] +++
    p << stringToHtml (show viewedDay)
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
 