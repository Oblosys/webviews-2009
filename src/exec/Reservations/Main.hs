{-# LANGUAGE DeriveDataTypeable, PatternGuards, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell, ImpredicativeTypes, FlexibleContexts #-}
module Main where

import Data.List
import BlazeHtml hiding (time, name)
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
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server
import System.IO.Unsafe (unsafePerformIO) -- just for calendar stuff
import TemplateHaskell

import Database
import ClientWebView
import ReservationUtils

appBgColor = Rgb 0xf8 0xf8 0xf8
dayColor = Rgb 0xf0 0xf0 0xf0         -- todo: these are background colors rather than colors
selectedDayColor = Rgb 0x80 0xb0 0xff
todayColor = Rgb 0x90 0x90 0x90
hourColor = Rgb 0xd0 0xd0 0xd0
selectedHourColor = Rgb 0x80 0xb0 0xff
reservationColor =  Rgb 0xf8 0xf8 0xf8
selectedReservationColor = selectedDayColor


data TestView1 =
  TestView1 (Widget (EditAction Database)) (Widget (Button Database)) String String
    deriving (Eq, Show, Typeable)
         
instance Initial TestView1 where
  initial = TestView1 initial initial initial initial
  
deriveMapWebViewDb ''Database ''TestView1

-- pass client state back with an edit action
-- seems easier than using JSVar's below
mkTestView1 = mkWebView $
 \vid (TestView1 ea _ status _) ->
  do { ea <- mkEditActionEx $ \args -> do { debugLn $ "edit action executed "++show args
                                          ; viewEdit vid $ (\(TestView1 a b _ d) -> TestView1 a b (head args) d)
                                          }
  -- todo: need a way to enforce that ea is put in the webview
     ; b <- mkButton "Test" True $ return () 
     ; return $ TestView1 ea b status $
         -- "console.log('test script running');" ++
         jsDeclareVar vid "clientState" "2" ++
         onClick b (jsVar vid "clientState" ++"++;" ++
                    callServerEditAction ea [jsVar vid "clientState","2"]) 
     }

instance Presentable TestView1 where
  present (TestView1 ea b status scr) = vList [toHtml status, present b] +++ mkScript scr

instance Storeable Database TestView1 where


-- trying to handle passing back client state by using a JSVar
data TestView2 =
  TestView2 (Widget (JSVar Database)) (Widget (Button Database)) String String
    deriving (Eq, Show, Typeable)
         
instance Initial TestView2 where
  initial = TestView2 initial initial initial initial

deriveMapWebViewDb ''Database ''TestView2

mkTestView2 = mkWebView $
 \vid (TestView2 oldXVar@(Widget _ _ (JSVar _ _ v)) _ status _) ->
  do { x <- mkJSVar "x" (if v == "" then "6" else v) 
     -- todo Even though the initial value is only assigned once to the client variable, it is also
     -- used to store the value at server side, so we need to check if it has a value and assign if not
     -- this is not okay. keep separate field for initializer?
     ; b <- mkButton "Test" True $ do { xval <- getJSVarContents x
                                      ; debugLn $ "value of js var is "++xval
                                      ; viewEdit vid $ (\(TestView2 a b _ d) -> TestView2 a b xval d)
                                      } 
     ; return $ TestView2 x b status $
         -- "console.log('test script running');" ++
         onClick b (refJSVar x++"++"{-++";console.log("++refJSVar x++");"-} ++
                    "queueCommand('SetC ("++show (getViewId x)++") \"'+"++refJSVar x++"+'\"');"++
                    "queueCommand('ButtonC ("++show (getViewId b)++")');")
     }
-- todo: reusing goes wrong in this case: ; nrOfPeopleVar <- mkJSVar "x" $ show $ getJSVarValue oldNrOfPeopleVar
--extra escape chars are added on each iteration. (this was in a webview that was re-presented constantly)

refJSVar (Widget _ _ (JSVar viewId name _)) = name++viewIdSuffix viewId

instance Presentable TestView2 where
  present (TestView2 x b status scr) = vList [toHtml status, present b] +++ present x +++ mkScript scr

instance Storeable Database TestView2 where

-----------------------------------------------------------------------------

data ReservationView = 
  ReservationView (Widget (Button Database)) (Widget (EditAction Database)) String
    deriving (Eq, Show, Typeable)
  
instance Initial ReservationView where                 
  initial = ReservationView initial initial initial

deriveMapWebViewDb ''Database ''ReservationView

mkReservationView restaurantViewId = mkWebView $
 \vid (ReservationView _ _ _) ->
  do { removeButton <- mkButton "x" True $ return ()
     ; db <- getDb
     ; removeAction <- mkEditActionEx $ \[reservationId] ->
         let resId = read reservationId
         in  case Map.lookup resId (allReservations db) of
               Nothing  -> return ()
               Just res -> confirmEdit ("Are you sure you wish to delete the reservation for "++name res++" at "++showTime (time res)++"?") $
                             modifyDb $ removeReservation resId  
     ; return $ ReservationView removeButton removeAction $ jsScript 
         [ jsFunction vid "load" [] $ 
           let selRes = jsVar restaurantViewId "selectedReservationObj"
           in  [ {-"console.log(\"Load reservation view called\")"
               ,-} onClick removeButton $ callServerEditAction removeAction [jsVar restaurantViewId "selectedReservationObj" ++".reservationId"]
               , jsIfElse selRes
                   [ {-"console.log(\"date\"+"++selRes++".date)" 
                   ,  "console.log(\"time\"+"++selRes++".time)"
                   ,-}  "$(\"#reservationView\").css(\"color\",\"black\")"
                   ,  jsGetElementByIdRef (widgetGetViewRef removeButton)++".disabled = false"
                   ,  "$(\"#dateField\").text("++selRes++".date)"
                   ,  "$(\"#nameField\").text("++selRes++".name)"
                   ,  "$(\"#timeField\").text("++selRes++".time)"
                   ,  "$(\"#nrOfPeopleField\").text("++selRes++".nrOfPeople)"
                   ,  "$(\"#commentField\").text("++selRes++".comment)"
                   ]
                   [ "$(\"#reservationView\").css(\"color\",\"grey\")"
                   , jsGetElementByIdRef (widgetGetViewRef removeButton)++".disabled = true"
                   , "$(\"#dateField\").text(\"\")"
                   , "$(\"#nameField\").text(\"\")"
                   , "$(\"#timeField\").text(\"\")"
                   , "$(\"#nrOfPeopleField\").text(\"\")"
                   , "$(\"#commentField\").text(\"\")"
                   -- , "console.log(\"not set\")"
                   ] 
               ]
         ]
     }
 
-- todo comment has hard-coded width. make constant for this
instance Presentable ReservationView where
  present (ReservationView removeButton _ script) = (withStyle "background-color:#f0f0f0" $ boxed $ 
    vListEx [ id_ "reservationView"] 
      [ hListEx [width "100%"] [ hList [ "Reservation date: ",nbsp
                                       , with [colorAttr reservationColor] $ with [id_ "dateField"] $ noHtml ]
                               , with [align "right"] $ mkClassDiv "RemoveButton" $ present removeButton]
      , hList [ "Time:",nbsp, with [colorAttr reservationColor] $ with [id_ "timeField"] $ noHtml]
      , hList [ "Name:",nbsp, with [colorAttr reservationColor] $ with [id_ "nameField"] $ noHtml]
      , hList [ "Nr. of people:",nbsp, with [colorAttr reservationColor] $ with [id_ "nrOfPeopleField"] $ noHtml ]
      , "Comment:" 
      , boxedEx 1 0 $ withStyle ("padding-left:4px;height:70px; width:356px; overflow:auto; color:" ++ htmlColor reservationColor) $ with [id_ "commentField"] $  
          noHtml
      ]) +++ mkScript script
   where reservationColor = Rgb 0x00 0x00 0xff
 
instance Storeable Database ReservationView where

instance MapWebView Database Reservation

-- CalendarDay ----------------------------------------------------------------------  
     
     
data CalendarDayView = 
  CalendarDayView Date Bool Bool Bool [Reservation]
    deriving (Eq, Show, Typeable)
  
instance Initial CalendarDayView where                 
  initial = CalendarDayView (initial, initial, initial) initial initial initial initial

deriveMapWebViewDb ''Database ''CalendarDayView

mkCalendarDayView date isSelected isToday isThisMonth reservations = mkWebView $
 \vid (CalendarDayView _ _ _ _ _) ->
  do { return $ CalendarDayView date isSelected isToday isThisMonth reservations
     }
     
instance Presentable CalendarDayView where
  present (CalendarDayView date@(day, month, year) isSelected isToday isThisMonth reservations) = 
    -- we use a margin of 3 together with the varying cell background to show today
    -- doing this with borders is awkward as they resize the table
    conditionallyBoxed isToday 2 (htmlColor todayColor) $
       mkTableEx [ width "44px", height "36", cellpadding "0", cellspacing "0" 
                 , style $ "margin:2px"] [] [] 
             [[ ([valign "top"] ++ if isThisMonth then [] else [style "color: #808080"], toHtml (show day)) ]
           ,[ ([style "font-size:80%; color:#0000ff"], if not $ null reservations then toHtml $ show (length reservations) ++ " (" ++ show (sum $ map nrOfPeople reservations)++")" else nbsp) ] 
        ]
    -- TODO: make Rgb for standard html colors, make rgbH for (rgbH 0xffffff)
    
    -- check slow down after running for a while
conditionallyBoxed cond width color elt =
  if cond then div_!*[style $ "border:solid; border-color:"++color++"; border-width:"++show width++"px;"] << elt
          else div_!*[style $ "padding:"++show width++"px;"] << elt

instance Storeable Database CalendarDayView where

-----------------------------------------------------------------------------

data DayView = 
  DayView [String] [Reservation] String
    deriving (Eq, Show, Typeable)
  
instance Initial DayView where                 
  initial = DayView initial initial initial

deriveMapWebViewDb ''Database ''DayView

-- the bar with hours under the calendar
mkDayView :: ViewId -> ViewId -> [Reservation] -> WebViewM Database (WV DayView)
mkDayView restaurantViewId hourViewId dayReservations = mkWebView $
 \vid (DayView _ _ _) ->
  do { let selectHourActions = map (selectHourEdit vid) [18..24]
     ; return $ DayView selectHourActions dayReservations $
              jsScript [ jsFunction vid "load" [] $ 
                      let selDayObj = jsVar restaurantViewId "selectedDayObj"
                      in [ {-jsLog "'Load day view called'"
                         ,-} jsIfElse selDayObj
                           [  -- don't have to load for now, since view is presented by server
                            
                             -- show selected hour
                             jsFor ("i=0; i<"++selDayObj++".hours.length; i++")
                               [ "$('#hourOfDayView_'+i).css('background-color',("++jsVar restaurantViewId "selectedHourIx"++"==i)?'"++htmlColor selectedHourColor++"'"++
                                                                                                      ":'"++htmlColor hourColor++"')"
                                                          ] -- todo: reference to hourOfDay is hard coded
                           {-, jsLog $ "'SelectedHourIx is '+"++ jsVar restaurantViewId "selectedHourIx"-}
                           , jsIfElse (jsVar restaurantViewId "selectedHourIx"++"!=null") 
                               [ jsAssignVar restaurantViewId "selectedHourObj" $  selDayObj ++".hours["++jsVar restaurantViewId "selectedHourIx"++"]"
                               ]
                               [ jsAssignVar restaurantViewId "selectedHourObj" "null" ]
                           ]
                           [] -- will not occur now, since day is given by server (if it occurs, also set selectedHourObj to null, see mkHourView)
                       
                       , jsCallFunction hourViewId "load" []
                            
                     {-      
                      , jsLog $jsVar restaurantViewId "selectedHourIx"
                      , jsIf (jsVar restaurantViewId "selectedHourIx"++"!=null") 
                          [ "var hourObject = "++jsVar restaurantViewId "hourObjects"++"["++jsVar restaurantViewId "selectedHourIx"++"]" 
                          , "console.log('Hour entry is'+hourObject.hourEntry)"
                          , jsFor "i=0; i<hourObject.reservations.length; i++" 
                              [ "console.log('item:'+hourObject.reservations[i].reservationEntry)" ]
                          ] -}
                      ]  
                  ]
     }
 where selectHourEdit dayViewId h = jsAssignVar restaurantViewId "selectedHourIx" (show $ h-18) ++";"++
                                    jsAssignVar restaurantViewId "selectedReservationIx" "0" ++";"++
                                    jsCallFunction  restaurantViewId "load" [] -- because the label changes, we need to load restaurant view


instance Presentable DayView where
  present (DayView selectHourActions dayReservations script) =
    mkTableEx [width "100%", cellpadding "0", cellspacing "0", style "border-collapse:collapse; font-size:80%"] [] [] 
      [[ ([id_ . toValue $ "hourOfDayView_"++show (hr-18)
          , strAttr "onClick" sa -- todo: don't like this onClick here
          , style $ "border: 1px solid #909090; background-color: #d0d0d0"]
         , presentHour hr) | (sa,hr) <- zip selectHourActions [18..24] ]] +++ mkScript script

   where presentHour hr = --withBgColor (Rgb 255 255 0) $
           vListEx [width "48px"] -- needs to be smaller than alotted hour cell, but larger than width of "xx(xx)"
                 [ toHtml $ show hr++"h"
                 , withStyle "font-size:80%; text-align:center; color:#0000ff" $ 
                     let ressAtHr = filter ((==hr) . fst . time) dayReservations
                     in  if not $ null ressAtHr then toHtml $ show (length ressAtHr) ++ " (" ++ show (sum $ map nrOfPeople ressAtHr)++")" else nbsp 
                 ]
instance Storeable Database DayView where

-----------------------------------------------------------------------------

data HourView = 
  HourView[String] [Reservation] String
    deriving (Eq, Show, Typeable)
  
instance Initial HourView where                 
  initial = HourView initial initial initial

deriveMapWebViewDb ''Database ''HourView

-- the list of reservations for a certain hour
mkHourView :: ViewId -> ViewId -> [Reservation] -> WebViewM Database (WV HourView)
mkHourView restaurantViewId reservationViewId hourReservations = mkWebView $
 \vid (HourView _ _ _) ->
  do { let selectReservationActions = map (selectReservationEdit vid) [0..19]
     ; return $ HourView selectReservationActions hourReservations $
         jsScript [ jsFunction vid "load" [] $ 
                      let selHourObj = jsVar restaurantViewId "selectedHourObj"
                      in [ {-"console.log('Load hour view called')"
                         , jsLog $ "'SelectedHourIx is '+"++ jsVar restaurantViewId "selectedHourIx"
                         , jsLog $ "'SelectedHour is '+"++ selHourObj
                         ,-} jsIfElse selHourObj
                           [ -- jsLog $ "'Hour object not null'",
                           
                           -- if selection is below reservation list make the selectedReservationIx null
                             jsIf (jsVar restaurantViewId "selectedReservationIx"++">="++selHourObj++".reservations.length")
                             [ jsAssignVar restaurantViewId "selectedReservationIx" "null" ]
                           -- load hour and show selected reservation
                           , jsFor ("i=0; i<"++selHourObj++".reservations.length; i++") 
                              [ {-"console.log('item:'+"++selHourObj++".reservations[i].reservationEntry)" 
                              ,-} "$('#reservationLine_'+i).css('background-color',("++jsVar restaurantViewId "selectedReservationIx"++"==i)?'"++htmlColor selectedReservationColor++"'"++
                                                                                                           ":'"++htmlColor reservationColor++"')"
                              , "$('#reservationEntry_'+i).text("++selHourObj++".reservations[i].reservationEntry)"
                              ]
                            
                           -- clear the other entries
                           , jsFor ("i="++selHourObj++".reservations.length; i<20; i++") 
                              [ "$('#reservationEntry_'+i).text('')"
                              , "$('#reservationLine_'+i).css('background-color','"++htmlColor reservationColor++"')"
                              ]
                           
                           , jsIfElse (jsVar restaurantViewId "selectedReservationIx"++"!=null") 
                               [ jsAssignVar restaurantViewId "selectedReservationObj" $  selHourObj ++".reservations["++jsVar restaurantViewId "selectedReservationIx"++"].reservation"
                               ]
                               [ jsAssignVar restaurantViewId "selectedReservationObj" "null" ]
                           ]
                           
                           -- empty reservation list, set selectedReservationObj to nul
                           [ {-jsLog "'Hour object null'"
                           ,-} jsFor ("i=0; i<20; i++") -- duplicated code 
                              [ "$('#reservationEntry_'+i).text('')"
                              , "$('#reservationLine_'+i).css('background-color','"++htmlColor reservationColor++"')"
                              ]
                           , jsAssignVar restaurantViewId "selectedReservationObj" "null"
                           ] 

                         , jsCallFunction reservationViewId "load" []
                         
                      ]  
                  ]
     }
 where selectReservationEdit hourViewId i = jsAssignVar restaurantViewId "selectedReservationIx" (show $ i) ++";"++
                                            jsCallFunction  hourViewId "load" []
 
instance Presentable HourView where
  present (HourView selectReservationActions hourReservations script) =
    (with [id_ "hourView"] $ -- in separate div, because spinners on scrolling elements  cause scrollbars to be shown
    boxedEx 1 0 $ withStyle "height:90px;overflow:auto" $
      mkTableEx [width "100%", cellpadding "0", cellspacing "0", style "border-collapse:collapse"] [] [] $ 
        [ [ ([id_ . toValue $ "reservationLine_"++show i
             , strAttr "onClick" sa -- todo: don't like this onClick here
             , style $ "border: 1px solid #909090"]
          , hList [nbsp, with [id_ . toValue $ "reservationEntry_"++show i] $ "reservationEntry"]) ]
        | (i,sa) <- zip [0..] selectReservationActions 
        ]) +++ mkScript script

instance Storeable Database HourView where


data RestaurantView = 
  RestaurantView (Maybe Date) (Int,Int) (Widget (Button Database)) (Widget (Button Database)) 
                 [[(WV CalendarDayView,String, Widget (EditAction Database))]] (WV DayView) (WV HourView) (WV ReservationView) String
    deriving (Eq, Show, Typeable)

instance Initial RestaurantView where                 
  initial = RestaurantView initial (initial,initial) initial initial [] initial initial initial initial

deriveMapWebViewDb ''Database ''RestaurantView

modifyViewedDate :: ((Int,Int) -> (Int,Int)) -> RestaurantView -> RestaurantView
modifyViewedDate f (RestaurantView d my lb nb weeks dayView hourView reservationView script) =
  RestaurantView d (f my) lb nb weeks dayView hourView reservationView script

mkRestaurantView = mkWebView $
 \vid (RestaurantView oldMSelectedDate viewedMonthYear' _ _ _ _ _ _ _) ->
  do { clockTime <-  liftIO getClockTime
     ; ct <- liftIO $ toCalendarTime clockTime
     ; let today@(_, currentMonth, currentYear) = dateFromCalendarTime ct
           now   = (ctHour ct, ctMin ct)
           viewedMonthYear@(viewedMonth,viewedYear) = if viewedMonthYear'==(0,0) then (currentMonth, currentYear) else viewedMonthYear' 
           (lastMonth, lastMonthYear) = decreaseMonth viewedMonthYear
           (nextMonth, nextMonthYear) = increaseMonth viewedMonthYear
           nrOfDaysInLastMonth = gregorianMonthLength (fromIntegral lastMonthYear) lastMonth 
           nrOfDaysInThisMonth = gregorianMonthLength (fromIntegral viewedYear) viewedMonth
           firstDayOfMonth = weekdayForDate (1, viewedMonth, viewedYear)
     
     ; let daysOfLastMonth = reverse $ take (firstDayOfMonth-1) [nrOfDaysInLastMonth,nrOfDaysInLastMonth-1..]
     ; let daysOfThisMonth = [1..nrOfDaysInThisMonth]
     ; let daysOfNextMonth = [1..]
     ; let calendarDaysOfLastMonth = [(d,lastMonth, lastMonthYear) | d <- daysOfLastMonth] 
     ; let calendarDaysOfThisMonth = [(d,viewedMonth, viewedYear) | d <- daysOfThisMonth]
     ; let calendarDaysOfNextMonth = [(d,nextMonth, nextMonthYear) | d <- daysOfNextMonth]
     ; let calendarDays = take (7*6) $ calendarDaysOfLastMonth ++ calendarDaysOfThisMonth ++ calendarDaysOfNextMonth 
     ; let selectScripts = [jsCallFunction vid "selectDay" [show i]| i <- [0..length calendarDays -1]] 
     ; selects <- mapM (selectDateEdit vid) calendarDays
     
     ; reservations <- fmap Map.elems $ withDb $ \db -> allReservations db
     
     ; -- hack
     ; let mSelectedDate = 
             case oldMSelectedDate of
                Just selectedDate -> oldMSelectedDate
                Nothing           -> Just today

     ; let calendarDayViewForDate date@(_,m,_) = mkCalendarDayView date (Just date==mSelectedDate) (date==today) (m==viewedMonth) $
                                           [r | r@(Reservation _ d _ _ _ _) <- reservations, date==d]  
     
     ; calendarDayViews <- mapM calendarDayViewForDate calendarDays
     
     ; let weeks = daysToWeeks $ zip3 calendarDayViews selectScripts selects
                                          
     ; let reservationsSelectedDay = filter ((==mSelectedDate). Just . date) reservations
      
     ; lastButton <- mkButton (showMonth lastMonth) True $ viewEdit vid $ modifyViewedDate decreaseMonth
     ; nextButton <- mkButton (showMonth nextMonth) True $ viewEdit vid $ modifyViewedDate increaseMonth
     ; reservationView <- mkReservationView vid
     ; hourView <- mkHourView vid (getViewId reservationView) []
     ; dayView <- mkDayView vid (getViewId hourView) reservationsSelectedDay

     
     ; return $ RestaurantView mSelectedDate (viewedMonth, viewedYear) lastButton nextButton weeks dayView hourView reservationView $ jsScript 
         [ {-"console.log(\"restaurant script\")"
         ,-} jsFunction vid "load" []  
           [ {-jsLog "'Load restaurant view'" -- TODO: in future version that is completely dynamic, check for selectedDayObj == null
           ,-} jsIfElse (jsVar vid "selectedDayObj"++".selectedDate && "++jsVar vid "selectedHourIx" ++"!=null") 
             [ "$('#betweenPlaceholder').text('Reservations on '+"++ jsVar vid "selectedDayObj"++".selectedDate.day+' '+"++ jsVar vid "selectedDayObj"++".selectedDate.month" ++
                       "+' between '+("++jsVar vid "selectedHourIx"++"+18)+'h and '+("++jsVar vid "selectedHourIx"++"+18+1)+'h')"
             ]
             [ "$('#betweenPlaceholder').html('&nbsp;')"
             ]
           , jsFor "i=0; i<6*7; i++"  -- TODO: hard coded, not nice, is there a length we can use?
             [ "$('#calDayView_'+i).css('background-color',("++jsVar vid "selectedDayIx"++"==i)?'"++htmlColor selectedDayColor++"'"++
                                                                                            ":'"++htmlColor dayColor++"')"
             ]
           , jsCallFunction (getViewId dayView) "load" []
           ]   
           
         , jsFunction vid "selectDay" ["i"]  
           [ jsAssignVar vid "selectedDayIx" "i"
           , jsAssignVar vid "selectedHourIx" "null"
           , jsAssignVar vid "selectedReservationIx" "null"
           , jsCallFunction vid "load" []
           ] 
         , jsDeclareVar vid "selectedDayIx" "null"  -- null is no selection
         , jsDeclareVar vid "selectedHourIx" "null"  -- null is no selection
         , jsDeclareVar vid "selectedReservationIx" "null"  -- null is no selection
         , jsAssignVar vid "selectedDayIx" $ case mSelectedDate of
                                               Nothing -> "null"
                                               Just dt  -> case elemIndex dt calendarDays of
                                                             Nothing -> "null"
                                                             Just i -> show i
         , jsAssignVar vid "selectedDayObj" (mkDay mSelectedDate reservationsSelectedDay) 
         , jsDeclareVar vid "selectedHourObj" "null" -- null is no selection
         , jsDeclareVar vid "selectedReservationObj" "null" -- null is no selection
         , jsAssignVar vid "selectedHourIx" (let resHours = sort $ map (fst . time) $  filter ((\d -> Just d ==mSelectedDate) . date) reservations
                                           in  if null resHours then "0" else show $ head resHours - 18) 
         
         , jsAssignVar vid "selectedReservationIx" "0"
         , jsCallFunction vid "load" []
         ]
     }
 where selectDateEdit vid d = mkEditAction $ viewEdit vid $ 
              \(RestaurantView _ my lb nb weeks dayView hourView reservationView script) ->
                RestaurantView (Just d) my lb nb weeks dayView hourView reservationView script
          
{-
  [ { hourEntry: nrOfRes(nrOfPeople)
    , reservations: [{listEntry:name+nr  reservation:{name: nrOfPeople: .. }]
    } ]
-}

mkDay mSelectedDate reservationsDay =  
  mkJson [("selectedDate", case mSelectedDate of 
                             Nothing -> "null"
                             Just (d,m,y) -> mkJson [("day",show d),("month",show $ showMonth m)]
          )
         ,("hours", jsArr [ let reservationsHour = filter ((==hr). fst . time) reservationsDay
                                nrOfReservations = length reservationsHour
                                nrOfPeopleInHour = sum $ map nrOfPeople reservationsHour
                      in  mkJson [ ("hourEntry", show $ if nrOfReservations == 0 then "" else show nrOfReservations++" ("++ show nrOfPeopleInHour ++")")
                                   , ("reservations", jsArr $ mkHour reservationsHour)]
                         | hr <- [18..24]]
          )]
                         

mkHour reservationsHour = [ mkJson [ ("reservationEntry", show $ showTime (time res)++" - "++ 
                                                                 name res ++ " ("++show (nrOfPeople res)++")")
                                   , ("reservation",mkReservation res)
                                   ] 
                          | res <- reservationsHour ]

mkReservation (Reservation rid date time name nrOfPeople comment) = mkJson [ ("reservationId", show $ show rid), ("date",show (showDate date)), ("time", show (showTime time))
                                                                         , ("name",show name), ("nrOfPeople", show nrOfPeople),("comment",show comment)]

{- day: mark today
mark different months, mark appointments
 -}
instance Presentable RestaurantView where
  present (RestaurantView mSelectedDate (currentMonth, currentYear) lastButton nextButton weeks dayView hourView reservationView script) =
    with [ style "font-family:arial", class_ "RestaurantView" ] $  
    (vList $ 
      [ mkClassDiv "CalendarHeader" $
              xp $ row [ h $ present lastButton
                 , flexSpace
                 , h $ withStyle "font-weight:bold" $ toHtml $ showMonth currentMonth ++ " "++show currentYear
                 , flexSpace
                 , h $ present nextButton
                 ]
      , vSpace 5
      , mkTableEx [cellpadding "0", cellspacing "0", style "border-collapse:collapse; text-align:center"] [] [style "border: 1px solid #909090"]  
                  (header :
                   [ [ ([{- withEditActionAttr selectionAction-} strAttr "onClick" $ "addSpinner('hourView');"++ -- todo refs are hardcoded!
                                                                selectScript++
                                                                ";queueCommand('PerformEditActionC ("++show (getViewId selectionAction)++") []')"]
                     , with [id_ $ toValue $ "calDayView_"++show (i*7+j)] $ present dayView) 
                     | (j,(dayView, selectScript, selectionAction)) <- zip [0..] week] 
                   | (i,week) <- zip [0..] weeks ]
                   )
      ] ++
      [ present dayView
      , vSpace 15
      , withStyle "font-size:80%" $
          with [id_ "betweenPlaceholder"] "uninitialized 'between' placeholder"
      , vSpace 6
      , present hourView
      , vSpace 15
      , present reservationView
      ]) +++ mkScript script
   where header = [ ([], toHtml $ showShortDay d) | d <- [1..7] ] 

instance Storeable Database RestaurantView where

-- Extra indirection, so we can style restaurant views that are not part of the combined view or embedded in an iFrame.
data RestaurantWrapperView = RestaurantWrapperView (WV RestaurantView) deriving (Eq, Show, Typeable)
  
instance Initial RestaurantWrapperView where                 
  initial = RestaurantWrapperView initial

deriveMapWebViewDb ''Database ''RestaurantWrapperView

mkRestaurantWrapperView = mkWebView $
 \vid (RestaurantWrapperView _) ->
  do { restaurantView <- mkRestaurantView
     ; return $ RestaurantWrapperView restaurantView
     }

instance Presentable RestaurantWrapperView where
  present (RestaurantWrapperView fv) = 
        mkClassDiv "RestaurantWrapperParent" $ mkPage [] $ mkClassDiv "RestaurantWrapperView" $ present fv 

instance Storeable Database RestaurantWrapperView where

-- Main ----------------------------------------------------------------------  

data MainView = 
  MainView (WV ClientView) (WV RestaurantView)
    deriving (Eq, Show, Typeable)
  
instance Initial MainView where                 
  initial = MainView initial initial

deriveMapWebViewDb ''Database ''MainView

mkMainRootView = mkWebView $
 \vid (MainView _ _) ->
  do { clientView <- mkClientView
     ; restaurantView <- mkRestaurantView                                            
     ; return $ MainView clientView restaurantView
     }

instance Presentable MainView where
  present (MainView cv rv) = 
    mkClassDiv "CombinedWrapperParent" $ mkPage [] $ mkClassDiv "CombinedWrapperView" $ 
     hListEx [] [ roundedBoxed (Just $ appBgColor) $ present rv
                , hSpace 50
                , roundedBoxed (Just $ appBgColor) $ present cv 
                ] 

instance Storeable Database MainView where


main :: IO ()
main = server 8102 "Reservations" rootViews ["Reservations.css"] "ReservationsDB.txt" mkInitialDatabase users

-- the webviews here are phantom typed, so we need rootView to get rid of the phantom types
rootViews = [ mkRootView ""                   mkMainRootView          -- combined view of restaurant and client
            , mkRootView "client"             mkClientWrapperView     -- for running in a browser by itself or on an iPhone/iPad
            , mkRootView "embeddedClient"     mkClientView            -- for running in an iFrame
            , mkRootView "restaurant"         mkRestaurantWrapperView -- for running in a browser by itself or on an iPad/iPhone 
            , mkRootView "embeddedRestaurant" mkRestaurantView        -- for running in an iFrame
            , mkRootView "test"               mkTestView1
            ]
             
  -- TODO: id's here?
  -- TODO: fix the sessionId stuff
  -- TODO: find good names for root, main, etc.

 -- TODO: sessionId? put this in an environment? or maybe the WebViewM monad?

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
