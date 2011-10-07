{-# OPTIONS -XDeriveDataTypeable -XPatternGuards -XMultiParamTypeClasses -XScopedTypeVariables #-}
module Main where

import Data.List
import Text.Html hiding (image, name)
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
import WebViewPrim
import WebViewLib
import HtmlLib
import Control.Monad.State
import Server
import System.IO.Unsafe (unsafePerformIO) -- just for calendar stuff
import Reservations.Database
import Reservations.ClientWebView

main :: IO ()
main = server rootViews "ReservationsDB.txt" theDatabase users


rootViews :: [ (String, Int -> WebViewM Database (WebView Database))]
rootViews = [ ("", mkMainRootView), ("client", \sessionId -> mkClientView), ("restaurant", \sessionId -> mkRestaurantView)
            , ("test", mkTestView1) ] 
  -- TODO: id's here?
  -- TODO: fix the sessionId stuff
  -- TODO: find good names for root, main, etc.

 -- TODO: sessionId? put this in an environment? or maybe the WebViewM monad?
data TestView1 =
  TestView1 (EditAction Database) (Widget (Button Database)) String String
    deriving (Eq, Show, Typeable, Data)
         
instance Initial TestView1 where
  initial = TestView1 initial initial initial initial
  
-- pass client state back with an edit action
-- seems easier than using JSVar's below
mkTestView1 _ = mkWebView $
 \vid (TestView1 ea _ status _) ->
  do { ea <- mkEditActionEx $ \args -> Edit $ do { debugLn $ "edit action executed "++show args
                                                 ; viewEdit vid $ (\(TestView1 a b _ d) -> TestView1 a b (head args) d)
                                                 }
  -- todo: need a way to enforce that ea is put in the webview
     ; b <- mkButton "Test" True $ Edit $ return () 
     ; return $ TestView1 ea b status $
         "console.log('test script running');" ++
         declareVar vid "clientState" "2" ++
         onClick b (readVar vid "clientState" ++"++;" ++
                    callServerEditAction ea [readVar vid "clientState","2"]) 
     }

instance Presentable TestView1 where
  present (TestView1 ea b status scr) = vList [stringToHtml status, present b] +++ mkScript scr

instance Storeable Database TestView1 where
  save _ = id


-- trying to handle passing back client state by using a JSVar
data TestView2 =
  TestView2 (Widget JSVar) (Widget (Button Database)) String String
    deriving (Eq, Show, Typeable, Data)
         
instance Initial TestView2 where
  initial = TestView2 initial initial initial initial
  
mkTestView2 _ = mkWebView $
 \vid (TestView2 oldXVar@(Widget _ _ (JSVar _ _ v)) _ status _) ->
  do { x <- mkJSVar "x" (if v == "" then "6" else v) 
     -- todo Even though the initial value is only assigned once to the client variable, it is also
     -- used to store the value at server side, so we need to check if it has a value and assign if not
     -- this is not okay. keep separate field for initializer?
     ; b <- mkButton "Test" True $ Edit $ do { xval <- getJSVarContents x
                                             ; debugLn $ "value of js var is "++xval
                                             ; viewEdit vid $ (\(TestView2 a b _ d) -> TestView2 a b xval d)
                                             } 
     ; return $ TestView2 x b status $
         "console.log('test script running');" ++
         onClick b (refJSVar x++"++;console.log("++refJSVar x++");" ++
                    "queueCommand('SetC ("++show (getViewId x)++") \"'+"++refJSVar x++"+'\"');"++
                    "queueCommand('ButtonC ("++show (getViewId b)++")');")
     }
-- todo: reusing goes wrong in this case: ; nrOfPeopleVar <- mkJSVar "x" $ show $ getJSVarValue oldNrOfPeopleVar
--extra escape chars are added on each iteration. (this was in a webview that was re-presented constantly)

refJSVar (Widget _ _ (JSVar viewId name _)) = name++viewIdSuffix viewId

instance Presentable TestView2 where
  present (TestView2 x b status scr) = vList [stringToHtml status, present b] +++ present x +++ mkScript scr

instance Storeable Database TestView2 where
  save _ = id
  
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
  RestaurantView (Maybe Date) (Maybe Int) (Maybe Reservation) (Int,Int) [[(WebView Database,EditAction Database)]] (WebView Database) (WebView Database) (WebView Database) String
    deriving (Eq, Show, Typeable, Data)

instance Initial RestaurantView where                 
  initial = RestaurantView initial initial initial (initial,initial) [] initial initial initial initial

mkRestaurantView = mkWebView $
 \vid (RestaurantView mSelectedDate mSelectedHour mSelectedReservation _ _ _ _ _ _) ->
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
      
     ; debugLn $ "\n\n\n\n\n\n\n\n" ++ show (mkDay reservationsSelectedDay)
     
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
     ; reservationView <- mkReservationView vid mSelectedReservation'

     
     ; return $ RestaurantView mSelectedDate' mSelectedHour' mSelectedReservation' (currentMonth, currentYear) weeks dayView hourView reservationView $
         "console.log(\"restaurant script\");"++
         declareVar vid "selectedDayIx" "\"\"" ++ -- "" is no selection
         declareVar vid "selectedHourIx" "null" ++ -- null is no selection
         declareVar vid "selectedReservationIx" "\"\"" ++ -- "" is no selection
         writeVar vid "hourObjects" (mkJsArr $ mkDay reservationsSelectedDay) ++
         writeVar vid "selectedHourIx" (maybe "null" (\h -> show $ h-18) mSelectedHour') ++ -- todo for now we just set it each time
         
         mkSelectedReservation vid mSelectedReservation' ++
         callFunction (getViewId dayView) "load" [] ++";"++
         callFunction (getViewId hourView) "load" [] ++";"++
         callFunction (getViewId reservationView) "load" [] ++";"
     }
 where selectDateEdit vid d = mkEditAction . Edit $
        do { (_,_,db,_,_) <- get
           ; viewEdit vid $ 
              \(RestaurantView _ h r my weeks dayView hourView reservationView script) ->
                let resHours = sort $ map (fst . time) $ filter ((==d). date) $ Map.elems $ allReservations db
                    newHr = if null resHours then h else Just $ head resHours 
                in  RestaurantView (Just d) newHr r my weeks dayView hourView reservationView script
           }
{-
  [ { hourEntry: nrOfRes(nrOfPeople)
    , reservations: [{listEntry:name+nr  reservation:{name: nrOfPeople: .. }]
    } ]
-}

mkDay reservationsDay =  [ let reservationsHour = filter ((==hr). fst . time) reservationsDay
                               nrOfReservations = length reservationsHour
                               nrOfPeopleInHour = sum $ map nrOfPeople reservationsHour
                           in  mkJson [ ("hourEntry", show $ if nrOfReservations == 0 then "" else show nrOfReservations++" ("++ show nrOfPeopleInHour ++")")
                                      , ("reservations", mkJsArr $ mkHour reservationsHour)]
                         | hr <- [18..23]]
                         
mkJsArr elts = "["++intercalate"," elts ++"]"

mkHour reservationsHour = [ mkJson [ ("reservationEntry", show $ name res ++ "("++show (nrOfPeople res)++")")
                                   , ("reservation",mkReservation res)
                                   ] 
                          | res <- reservationsHour ]

mkReservation (Reservation _ date time name nrOfPeople comment) = mkJson [ ("date",show (showDate date)), ("time", show (showTime time)), ("name",show name)
                                                                         , ("nrOfPeople", show nrOfPeople),("comment",show comment)]

mkSelectedReservation vid mRes =
  writeVar vid "selectedReservation" $
    case mRes of 
      Nothing -> "null"
      Just (Reservation _ date time name nrOfPeople comment) -> mkJson [ ("date",show (showDate date)), ("time", show (showTime time)), ("name",show name)
                                                                       , ("nrOfPeople", show nrOfPeople),("comment",show comment)]

getWebViewId (WebView vid _ _ _ _) = vid

{- day: mark today
mark different months, mark appointments
 -}
instance Presentable RestaurantView where
  present (RestaurantView mSelectedDate mSelectedHour mSelectedReservation (currentMonth, currentYear) weeks dayView hourView reservationView script) = 
    (vList $ 
      [ with [thestyle "text-align:center; font-weight:bold"] $ stringToHtml $ showMonth currentMonth ++ " "++show currentYear
      , vSpace 5
      , mkTableEx [cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; text-align:center"] [] [thestyle "border: 1px solid #909090"]  
                  (header :
                   [ [ ([{- withEditActionAttr selectionAction-} strAttr "onClick" $ "addSpinner('hourView');"++ -- todo refs are hardcoded!
                                                                "addSpinner('reservationView');"++
                                                                "queueCommand('PerformEditActionC ("++show (getViewId selectionAction)++") []')"], present dayView) 
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
      ]) +++ mkScript script
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
selectedHourColor = Rgb 0x80 0xb0 0xff

instance Presentable CalendarDayView where
  present (CalendarDayView date@(day, month, year) isSelected isToday isThisMonth reservations) = 
    withBgColor (if isToday then Rgb 0x90 0x90 0x90 else if isSelected then selectedDayColor else Rgb 240 240 240) $
    -- we use a margin of 3 together with the varying cell background to show today
    -- doing this with borders is awkward as they resize the table
      mkTableEx [ width "40px", height 40, cellpadding 0, cellspacing 0 
                , thestyle $ "margin:2px; background-color:" ++ if isSelected then htmlColor selectedHourColor else htmlColor (Rgb 240 240 240) ] [] [] 
        [[ ([valign "top"] ++ if isThisMonth then [] else [thestyle "color: #808080"], stringToHtml (show day)) ]
        ,[ ([thestyle "font-size:80%; color:#0000ff"], if not $ null reservations then stringToHtml $ show (length reservations) ++ " (" ++ show (sum $ map nrOfPeople reservations)++")" else nbsp) ] 
        ]
    -- TODO: make Rgb for standard html colors, make rgbH for (rgbH 0xffffff)
    
    -- check slow down after running for a while

instance Storeable Database CalendarDayView where
  save _ = id

-----------------------------------------------------------------------------

data DayView = 
  DayView (Maybe Int) [EditAction Database] [Reservation] String
    deriving (Eq, Show, Typeable, Data)
  
instance Initial DayView where                 
  initial = DayView initial initial initial initial

mkDayView :: ViewId -> Maybe Int -> [Reservation] -> WebViewM Database (WebView Database)
mkDayView restaurantViewId mSelectedHour dayReservations = mkWebView $
 \vid (DayView _ _ _ _) ->
  do { selectHourActions <- mapM (selectHourEdit restaurantViewId) [18..24]
     ; return $ DayView mSelectedHour selectHourActions dayReservations $
         declareFunction vid "load" [] $
           "console.log(\"Load day view called \"+"++readVar restaurantViewId "selectedHourIx"++");" ++
           "if ("++readVar restaurantViewId "selectedHourIx"++"!=null)"++ -- todo: reference to hourOfDay is hard coded
           "$(\"#hourOfDayView_\"+"++readVar restaurantViewId "selectedHourIx"++").css(\"background-color\",\""++htmlColor selectedHourColor++"\");"
     }
 where selectHourEdit vid h = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView d _ r my weeks dayView hourView reservationView scr) -> RestaurantView d (Just h) r my weeks dayView hourView reservationView scr
 
instance Presentable DayView where
  present (DayView mSelectedHour selectHourActions dayReservations script) =
    mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse; font-size:80%"] [] [] 
      [[ ([identifier $ "hourOfDayView_"++show (hr-18),withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: #d0d0d0"]
         , presentHour hr) | (sa,hr) <- zip selectHourActions [18..24] ]] +++ mkScript script

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
  HourView (Maybe Reservation) (Maybe Int) [EditAction Database] [Reservation] String
    deriving (Eq, Show, Typeable, Data)
  
instance Initial HourView where                 
  initial = HourView initial initial initial initial initial

mkHourView :: ViewId -> Maybe Reservation -> Maybe Int -> [Reservation] -> WebViewM Database (WebView Database)
mkHourView restaurantViewId mSelectedReservation mSelectedHour hourReservations = mkWebView $
 \vid (HourView _ _ _ _ _) ->
  do { selectReservationActions <- mapM (selectReservationEdit restaurantViewId) hourReservations
     ; return $ HourView mSelectedReservation mSelectedHour selectReservationActions hourReservations $
         jsScript [ jsFunction vid "load" [] 
                      [ "console.log('Load hour view called')"
                      , "console.log("++readVar restaurantViewId "selectedHourIx"++")"
                      , jsIf (readVar restaurantViewId "selectedHourIx") 
                          [ "var hourObject = "++readVar restaurantViewId "hourObjects"++"["++readVar restaurantViewId "selectedHourIx"++"]" 
                          , "console.log('Hour entry is'+hourObject.hourEntry)"
                          , jsFor "i=0; i<hourObject.reservations.length; i++" 
                              [ "console.log('item:'+hourObject.reservations[i].reservationEntry)" ]
                          ]
                      ]
                  ]
     }
 where selectReservationEdit vid r = mkEditAction . Edit $ viewEdit vid $ \(RestaurantView d h _ my weeks dayView hourView reservationView scr) -> RestaurantView d h (Just r) my weeks dayView hourView reservationView scr
 
instance Presentable HourView where
  present (HourView mSelectedReservation mSelectedHour selectReservationActions hourReservations script) =
    (with [identifier "hourView"] $ -- in separate div, because spinners on scrolling elements  cause scrollbars to be shown
    boxedEx 0 $ with [ thestyle "height:90px;overflow:auto"] $
      mkTableEx [width "100%", cellpadding 0, cellspacing 0, thestyle "border-collapse:collapse"] [] [] $ 
        [ [ ([withEditActionAttr sa, thestyle $ "border: 1px solid #909090; background-color: "++
                                                if Just r==mSelectedReservation then htmlColor selectedDayColor else "#f8f8f8"]
            , hList [hSpace 2, stringToHtml $ showTime tm ++ " -   "++nm++" ("++show nr++")"]) ]
        | (sa,r@(Reservation _ _ tm nm nr _)) <- zip selectReservationActions hourReservations 
        ]) +++ mkScript script
instance Storeable Database HourView where
  save _ = id

-----------------------------------------------------------------------------

data ReservationView = 
  ReservationView (Maybe Reservation) (Widget (Button Database)) String
    deriving (Eq, Show, Typeable, Data)
  
instance Initial ReservationView where                 
  initial = ReservationView initial initial initial

-- todo: probably don't need the reservation hereanymore
mkReservationView restaurantViewId mReservation = mkWebView $
 \vid (ReservationView _ _ _) ->
  do { removeButton <- mkButton "x" True $ 
         Edit $ docEdit $ maybe id (\res -> removeReservation $ reservationId res ) mReservation
     ; return $ ReservationView mReservation removeButton $
         declareFunction vid "load" [] $ 
           let selRes = readVar restaurantViewId "selectedReservation"
           in  "console.log(\"Load reservation view called\");" ++
                 "if ("++selRes++"!=null) {" ++
                 "console.log(\"date\"+"++selRes++".date);" ++
                 "console.log(\"time\"+"++selRes++".time);" ++
                 "$(\"#reservationView\").css(\"color\",\"black\");" ++
                 getElementByIdRef (widgetGetViewRef removeButton)++".disabled = false;"++
                 "$(\"#dateField\").text("++selRes++".date);"++
                 "$(\"#nameField\").text("++selRes++".name);"++
                 "$(\"#timeField\").text("++selRes++".time);"++
                 "$(\"#nrOfPeopleField\").text("++selRes++".nrOfPeople);"++
                 "$(\"#commentField\").text("++selRes++".comment);"++
               "} else {" ++
                 "$(\"#reservationView\").css(\"color\",\"grey\");" ++
                 getElementByIdRef (widgetGetViewRef removeButton)++".disabled = true;"++
                 "$(\"#dateField\").text(\"\");"++
                 "$(\"#nameField\").text(\"\");"++
                 "$(\"#timeField\").text(\"\");"++
                 "$(\"#nrOfPeopleField\").text(\"\");"++
                 "$(\"#commentField\").text(\"\");"++
                 "console.log(\"not set\");" ++
               "}"
     }
 
-- todo comment has hard-coded width. make constant for this
instance Presentable ReservationView where
  present (ReservationView mReservation removeButton script) = (with [thestyle "background-color:#f0f0f0"] $ boxed $ 
    vListEx [ identifier "reservationView"] 
      [ hListEx [width "100%"] [ hList [ stringToHtml "Reservation date: ",nbsp
                                       , with [colorAttr reservationColor] $ with [identifier "dateField"] $ noHtml ]
                               , with [align "right"] $ present removeButton]
      , hList [stringToHtml "Time:",nbsp, with [colorAttr reservationColor] $ with [identifier "timeField"] $ noHtml]
      , hList [stringToHtml "Name:",nbsp, with [colorAttr reservationColor] $ with [identifier "nameField"] $ noHtml]
      , hList [stringToHtml "Nr. of people:",nbsp, with [colorAttr reservationColor] $ with [identifier "nrOfPeopleField"] $ noHtml ]
      , stringToHtml "Comment:" 
      , boxedEx 0 $ with [thestyle $ "padding-left:4px;height:70px; width:300px; overflow:auto; color:" ++ htmlColor reservationColor] $ with [identifier "commentField"] $  
          noHtml
      ]) +++ mkScript script
   where reservationColor = Rgb 0x00 0x00 0xff
 
instance Storeable Database ReservationView where
  save _ = id





 
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