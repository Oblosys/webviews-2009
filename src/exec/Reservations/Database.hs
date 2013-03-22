{-# OPTIONS -XDeriveDataTypeable #-}
module Database where

import Data.Time.Calendar hiding (Day)
import System.Time hiding (Day, Month)

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 
import Types

type WV v = WebView Database v

users :: Map String (String, String)
users = Map.fromList [("martijn", ("p", "Martijn"))
                     ,("", ("", "Anonymous"))
                     ] 
-- TODO: maybe this can be (a special) part of db?


newtype ReservationId = ReservationId {unReservationId :: Int} deriving (Show, Read, Eq, Ord)

-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allReservations :: Map ReservationId Reservation
                         } deriving (Eq, Show, Read, Typeable)


type Hours = Int
type Minutes = Int
type Day = Int
type Month = Int
type Year = Int
type Time = (Hours, Minutes)
type Date = (Day, Month, Year)

data Reservation = 
  Reservation { reservationId :: ReservationId
              , date :: Date
              , time :: Time
              , name :: String
              , nrOfPeople :: Int
              , comment :: String
              } deriving (Eq, Show, Read)

-- not safe, requires that id in reservation is respected by f
updateReservation :: ReservationId -> (Reservation -> Reservation) -> Database -> Database
updateReservation i f db = 
  let reservation = unsafeLookup (allReservations db) i
  in  db  { allReservations = Map.insert i (f reservation) (allReservations db)
          }
-- add error

removeReservation :: ReservationId -> Database -> Database
removeReservation i db = db { allReservations = Map.delete i (allReservations db) }

newReservation :: Database -> (Reservation, Database)
newReservation db =
  let ids = [ i | ReservationId i <- map fst (Map.toList $ allReservations db) ]
      newId = ReservationId $ if null ids then 0 else (maximum ids + 1)
      newReservation = Reservation newId (0,0,0) (0,0) "" 0 ""
  in  (newReservation, db { allReservations = Map.insert newId newReservation (allReservations db) } )
   
unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.


mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { clockTime <- getClockTime
    ; ct <- toCalendarTime clockTime
    ; let today@(currentDay, currentMonth, currentYear) = (ctDay ct, 1+fromEnum (ctMonth ct), ctYear ct)
          (nextMonth, nextMonthYear) = if currentMonth == 12 then (1,currentYear+1) else (currentMonth+1,currentYear)
          daysInCurrentMonth = gregorianMonthLength (fromIntegral currentYear) currentMonth
          daysInNextMonth =  gregorianMonthLength (fromIntegral nextMonthYear) nextMonth
          datedReservations = addDates currentMonth currentYear (take daysInCurrentMonth lotOfReservations) ++
                              addDates nextMonth nextMonthYear (take daysInNextMonth lotOfReservations)
    ; return $ Database $ Map.fromList $ addIds datedReservations
                
    }
 where addIds ress = [ (ReservationId i, Reservation (ReservationId i) dt tm nm nr c) 
                     | (i,(dt, tm, nm, nr, c)) <- zip [0..] ress
                     ]
                
       addDates m y resss = [ ((d,m,y),tm,nm,nr,c)
                            | (d,ress) <- zip [1..] resss
                            , (tm,nm,nr,c) <- ress
                            ]
lotOfReservations = concat . repeat $
                      [  [ ((20,00), "Nathan", 2, "Nathan says hi")
                         , ((20,00), "Tommy", 3, "")
                         , ((20,00), "Paul", 2, "")
                         , ((20,30), "Bridget", 3, "")
                         , ((20,30), "Nicole", 4, "")
                         , ((22,00), "Ann", 8, "")
                         ]
                       , [ ((21,00), "Karel 2", 8, "Dinner at nine") 
                         ]
                       , [ ((18,00), "Pino", 3, "Please provide bird seed") 
                         ]
                       , []
                       ]