{-# OPTIONS -XDeriveDataTypeable #-}
module Reservations.Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

users :: Map String (String, String)
users = Map.fromList [("martijn", ("p", "Martijn"))
                     ,("", ("", "Anonymous"))
                     ] 
-- TODO: maybe this can be (a special) part of db?


newtype ReservationId = ReservationId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allReservations :: Map ReservationId Reservation
                         } deriving (Eq, Show, Read, Typeable,Data)


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
              } deriving (Eq, Show, Read, Typeable, Data)

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
      newReservation = Reservation newId (0,0,0) (0,0) "" 0
  in  (newReservation, db { allReservations = Map.insert newId newReservation (allReservations db) } )
   
unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.


theDatabase = Database 
  (Map.fromList [ (ReservationId 1, Reservation (ReservationId 1) (21,9,2011) (20,30) "Martijn" 2)
                , (ReservationId 2, Reservation (ReservationId 2) (24,9,2011) (18,00) "Pino" 3)
                ])
