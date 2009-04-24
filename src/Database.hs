{-# OPTIONS -fglasgow-exts #-}
module Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

newtype VisitId = VisitId Int deriving (Show, Eq, Ord, Typeable, Data)

newtype PigId = PigId Int deriving (Show, Eq, Ord, Typeable, Data)

-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allVisits :: Map VisitId Visit, allPigs :: Map PigId Pig }
                  deriving (Show, Typeable,Data)

data Visit = 
  Visit { visitId :: VisitId, zipCode :: String
        , date :: String, pigs :: [PigId]
        } deriving (Show, Typeable, Data)

updateVisit :: VisitId -> (Visit -> Visit) -> Database -> Database
updateVisit i f db = 
  let visit = unsafeLookup (allVisits db) i
  in  db  { allVisits = Map.insert i (f visit) (allVisits db)
          }
-- add error

data Pig = 
  Pig { pigId :: PigId, parentVisit :: VisitId, pigName :: String, symptoms :: [Int]
      , diagnose :: Either Int String 
      } deriving (Show, Typeable,Data)

-- put id in element? It is also in the map.

updatePig :: PigId -> (Pig -> Pig) -> Database -> Database
updatePig i f db = 
  let visit = unsafeLookup (allPigs db) i
  in  db  { allPigs = Map.insert i (f visit) (allPigs db)
          }

removePig :: PigId -> Database -> Database
removePig i db = db { allPigs = Map.delete i (allPigs db) }

newPig :: VisitId -> Database -> (Pig, Database)
newPig vid db =
  let ids = [ i | PigId i <- map fst (Map.toList $ allPigs db) ]
      newId = PigId $ if null ids then 0 else (maximum ids + 1)
      newPig = Pig newId vid "<new>" [0,0,0] (Left 0)
  in  ( newPig, db { allPigs = Map.insert newId newPig (allPigs db) } )

theDatabase = Database (Map.fromList [ (VisitId 1, Visit (VisitId 1) "3581" "27-3-2009"
                                                  [ PigId 1, PigId 2, PigId 3 ])])
                    (Map.fromList [ (PigId 1, Pig (PigId 1) (VisitId 1) "Knir" [0,2,1] (Left 2))
                                  , (PigId 2, Pig (PigId 2) (VisitId 1) "Knar" [0,1,1] (Right "Malaria"))
                                  , (PigId 3, Pig (PigId 3) (VisitId 1) "Knor" [1,1,1] (Left 3)) ])
--                    ]
  
{-

data Vet = Vet { name :: String, visits :: [Visit]} deriving Show

--data Visit = Visit { zipCode :: String, date :: String, sties :: [Sty] } deriving Show
data Sty = Sty { pigs :: [Pig] } deriving Show

root :: Root
root = Root $ Vet "Martijn"
                [ Visit "3581" "27-3-2009" $
                    [ Sty [ Pig [0,0,0] (Left 2) ]
                    , Sty [ Pig [0,1,1] (Right "Malaria")
                          , Pig [1,1,1] (Left 3) ]
                    ]
                , Visit "7612" "26-3-2009" $
                    [ Sty [ Pig [0,0,0] (Left 2) ]
                    ]
                ]
-}

unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.

