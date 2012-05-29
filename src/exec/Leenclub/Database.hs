{-# OPTIONS -XDeriveDataTypeable #-}
module Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

leners :: Map String (String, String)
leners = Map.fromList [("martijn", ("p", "Martijn"))
                     ,("henny", ("h", "Henny Verweij")) 
                     ,("jaap", ("j", "Jaap Lageman"))
                     ,("", ("", "Anonymous"))
                     ] 
-- TODO: maybe this can be (a special) part of db?

newtype LenerId = LenerId String deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId Int deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allLeners :: Map LenerId Lener, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data Lener = 
  Lener { lenerId :: LenerId, lenerName :: String, lenerZipCode :: String
        , lenerItems :: [ItemId]
        } deriving (Eq, Show, Read, Typeable, Data)

lenerLogin Lener{lenerId = LenerId login} = login

updateLener :: LenerId -> (Lener -> Lener) -> Database -> Database
updateLener i f db = 
  let lener = unsafeLookup (allLeners db) i
  in  db  { allLeners = Map.insert i (f lener) (allLeners db)
          }
-- add error

removeLener :: LenerId -> Database -> Database
removeLener i db = db { allLeners = Map.delete i (allLeners db) }

{-
newLener :: Database -> (Lener, Database)
newLener db =
  let ids = [ i | LenerId i <- map fst (Map.toList $ allLeners db) ]
      newId = LenerId $ if null ids then 0 else (maximum ids + 1)
      newLener = Lener newId "" "" []
  in  ( newLener, db { allLeners = Map.insert newId newLener (allLeners db) } )
  -}
          
data Item = 
  Item { itemId :: ItemId, itemOwner :: LenerId, itemName :: String
      } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.

updateItem :: ItemId -> (Item -> Item) -> Database -> Database
updateItem i f db = 
  let visit = unsafeLookup (allItems db) i
  in  db  { allItems = Map.insert i (f visit) (allItems db)
          }

removeItem :: ItemId -> Database -> Database
removeItem i db = db { allItems = Map.delete i (allItems db) }

newItem :: LenerId -> Database -> (Item, Database)
newItem uid db =
  let ids = [ i | ItemId i <- map fst (Map.toList $ allItems db) ]
      newId = ItemId $ if null ids then 0 else (maximum ids + 1)
      newItem = Item newId uid "<new>"
  in  ( newItem, db { allItems = Map.insert newId newItem (allItems db) } )


mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { return $ Database 
                (Map.fromList [ (LenerId "martijn", Lener (LenerId "martijn") "Martijn Schrage" "3581 RA" 
                                                      [ItemId 0, ItemId 1, ItemId 2])
                              , (LenerId "jaap", Lener (LenerId "jaap") "Jaap Lageman" "3581 RA" 
                                                      [ItemId 3, ItemId 4])
                              , (LenerId "henny", Lener (LenerId "henny") "Henny Verweij" "3500 XX" 
                                                      [ItemId 5, ItemId 6])
                ])
                (Map.fromList [ (ItemId 0, Item (ItemId 0) (LenerId "martijn") "Oblomov")
                              , (ItemId 1, Item (ItemId 1) (LenerId "martijn") "Grand Theft Auto 4")
                              , (ItemId 2, Item (ItemId 2) (LenerId "martijn") "iPhone 3gs")
                              , (ItemId 3, Item (ItemId 3) (LenerId "jaap") "Boormachine")
                              , (ItemId 4, Item (ItemId 4) (LenerId "jaap") "Spyder calibratie-apparaat")
                              , (ItemId 5, Item (ItemId 5) (LenerId "henny") "Tomtom")
                              , (ItemId 6, Item (ItemId 6) (LenerId "henny") "Boormachine")
                              ] {- (PigId 1, Pig (PigId 1) (VisitId 1) "Knir" [0,0,0] (Left 2))
                              , (PigId 2, Pig (PigId 2) (VisitId 1) "Knar" [0,1,1] (Right "Malaria"))
                              , (PigId 3, Pig (PigId 3) (VisitId 1) "Knor" [1,0,1] (Left 3)) 
                              , (PigId 4, Pig (PigId 4) (VisitId 2) "Piglet" [1,1,1] (Left 3)) 
                              , (PigId 5, Pig (PigId 5) (VisitId 2) "Pinky" [0,1,1] (Left 3)) 
                ] -})
    }
--                    ]


unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.



