{-# OPTIONS -XDeriveDataTypeable #-}
module Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

users :: Map String (String, String)
users = Map.fromList [("martijn", ("p", "Martijn"))
                     ,("henny", ("h", "Henny Verweij")) 
                     ,("jaap", ("j", "Jaap Lageman"))
                     ,("", ("", "Anonymous"))
                     ] 
-- TODO: maybe this can be (a special) part of db?

newtype UserId = UserId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId Int deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allUsers :: Map UserId User, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data User = 
  User { userId :: UserId, name :: String, zipCode :: String
        , items :: [ItemId]
        } deriving (Eq, Show, Read, Typeable, Data)

updateUser :: UserId -> (User -> User) -> Database -> Database
updateUser i f db = 
  let user = unsafeLookup (allUsers db) i
  in  db  { allUsers = Map.insert i (f user) (allUsers db)
          }
-- add error

removeUser :: UserId -> Database -> Database
removeUser i db = db { allUsers = Map.delete i (allUsers db) }

newUser :: Database -> (User, Database)
newUser db =
  let ids = [ i | UserId i <- map fst (Map.toList $ allUsers db) ]
      newId = UserId $ if null ids then 0 else (maximum ids + 1)
      newUser = User newId "" "" []
  in  ( newUser, db { allUsers = Map.insert newId newUser (allUsers db) } )
          
data Item = 
  Item { itemId :: ItemId, owner :: UserId, itemName :: String
      } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.

updateItem :: ItemId -> (Item -> Item) -> Database -> Database
updateItem i f db = 
  let visit = unsafeLookup (allItems db) i
  in  db  { allItems = Map.insert i (f visit) (allItems db)
          }

removeItem :: ItemId -> Database -> Database
removeItem i db = db { allItems = Map.delete i (allItems db) }

newItem :: UserId -> Database -> (Item, Database)
newItem uid db =
  let ids = [ i | ItemId i <- map fst (Map.toList $ allItems db) ]
      newId = ItemId $ if null ids then 0 else (maximum ids + 1)
      newItem = Item newId uid "<new>"
  in  ( newItem, db { allItems = Map.insert newId newItem (allItems db) } )


mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { return $ Database 
                (Map.fromList [ (UserId 0, User (UserId 0) "Martijn Schrage" "3581 RA" [])
                              , (UserId 1, User (UserId 1) "Jaap Lageman" "3581 RA" [])
                ])
                (Map.fromList [] {- (PigId 1, Pig (PigId 1) (VisitId 1) "Knir" [0,0,0] (Left 2))
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



