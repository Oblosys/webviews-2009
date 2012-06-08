{-# OPTIONS -XDeriveDataTypeable #-}
module Database where

import Data.Generics
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

lenders :: Map String (String, String)
lenders = Map.fromList [("martijn", ("p", "Martijn"))
                      ,("henny", ("h", "Henny Verweij")) 
                      ,("jaap", ("j", "Jaap Lageman"))
                      ,("", ("", "Anonymous"))
                      ] 
-- TODO: maybe this can be (a special) part of db?

newtype LenderId = LenderId String deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId Int deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allLenders :: Map LenderId Lender, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data Lender = 
  Lender { lenderId :: LenderId, lenderName :: String, lenderZipCode :: String
         , lenderItems :: [ItemId]
         } deriving (Eq, Show, Read, Typeable, Data)

lenderLogin Lender{lenderId = LenderId login} = login


searchLenders :: String -> Database -> [Lender]
searchLenders term db = [ lender | lender <- Map.elems $ allLenders db
                        , any (isInfixOf term) [lenderName lender, lenderZipCode lender, lenderLogin lender]
                        ]

updateLender :: LenderId -> (Lender -> Lender) -> Database -> Database
updateLender i f db = 
  let lender = unsafeLookup (allLenders db) i
  in  db  { allLenders = Map.insert i (f lender) (allLenders db)
          }
-- add error

removeLender :: LenderId -> Database -> Database
removeLender i db = db { allLenders = Map.delete i (allLenders db) }

{-
newLender :: Database -> (LenLenderatabase)
newLender db =
  let ids = [ i | LenderId i <- map fst (Map.toList $ allLenders db) ]
      newId = LenderId $ if null ids then 0 else (maximum ids + 1)
      newLender = Lender newId "" "" []
  in  ( newLender, db { allLenders = Map.insert newId newLender (allLenders db) } )
  -}
          
data Item = 
  Item { itemId :: ItemId, itemOwner :: LenderId, itemName :: String
      } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.

itemIdNr Item{itemId = ItemId i} = i

updateItem :: ItemId -> (Item -> Item) -> Database -> Database
updateItem i f db = 
  let visit = unsafeLookup (allItems db) i
  in  db  { allItems = Map.insert i (f visit) (allItems db)
          }

removeItem :: ItemId -> Database -> Database
removeItem i db = db { allItems = Map.delete i (allItems db) }

newItem :: LenderId -> Database -> (Item, Database)
newItem uid db =
  let ids = [ i | ItemId i <- map fst (Map.toList $ allItems db) ]
      newId = ItemId $ if null ids then 0 else (maximum ids + 1)
      newItem = Item newId uid "<new>"
  in  ( newItem, db { allItems = Map.insert newId newItem (allItems db) } )


mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { return $ Database 
                (Map.fromList [ (LenderId "martijn", Lender (LenderId "martijn") "Martijn Schrage" "3581 RA" 
                                                      [ItemId 0, ItemId 1, ItemId 2])
                              , (LenderId "jaap", Lender (LenderId "jaap") "Jaap Lageman" "3581 RA" 
                                                      [ItemId 3, ItemId 4])
                              , (LenderId "henny", Lender (LenderId "henny") "Henny Verweij" "3533 GT" 
                                                      [ItemId 5, ItemId 6])
                ])
                (Map.fromList [ (ItemId 0, Item (ItemId 0) (LenderId "martijn") "Oblomov")
                              , (ItemId 1, Item (ItemId 1) (LenderId "martijn") "Grand Theft Auto 4")
                              , (ItemId 2, Item (ItemId 2) (LenderId "martijn") "iPhone 3gs")
                              , (ItemId 3, Item (ItemId 3) (LenderId "jaap") "Boormachine")
                              , (ItemId 4, Item (ItemId 4) (LenderId "jaap") "Spyder calibratie-apparaat")
                              , (ItemId 5, Item (ItemId 5) (LenderId "henny") "Tomtom")
                              , (ItemId 6, Item (ItemId 6) (LenderId "henny") "Boormachine")
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



