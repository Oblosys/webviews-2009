{-# OPTIONS -fglasgow-exts #-}
module Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

users :: Map String (String, String)
users = Map.fromList [("martijn", ("p", "Martijn"))
                     ,("alexey", ("mrchebas", "El Pulpo")) 
                     ,("lambert", ("bla", "Lambert"))
                     ,("doaitse", ("bla", "S. Doaitse Swierstra"))
                     ,("afie",    ("", "Arjan"))
                     ,("", ("", "Anonymous"))
                     ] 
-- TODO: maybe this can be (a special) part of db?


newtype VisitId = VisitId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype PigId = PigId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype CommentId = CommentId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allVisits :: Map VisitId Visit, allPigs :: Map PigId Pig 
                         , allComments :: Map CommentId Comment }
                  deriving (Eq, Show, Read, Typeable,Data)

data Visit = 
  Visit { visitId :: VisitId, zipCode :: String
        , date :: String, pigs :: [PigId]
        } deriving (Eq, Show, Read, Typeable, Data)

updateVisit :: VisitId -> (Visit -> Visit) -> Database -> Database
updateVisit i f db = 
  let visit = unsafeLookup (allVisits db) i
  in  db  { allVisits = Map.insert i (f visit) (allVisits db)
          }
-- add error

removeVisit :: VisitId -> Database -> Database
removeVisit i db = db { allVisits = Map.delete i (allVisits db) }

newVisit :: Database -> (Visit, Database)
newVisit db =
  let ids = [ i | VisitId i <- map fst (Map.toList $ allVisits db) ]
      newId = VisitId $ if null ids then 0 else (maximum ids + 1)
      newVisit = Visit newId "" "" []
  in  ( newVisit, db { allVisits = Map.insert newId newVisit (allVisits db) } )
          
data Pig = 
  Pig { pigId :: PigId, parentVisit :: VisitId, pigName :: String, symptoms :: [Int]
      , diagnose :: Either Int String 
      } deriving (Eq, Show, Read, Typeable,Data)

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

data Comment =
  Comment { commentId :: CommentId, commentAuthor :: String, commentDate :: String
          , commentText :: String
          } deriving (Eq, Show, Read, Typeable,Data)

newComment :: Database -> (Comment, Database)
newComment db =
  let ids = [ i | CommentId i <- map fst (Map.toList $ allComments db) ]
      newId = CommentId $ if null ids then 0 else (maximum ids + 1)
      newComment = Comment newId "" "" ""
  in  ( newComment, db { allComments = Map.insert newId newComment (allComments db) } )

updateComment :: CommentId -> (Comment -> Comment) -> Database -> Database
updateComment i f db = 
  let visit = unsafeLookup (allComments db) i
  in  db  { allComments = Map.insert i (f visit) (allComments db)
          }
      
removeComment :: CommentId -> Database -> Database
removeComment i db = db { allComments = Map.delete i (allComments db) }

theDatabase = Database 
  (Map.fromList [ (VisitId 1, Visit (VisitId 1) "3581" "27-3-2009"
                    [ PigId 1, PigId 2, PigId 3 ])
                , (VisitId 2, Visit (VisitId 2) "3581" "27-3-2009"
                    [ PigId 4, PigId 5])
                ])
  (Map.fromList [ (PigId 1, Pig (PigId 1) (VisitId 1) "Knir" [0,0,0] (Left 2))
                , (PigId 2, Pig (PigId 2) (VisitId 1) "Knar" [0,1,1] (Right "Malaria"))
                , (PigId 3, Pig (PigId 3) (VisitId 1) "Knor" [1,0,1] (Left 3)) 
                , (PigId 4, Pig (PigId 4) (VisitId 2) "Piglet" [1,1,1] (Left 3)) 
                , (PigId 5, Pig (PigId 5) (VisitId 2) "Pinky" [0,1,1] (Left 3)) 
                ])
  (Map.fromList [ (CommentId 1, Comment (CommentId 1) ""
                                        "7 May 2009, 18:30" "First comment")
                ])
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



