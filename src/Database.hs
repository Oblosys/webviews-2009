module Database where

import Data.Map (Map)
import qualified Data.Map as Map 

newtype VisitId = VisitId Int deriving (Show, Eq, Ord)
newtype PigId = PigId Int deriving (Show, Eq, Ord)

data Database = Database { allVisits :: Map VisitId Visit, allPigs :: Map PigId Pig } deriving Show

data Visit = Visit { visitId :: VisitId, zipCode :: String, date :: String, pigs :: [PigId] } deriving Show

data Pig = Pig { pigId :: PigId, symptoms :: [Int], diagnose :: Either Int String } deriving Show

-- put id in element? It is also in the map.

database = Database (Map.fromList [ (VisitId 1, Visit (VisitId 1) "3581" "27-3-2009" [])])
                    (Map.fromList [ (PigId 1, Pig (PigId 1) [0,0,0] (Left 2))
                                  , (PigId 2, Pig (PigId 2) [0,1,1] (Right "Malaria"))
                                  , (PigId 3, Pig (PigId 3) [1,1,1] (Left 3)) ])
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