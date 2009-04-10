module Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

newtype VisitId = VisitId Int deriving (Show, Eq, Ord, Typeable, Data)
newtype PigId = PigId Int deriving (Show, Eq, Ord, Typeable, Data)

data Database = Database { allVisits :: Map VisitId Visit, allPigs :: Map PigId Pig } deriving Show

data Visit = Visit { visitId :: VisitId, zipCode :: String, date :: String, pigs :: [PigId] } deriving Show

data Pig = Pig { pigId :: PigId, pigName :: String, symptoms :: [Int], diagnose :: Either Int String } deriving Show

-- put id in element? It is also in the map.

theDatabase = Database (Map.fromList [ (VisitId 1, Visit (VisitId 1) "3581" "27-3-2009" 
                                                  [ PigId 1, PigId 2, PigId 3 ])])
                    (Map.fromList [ (PigId 1, Pig (PigId 1) "Knir" [0,2,1] (Left 2))
                                  , (PigId 2, Pig (PigId 2) "Knar" [0,1,1] (Right "Malaria"))
                                  , (PigId 3, Pig (PigId 3) "Knor" [1,1,1] (Left 3)) ])
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