{-# LANGUAGE DeriveDataTypeable #-}
module DatabaseTypes where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map

newtype LenderId = LenderId { lenderIdLogin :: String } deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId Int deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allLenders :: Map LenderId Lender, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data Gender = M | F deriving (Eq, Show, Read, Typeable,Data)

data Lender = 
  Lender { lenderId :: LenderId, lenderFirstName :: String, lenderLastName :: String, lenderGender :: Gender 
         , lenderMail :: String
         , lenderStreet :: String, lenderStreetNr :: String, lenderCity :: String, lenderZipCode :: String
         , lenderCoord :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , lenderImage :: String
         , lenderRating :: Int, lenderItems :: [ItemId]
         } deriving (Eq, Show, Read, Typeable, Data)

data MovieOrSeries = Movie | Series deriving (Eq, Show, Read, Typeable,Data)

data Category = Book { bookAuthor :: String, bookYear :: Int, bookLanguage :: String, bookGenre :: String, bookPages :: Int, bookISBN :: String}
              | Game { gamePlatform :: String, gameYear :: Int, gameDeveloper :: String, gameGenre :: String }
              | CD   { cdArtist :: String, cdYear :: Int, cdGenre :: String }
              | DVD  { dvdMovieOrSeries :: MovieOrSeries, dvdDirector :: String, dvdLanguage :: String, dvdYear :: Int, dvdGenre :: String
                     , dvdRunningTime :: Int, dvdIMDb :: String, dvdSeason :: Int, dvdNrOfEpisodes :: Int }
              | Tool { toolBrand :: String, toolType :: String, toolYear :: Int }
              | Electronics {}
              | Misc {} deriving (Eq, Show, Read, Typeable,Data)

data Item = 
  Item { itemId :: ItemId, itemOwner :: LenderId, itemPrice :: Int, itemName :: String, itemDescr :: String, itemState :: String
       , itemImage :: String
       , itemCategory :: Category
       , itemBorrowed :: Maybe LenderId
       } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.
