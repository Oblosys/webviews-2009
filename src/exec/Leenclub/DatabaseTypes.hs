{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveDataTypeable #-}
module DatabaseTypes where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels

newtype LenderId = LenderId { _lenderIdLogin :: String } deriving (Show, Read, Eq, Ord, Typeable, Data)

newtype ItemId = ItemId { _itemIdNr :: Int } deriving (Show, Read, Eq, Ord, Typeable, Data)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allLenders :: Map LenderId Lender, allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable,Data)

data Gender = M | F deriving (Eq, Show, Read, Typeable,Data)

data Lender = 
  Lender { _lenderId :: LenderId, _lenderFirstName :: String, _lenderLastName :: String, _lenderGender :: Gender 
         , _lenderMail :: String
         , _lenderStreet :: String, _lenderStreetNr :: String, _lenderCity :: String, _lenderZipCode :: String
         , _lenderCoords :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , _lenderImage :: String
         , _lenderRating :: Int, _lenderNrOfPoints :: Int, _lenderItems :: [ItemId]
         } deriving (Eq, Show, Read, Typeable, Data)


data MovieOrSeries = Movie | Series deriving (Eq, Show, Read, Typeable,Data)

data Category = Book { bookAuthor :: String, bookYear :: Int, bookLanguage :: String, bookGenre :: String, bookPages :: Int, bookISBN :: String }
              | Game { gamePlatform :: String, gameYear :: Int, gameDeveloper :: String, gameGenre :: String }
              | CD   { cdArtist :: String, cdYear :: Int, cdGenre :: String }
              | DVD  { dvdMovieOrSeries :: MovieOrSeries, dvdDirector :: String, dvdLanguage :: String, dvdYear :: Int, dvdGenre :: String
                     , dvdRunningTime :: Int, dvdIMDb :: String, dvdSeason :: Int, dvdNrOfEpisodes :: Int }
              | Tool { toolBrand :: String, toolType :: String, toolYear :: Int }
              | Electronics {}
              | Misc {} deriving (Eq, Show, Read, Typeable,Data)

data Item = 
  Item { _itemId :: ItemId, _itemOwner :: LenderId, _itemPrice :: Int, _itemName :: String, _itemDescr :: String, _itemState :: String
       , _itemImage :: String
       , _itemCategory :: Category
       , _itemBorrowed :: Maybe LenderId
       } deriving (Eq, Show, Read, Typeable,Data)

-- put id in element? It is also in the map.

mkLabels [ ''LenderId, ''Lender, ''ItemId, ''Item ]
