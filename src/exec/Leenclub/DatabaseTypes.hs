{-# LANGUAGE TemplateHaskell, TypeOperators, DeriveDataTypeable #-}
module DatabaseTypes where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Category hiding (Category) -- fclabels
import Data.Label                         -- fclabels
import Prelude hiding ((.), id)           -- fclabels

newtype LenderId = LenderId { _lenderIdLogin :: String } deriving (Show, Read, Eq, Ord)

newtype ItemId = ItemId { _itemIdNr :: Int } deriving (Show, Read, Eq, Ord)


-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { _allLenders :: Map LenderId Lender, _allItems :: Map ItemId Item 
                         }
                  deriving (Eq, Show, Read, Typeable)

data Gender = M | F deriving (Eq, Show, Read)

data Lender = 
  Lender { _lenderId :: LenderId, _lenderFirstName :: String, _lenderLastName :: String, _lenderGender :: Gender 
         , _lenderMail :: String
         , _lenderStreet :: String, _lenderStreetNr :: String, _lenderCity :: String, _lenderZipCode :: String
         , _lenderCoords :: (Double, Double) -- http://maps.google.com/maps/geo?q=adres&output=xml for lat/long
         , _lenderImage :: String
         , _lenderRating :: Int, _lenderNrOfPoints :: Int, _lenderItems :: [ItemId]
         } deriving (Eq, Show, Read)


data MovieOrSeries = Movie | Series deriving (Eq, Show, Read)

data Category = Book { _bookAuthor :: String, _bookYear :: Int, _bookLanguage :: String, _bookGenre :: String, _bookPages :: Int, _bookISBN :: String }
              | Game { _gamePlatform :: String, _gameYear :: Int, _gameDeveloper :: String, _gameGenre :: String }
              | CD   { _cdArtist :: String, _cdYear :: Int, _cdGenre :: String }
              | DVD  { _dvdMovieOrSeries :: MovieOrSeries, _dvdDirector :: String, _dvdLanguage :: String, _dvdYear :: Int, _dvdGenre :: String
                     , _dvdRunningTime :: Int, _dvdIMDb :: String, _dvdSeason :: Int, _dvdNrOfEpisodes :: Int }
              | Tool { _toolBrand :: String, _toolType :: String, _toolYear :: Int }
              | Electronics {}
              | Misc {} deriving (Eq, Show, Read)

data Item = 
  Item { _itemId :: ItemId, _itemOwner :: LenderId, _itemPrice :: Int, _itemName :: String, _itemDescr :: String, _itemState :: String
       , _itemImage :: String
       , _itemCategory :: Category
       , _itemBorrowed :: Maybe LenderId
       } deriving (Eq, Show, Read)

-- put id in element? It is also in the map.

mkLabels [ ''Database, ''LenderId, ''Lender, ''ItemId, ''Item, ''Category ]
