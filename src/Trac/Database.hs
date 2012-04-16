{-# OPTIONS -XDeriveDataTypeable #-}
module Trac.Database where

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map 

newtype TicketId = TicketId Int deriving (Show, Read, Eq, Ord, Typeable, Data)

-- must be Typeable and Data, because update functions in views (which must be Data etc.) are Database->Database
data Database = Database { allTickets :: Map TicketId Ticket }
                  deriving (Eq, Show, Read, Typeable,Data)

data Ticket = 
  Ticket { ticketId :: TicketId, content :: String
        } deriving (Eq, Show, Read, Typeable, Data)

mkInitialDatabase :: IO (Database)
mkInitialDatabase =
 do { return $ Database Map.empty
    }

unsafeLookup map key = 
  Map.findWithDefault
    (error $ "element "++ show key ++ " not found in " ++ show map) key map
-- do we want extra params such as pig nrs in sub views?
-- error handling database access
-- Unclear: we need both pig ids and subview ids?
-- make clear why we need an explicit view, and the html rep is not enough.



